package com.sun.jbi.common.classloader;
/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)CustomClassLoaderUtil.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.I18n;

/**
 * Utility used to create a class loader that can load jar files 
 * from the path specified. ex: jar files packaged as part of a service unit, 
 * when the service unit deployed root path is provided. 
 * 
 * The JBI spec requires that the Component be constructed (by the JBI implementation)
 * using the execution class-loader. The component manager will create an instance of 
 * this class in its init() method passing in the execution class-loader. The 
 * execution class-loader will act as the parent of the service class-loader which 
 * delegates first to the parent and then attempts to load the jar's itself.
 * 
 * Each component will have an instance of this utility. 
 * 
 * Internal cache of service class-loaders is maintained by service units deployed 
 * on the component if there are jar files present in the service unit deployed path.
 * which is registered at the <code>ServiceUnitManager.init()</code> method and 
 * unregistered at the <code>ServiceUnitManager.shutdown()</code> method.
 *
 * service unit operations may choose to switch the thread context class-loader of
 * the calling thread by invoking the <code>switchClassLoader()</code> method.
 * 
 * the component will cleanup this instance when its <code>shutdown</code> is called.
 * 
 * @author Philip Varghese
 */
public class CustomClassLoaderUtil {
	private static Logger mLogger = 
	        Logger.getLogger(CustomClassLoaderUtil.class.getName());
	
	/**
	 * Enum indicating the type of the class-loader to be switched.
	 * context_classloader is the thread context class-loader of the calling 
	 * thread.
	 * service_classloader is the class-loader that loads the jars form the
	 * path provided.  
	 */
	public enum SwitchType {context_classloader, service_classloader};
	
	/**
	 * The JBI execution class-loader of the component that created this
	 * instance.
	 */
	private ClassLoader mComponentCL = null;
	
	/**
	 * Internal cache of the service class-loader to the key(ex: service unit name)
	 * that is maintained for each service unit that is deployed to the 
	 * respective component. The Service class-loader is created only 
	 * if there are jar files to load in the unit deployed path.
	 */
	private EntryRegistry<String, ClassLoaderCache> mCache; 

    /**
     * Constructor that accepts the JBI component execution class-loader.
     * @param ctx The JBI component's context.
     * @param componentClassloader JBI component execution class-loader.
     */
    public CustomClassLoaderUtil(ClassLoader componentClassloader) {
		mComponentCL = componentClassloader;
		mCache = new EntryRegistry<String, ClassLoaderCache>();
	}
	
	/**
	 * Creates an instance of service class-loader if there are jar files 
	 * in the unit deployed path. 
	 * The service class-loader will have the JBI component execution 
	 * class-loader as its parent for parent first delegation. 
	 * Should be called for example from <code>ServiceUnitManager.init()</code> to 
	 * initialize the service class-loader.
	 * @param key unit name
	 * @param path unit deployed path
	 */
	public synchronized void registerServiceClassLoader(String key, String path) {
		// find all the jar files in this path.
		URL[] urls = pathToURLs(path);
		if (urls == null || urls.length == 0) {
            // return if there are no jars.
            return;
		}
		
		ServiceClassLoader serviceLoader = newServiceClassLoader(
		        key +"-ServiceClassLoader", urls);
		ClassLoaderCache clCache = new ClassLoaderCache(serviceLoader);
			
		mCache.register(key, clCache);
	}
	
	/**
	 * Removes the cached service class-loader created for this service unit.
	 * Should be called for example from <code>ServiceUnitManager.shutdown()</code>
	 * for the service unit class-loader.
	 * @param key unit name.
	 */
	public synchronized void unregisterServiceClassLoader(String key) {
		if (mCache != null) {
		    ClassLoaderCache suCache = mCache.remove(key);
		    if (suCache != null) {
				suCache.cleanup();
				suCache = null;
			}
		}
	}
	
	/**
	 * Called by runtime threads in JBI components when they want to switch 
	 * their thread context class-loader to the service class-loader that 
	 * was initialized for the service unit that the processing belongs to.
	 * <code>SwitchType.service_classloader</code> switches the thread context
	 * class-loader of the calling thread to the service class-loader. 
	 * <code>SwitchType.context_classloader</code> switches the thread context
	 * class-loader of the calling thread to it original class-loader if it was
	 * present. 
	 * It is expected that the thread that calls this method to switch its thread
	 * context class-loader to the service class-loader will call it back to reset
	 * its thread context classloader to the original one when its finished its task
	 * that required the use of the service class-loader.
	 * 
	 * NOTE: The JBI spec does not mandate that the thread calling 
	 * <code>ServiceUnitManager</code> life-cycle methods have a thread context
	 * class-loader set on the type specified. 
	 * 
	 * @param suName The service unit name.
	 * @param type The type of context switch to make.
	 */
	public synchronized void switchClassLoader(String suName, SwitchType type ) {
		if (mCache == null || !mCache.containsKey(suName)) {
			return; 
		}
		
		ClassLoader toSwitch = null;
		ClassLoaderCache cached = mCache.lookup(suName);
		switch (type) {
			case service_classloader: {
				// store the thread context class-loader to reset later..
				cached.setThreadCtxClassLoader(Thread.currentThread().getContextClassLoader());
				toSwitch = cached.getServiceClassLoader();
				break;
			}
			default: { 
				toSwitch = cached.getThreadCtxClassLoader();
				cached.setThreadCtxClassLoader(null);
			}
		}
		
		Thread.currentThread().setContextClassLoader(toSwitch);
	}
	
	/**
	 * Cleanup of associated structures. Cleans up the service class-loader
	 * cache if present.
	 */
	public synchronized void cleanup() {
		mComponentCL = null;
		if (mCache != null && !mCache.isEmpty()) {
			Set<String> keys = mCache.keySet();
			for (String key : keys) {
				ClassLoaderCache cache = mCache.remove(key);
				if (cache != null) {
					cache.cleanup();
				}
			}
		}
		mCache = null;
	}
	
    /**
     * Utility that looks for jar's recursively in the path provided. 
     * NOTE: beware of recursive path iterations if provided with such 
     * a directory (File) structure. 
     * @param directory
     * @param filter
     * @param recurse
     * @return
     */
    protected Collection<URL> listFiles(File directory, FilenameFilter filter, boolean recurse) {
        // list of files / directories 
        List<URL> urls = new ArrayList<URL>();
        
        // Get files / directories in the directory
        File[] entries = (directory == null) ? null : directory.listFiles();
        if (entries != null) {
            // go over the entries
            for(File entry : entries) {
                // If there is no filter or the filter accepts the 
                // file / directory, add it to the list
                if (filter == null || filter.accept(directory, entry.getName())) {
                    try {
                        URL url = entry.toURI().toURL();
                        urls.add(url);
                    } 
                    catch (MalformedURLException urlExp) {
                        // ignore the iteration, as its not added. 
                    }
                }
                
                // If the file is a directory and the recurse flag is set, recurse into the directory 
                if (recurse && entry.isDirectory()) {
                    urls.addAll(listFiles(entry, filter, recurse));
                }
            }
        }
        
        // Return collection of files
        return urls;
    }

    protected Logger log() {
        return mLogger;
    }

    protected ServiceClassLoader newServiceClassLoader(String name, URL... urls) {
        if (log().isLoggable(Level.FINER)) {
            List<URL> list = new ArrayList<URL>();
            if (urls != null) {
                for (URL u : urls) {
                    list.add(u);
                }
            }
            
            log().finer(I18n.format(
                    "UTIL-2001: Creating new classloader {0} with {1} jars: {2}", 
                    name, String.valueOf(list.size()), String.valueOf(list)));
        }
        
        return new ServiceClassLoader(name, urls, mComponentCL);
    }
    
    protected URL[] pathToURLs(String path) {
        return pathToURLs(path, true);
    }
    
    protected URL[] pathToURLs(String path, boolean recurse) {
        // find all the jar files in this path.
        File rootFile = new File(path);
        FilenameFilter filter = new FilenameFilter() {
            /** @see java.io.FilenameFilter#accept(java.io.File, java.lang.String) */
            public boolean accept(File dir, String name) {
                return (name != null && name.endsWith(".jar"));
            }
        };
        Collection<URL> urlColl = listFiles(rootFile, filter, recurse);
        
        int colSize = urlColl.size();
        if (colSize == 0) {
            // return if there are no jars.
            return null;
        }
        
        URL[] urls = new URL[colSize];
        urlColl.toArray(urls);
        
        return urls;
    }


    /**
     * Cache structure to keep references to the original thread
     * context class-loader if present and the service class-loader
     * for a service unit. Created when the service unit is initialized
     * and removed when the service unit is un-installed. 
     */
    class ClassLoaderCache {
        private ClassLoader mThreadCtxClassLoader;
        private ServiceClassLoader mServiceClassLoader; 
        
        public ClassLoaderCache(ServiceClassLoader serviceClassLoader) {
            mServiceClassLoader = serviceClassLoader;
        }

        public ClassLoader getThreadCtxClassLoader() {
            return mThreadCtxClassLoader;
        }
        
        public ClassLoader getServiceClassLoader() {
            return mServiceClassLoader;
        }
        
        public void cleanup() {
            mThreadCtxClassLoader = null;
            mServiceClassLoader.releaseResources();
            mServiceClassLoader = null;
        }

        public void setThreadCtxClassLoader(ClassLoader componentLoader) {
            mThreadCtxClassLoader = componentLoader;
        }
    }
    
    /**
     * ClassLoader for loading the jar files in the path specified. 
     * Used as the Service class-loader.
     */
    class ServiceClassLoader extends ClassLoader {
        private CustomJarCache mJarCache;
        private String mName;
        
        public ServiceClassLoader(String name, URL[] urls, ClassLoader parent) {
            super(parent);
            mName = name;
            initJarCache(urls);
        }
      
        public Class<?> findClass(String name) throws ClassNotFoundException {
            try {
                return super.findClass(name);
            } 
            catch (ClassNotFoundException cnfe) {
                InputStream is = getResourceAsStream(name.replace('.', '/') + ".class");
                if (is == null) {
                    throw new ClassNotFoundException(name);
                }

                int count;
                byte[] buf = new byte[8192];
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                try {
                    while ((count = is.read(buf, 0, buf.length)) > 0) {
                        bos.write(buf, 0, count);
                    }
                } catch (IOException ioe) {
                    throw new RuntimeException(ioe);
                } finally {
                    try {
                        is.close();
                    } catch (Exception e) { /* Ignore exception */ }
                }
                //logger.info("JarCacheClassLoader: loaded " + name);
                return defineClass(name, bos);
            }
        }
        
        private Class<?> defineClass(String name, ByteArrayOutputStream bos) {
            // Define the package.
            // DEVNOTE VM : Added code to define package for the class to be defined if the package is not
            // already defined. This change was made to fix bug described at 
            // https://open-esb.dev.java.net/issues/show_bug.cgi?id=1130
            // We currently do not get the manifest if it exists for additional package information.
            int i = name.lastIndexOf('.'); 
            if (i != -1) {
                String pkgname = name.substring(0, i);
                // Check if package already loaded.
                Package pkg = getPackage(pkgname);
                if (pkg == null) {
                    definePackage(pkgname, null, null, null, null, null, null, null);
                }
            }
            byte[] classBytes = bos.toByteArray();
            return defineClass(name, classBytes, 0, classBytes.length);
        }

        public InputStream getResourceAsStream(String fileName) {
            InputStream is = super.getResourceAsStream(fileName);
            if (is != null) {
                return is;
            }
            return mJarCache.getResourceAsStream(fileName);
        }

        public URL findResource(String fileName) {
            URL url = mJarCache.findResource(fileName);
            if (url == null) {
                url = super.findResource(fileName);
            }
            return url;
        }

        public Enumeration<URL> findResources(String fileName) throws IOException {
            Enumeration<URL> e = mJarCache.findResources(fileName).elements();
            if (e.hasMoreElements()) {
                return e;
            }
            return super.findResources(fileName);
        }
        
        /** Initialize the JAR cache by adding entries for each jar in this
         *  classloader's path.
         */
        private void initJarCache(URL[] urls) {
            mJarCache = new CustomJarCache();
            
            if (urls != null) {
                // iterate through each URL in the classpath
                for (URL url : urls) {
                    try {
                        // make sure the entry exists and that it's a jar
                        File file = new File(url.getFile());
                        if (file.exists() && file.getName().endsWith(".jar")) {
                            mJarCache.addJar(file.getPath());
                        }
                    }
                    catch (java.io.IOException ioEx) {
                        // ignore if there is an exception in this iteration
                        // as its not added.
                    }
                }
            }
        }
        
        /** Releases all resources currently held in this classloader's cache. */
        public void releaseResources() {
            if (mJarCache != null) {
                mJarCache.close();
            }
            mJarCache = null;
        }

        /** @see java.lang.Object#toString() */
        @Override
        public String toString() {
            return mName +"-ServiceClassLoader";
        }
    }
}
