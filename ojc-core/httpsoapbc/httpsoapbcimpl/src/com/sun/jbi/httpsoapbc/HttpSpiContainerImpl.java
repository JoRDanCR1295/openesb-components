package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;
import com.sun.xml.ws.api.ResourceLoader;
import com.sun.xml.ws.api.server.Container;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

/**
 * SPI root implementation for the HTTP BC. This is a representation of the BC
 * as a container of services.  Runtime components can use {@link Container#getSPI}
 * to request SPIs, and the HttpBcSpiContainer yields objects for the services
 * it recognizes and provides.
 * 
 * @author Noel Ang
 */
public class HttpSpiContainerImpl extends Container {
    private static final Messages mMessages = Messages.getMessages(HttpSpiContainerImpl.class);
    private static final Logger mLog = Messages.getLogger(HttpSpiContainerImpl.class);

    private QName mServiceName;
    private String mServiceUnitID;
    private String mServiceUnitRoot;
    private File mServiceUnitRootPath;
    private ResourceLoader mHttpResourceLoader;
    
    public HttpSpiContainerImpl(HttpSoapEndpoint endpoint) throws Exception {
        super();
        
        mServiceUnitRoot = endpoint.getServiceUnitRootPath();
        mServiceName = endpoint.getServiceName();
        mServiceUnitID = endpoint.getServiceUnitID();
        
        // Translate file path to URL
        try {
            mServiceUnitRootPath = new java.io.File(mServiceUnitRoot);
            mHttpResourceLoader = new ResourceLoaderImpl(mServiceUnitRootPath);
        } catch (Exception e) {
            throw new Exception (
                    mMessages.getString("HTTPBC-E00362.Unable_to_retrieve_su_root",
                                        new Object[] {
                                            endpoint.getServiceUnitID(),
                                            mServiceUnitRoot
                                        })
            );
        }
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpSpiContainerImpl created, service unit name = "
                    + mServiceName.toString()
                    + ", service unit ID = " + mServiceUnitID
                    + ", service unit root path = " + mServiceUnitRootPath.getAbsolutePath());
        }
    }
    
    public <T> T getSPI(Class<T> spiType) {
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "getSPI called with spiType = "
                    + (spiType == null ? "null" : spiType.getName()));
        }
        
        if (spiType == ResourceLoader.class) {
            return spiType.cast(mHttpResourceLoader);
        } else {
            mLog.log(Level.FINE, "Service provider type: " + spiType + " not supported by HTTP BC");
            return null;
        }
    }
    
    private static class ResourceLoaderImpl extends ResourceLoader {
        
        private Set<Locator> mLocators = new HashSet<Locator>();
        private File mRootPath;

        public ResourceLoaderImpl(File rootPath) {
            mRootPath = rootPath;
            mLocators.add(new FileLocatorImpl(mRootPath));
            mLocators.add(new ClassLocatorImpl(mRootPath));
        }
        
        public URL getResource(String resource) throws MalformedURLException {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "Resource requested = " + resource);
            }
            
            // A resource can be resolved in a number of possible ways.
            // The supplied name could point to an actual, physical file,
            // with or without path qualification, and it could point
            // to something virtual, like a class name, foo.bar.baz, which
            // needs to be resolved in an implementation-specific way 
            // (e.g., use package name  for directory traversal before
            // locating the actual class file).
            
            // The code below makes use of a collection of Locators to
            // resolve the resource name.  Each locator is invoked, given
            // the name, and the first locator that succeeds in resolving
            // the name wins.
            URL resourceUrl = null;
            for (Locator locator : mLocators) {
                resourceUrl = locator.locate(resource);
                if (resourceUrl != null) {
                    break;
                }
            }
            
            if (resourceUrl != null) {
                mLog.log(Level.FINE, "resource [" + resource + "] URI: "
                        + resourceUrl.toString()
                        + "(" + resourceUrl.getClass().getName() + ")");
            } else {
                mLog.log(Level.FINE, "resourceUrl not located for resource: "
                        + resource);
            }
            
            return resourceUrl;
        }
        
        private static interface Locator {
            URL locate(Object key);
        }
        
        private static class FileLocatorImpl implements Locator {
            private File mRootPath;
            
            public FileLocatorImpl(File rootPath) {
                mRootPath = rootPath;
            }
            
            public URL locate(Object key) {
                URL resourceUrl = null;
                if (key != null) {
                    String resource = String.valueOf(key);
                    File resourceFile = new File(mRootPath, resource);
                    if (resourceFile.exists()) {
                        try {
                            resourceUrl = resourceFile.toURL(); 
                        } catch (MalformedURLException e) {
                            resourceUrl = null;
                        }
                    }
                }
                return resourceUrl;
            }
        }
        
        private static class ClassLocatorImpl implements Locator {
            private File mRootPath;
            
            public ClassLocatorImpl(File rootPath) {
                mRootPath = rootPath;
            }
            
            public URL locate(Object key) {
                URL resourceUrl = null;
                if (key != null) {
                    String resource = String.valueOf(key);
                    String className = classname(resource);
                    String packageName = packagename(resource);
                    packageName = packageName.replace('.', '/');
                    
                    if (!"".equals(className)) {
                        StringBuffer path = new StringBuffer();
                        if (!"".equals(packageName)) {
                            path.append(packageName).append("/");
                        }
                        path.append(className).append(".class");
                        File resourceFile = new File(mRootPath, path.toString());
                        if (resourceFile.exists()) {
                            try {
                                // return the base path to the class, not
                                // the path to the class *file*, since the
                                // intermediate subdirectories are significant
                                // as part of the class's package name.
                                resourceUrl = mRootPath.toURL();
                            } catch (MalformedURLException e) {
                                resourceUrl = null;
                            }
                        }
                    }
                }
                return resourceUrl;
            }
            
            private String classname(String resource) {
                String classname = resource;
                if (classname.indexOf('.') != -1) {
                    classname = classname.substring(classname.lastIndexOf('.') + 1);
                }
                return classname;
            }
            
            private String packagename(String resource) {
                String packagename = "";
                if (resource.indexOf('.') != -1) {
                    packagename = resource.substring(0, resource.lastIndexOf('.'));
                }
                return packagename;
            }
        }
    }
}
