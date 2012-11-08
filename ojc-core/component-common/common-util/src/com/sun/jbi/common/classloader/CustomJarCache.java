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
 * @(#)CustomJarCache.java
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Vector;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/** Implements a cache of JarFiles used by Classloader implementations to 
 *  load classes and resources.  The advantage that this class provides over
 *  traditional URLClassLoader behavior is that open JarFile references can be
 *  closed on demand when the classloader is no longer needed.
 */
public class CustomJarCache {

	Map                 mOpenJarFiles = new HashMap();
	Map                 mResourceIndex = Collections.synchronizedMap(new HashMap());
	URLStreamHandler    mUrlStreamHandler = new JarStreamHandlerImpl();

	/** Overrides close() method to prevent external clients from closing the
	 *  JarFile.  The closeImpl() method should be called instead.
	 */
	public static class JarFileImpl extends JarFile {
		public JarFileImpl(String name)
		throws IOException {
			super(name);
		}

		public void close() {
			// don't close: in case somebody does this:
			// JarURLConnection c =
			// (JarURLConnection)getResource("jar:file:blah.jar").openConnection();
			// c.getJarFile().close();
		}

		public void closeImpl()
			throws IOException {
			super.close();
		}

		protected void finalize() 
			throws IOException {
			closeImpl();
		}
	}

	/** Implementation of JarURLConnection which serves content out of the cache.
	 */
	public class JarURLConnectionImpl extends JarURLConnection {
		JarFile mJarFile;

		public JarURLConnectionImpl(URL url) 
			throws MalformedURLException {
			super(url);
		}

		public JarFile getJarFile() 
			throws IOException  {
			if (mJarFile == null) {
				URL jarFileURL = getJarFileURL();
				synchronized (mOpenJarFiles) {
					mJarFile = (JarFile) mOpenJarFiles.get(jarFileURL.getPath());
					if (mJarFile == null)  {
						mJarFile = new JarFileImpl(jarFileURL.getPath());
						mOpenJarFiles.put(jarFileURL.getPath(), mJarFile);
					}
				}
			}

			return mJarFile;
		}

		public InputStream getInputStream()
			throws IOException  {
			return getJarFile().getInputStream(getJarFile().getEntry(getEntryName()));
		}

		public void connect() 
			throws IOException  {
			// jarFileURLConnection = jarFileURL.openConnection();
		}

	}

	class JarStreamHandlerImpl extends URLStreamHandler {
		protected URLConnection openConnection(URL url) 
			throws IOException  {
			return new JarURLConnectionImpl(url);
		}
	}

	public URLStreamHandler getURLStreamHandler() {
		return mUrlStreamHandler;
	}

	public URL createURL(URL jarFileURL, String entry)
		throws MalformedURLException  {
		URL url = new URL("jar", null, -1, jarFileURL.toString() + "!/" + 
				(entry == null ? "" : entry), getURLStreamHandler());
		return url;
	}

	/** Closes all JarFiles stored in the cache, which should make them
	 *  eligible for filesystem-level deletion.
	 */
	public void close()  {
		synchronized (mOpenJarFiles) {
			Iterator iter = mOpenJarFiles.values().iterator();
			while (iter.hasNext())  {
				JarFileImpl jarFile = (JarFileImpl) iter.next();
				try  {
					jarFile.closeImpl();
				}
				catch (IOException exc) {
					Logger.getLogger(this.getClass().getPackage().getName()).
					log(Level.FINE, 
							"I/O error while closing " + jarFile.getName(), exc);
				}
			}

			mResourceIndex.clear();
			mResourceIndex = null;
			mOpenJarFiles.clear();
			mOpenJarFiles = null;
			mUrlStreamHandler = null;
		}
	}

	public InputStream getResourceAsStream(String fileName) {
		fileName = fileName.replace('\\', '/');

		if (fileName.startsWith("/"))  {
			fileName = fileName.substring(1);
		}
		else if (fileName.startsWith("./")) {
			fileName = fileName.substring(2);
		}

		Object key = mResourceIndex.get(fileName);
		synchronized (mOpenJarFiles)  {
			if (key != null) {
				JarFile jarFile = (JarFile) mOpenJarFiles.get(key);
				if (jarFile != null) {
					try  {
						JarEntry entry = jarFile.getJarEntry(fileName);
						return jarFile.getInputStream(entry);
					} 
					catch (IOException exc) {
						throw new RuntimeException(exc);
					}
				}
			}

			Iterator iter = mOpenJarFiles.entrySet().iterator();
			while (iter.hasNext()) {
				Map.Entry e = (Map.Entry) iter.next();
				JarFile jarFile = (JarFile) e.getValue();
				JarEntry entry = jarFile.getJarEntry(fileName);
				if (entry != null)  {
					try  {
						mResourceIndex.put(fileName, e.getKey());
						return jarFile.getInputStream(entry);
					}
					catch (IOException exc) {
						throw new RuntimeException(exc);
					}
				}
			}
		}
		return null;
	}

	public URL findResource(String fileName) {
		fileName = fileName.replace('\\', '/');
		if (fileName.startsWith("/")) {
			fileName = fileName.substring(1);
		}
		else if (fileName.startsWith("./")) {
			fileName = fileName.substring(2);
		}

		Object key = mResourceIndex.get(fileName);
		synchronized (mOpenJarFiles) {
			if (key != null) {
				JarFile jarFile = (JarFile) mOpenJarFiles.get(key);
				if (jarFile != null) {
					try {
						return createURL(new File(jarFile.getName()).toURL(), fileName);
					}
					catch (MalformedURLException exc) {
						throw new RuntimeException(exc);
					}
				}
			}
			Iterator iter = mOpenJarFiles.entrySet().iterator();
			while (iter.hasNext()) {
				Map.Entry e = (Map.Entry) iter.next();
				JarFile jarFile = (JarFile) e.getValue();
				JarEntry entry = jarFile.getJarEntry(fileName);
				if (entry != null) {
					try {
						mResourceIndex.put(fileName, e.getKey());
						return createURL(new File(jarFile.getName()).toURL(), fileName);
					}
					catch (MalformedURLException exc) {
						throw new RuntimeException(exc);
					}
				}
			}
		}
		return null;
	}

	public Vector<URL> findResources(String fileName) {
		fileName = fileName.replace('\\', '/');
		if (fileName.startsWith("/")) {
			fileName = fileName.substring(1);
		}
		else if (fileName.startsWith("./")) {
			fileName = fileName.substring(2);
		}

		Vector result = new Vector();
		synchronized (mOpenJarFiles) {
			Iterator iter = mOpenJarFiles.values().iterator();
			while (iter.hasNext()) {
				JarFile jarFile = (JarFile) iter.next();
				JarEntry entry = jarFile.getJarEntry(fileName);
				if (entry != null) {
					try {
						result.add(createURL(new File(jarFile.getName()).toURL(), fileName));
					}
					catch (MalformedURLException exc) {
						throw new RuntimeException(exc);
					}
				}
			}
		}
		return result;
	}

	public void addJar(String fileName) 
		throws IOException {
		try {
			addJar(new JarFileImpl(fileName));
		}
		catch (IOException e) {
			throw new RuntimeException("jar file: " + fileName, e);
		}
	}

	public void addJar(JarFile file)
		throws IOException {
		if ( null == file ) {
			String arg = "File is : " + file;
			throw new java.lang.IllegalArgumentException(arg);
			/*StringTranslator translator = (StringTranslator)
            EnvironmentContext.getInstance().getStringTranslatorFor(this);
            throw new java.lang.IllegalArgumentException(translator.getString(
               LocalStringKeys.NULL_ARGUMENT, "file")); */
		} 

		String name = file.getName();
		File f = new File(name);
		URL url = f.toURL();
		synchronized (mOpenJarFiles) {
			JarFileImpl prev = (JarFileImpl) mOpenJarFiles.get(url.getPath());
			if (prev != null) {
				prev.closeImpl();
			}
			mOpenJarFiles.put(url.getPath(), (JarFileImpl) file);
		}
	}

	public URL[] getURLs() {
		List list = new LinkedList();
		synchronized (mOpenJarFiles) {
			Iterator iter = mOpenJarFiles.values().iterator();
			while (iter.hasNext()) {
				String fileName = ((JarFile) iter.next()).getName();
				try {
					list.add(createURL(new File(fileName).toURL(), null));
				}
				catch (MalformedURLException exc) {
					throw new RuntimeException(exc);
				}
			}
		}

		URL[] urls = new URL[list.size()];
		list.toArray(urls);
		return urls;
	}
}
