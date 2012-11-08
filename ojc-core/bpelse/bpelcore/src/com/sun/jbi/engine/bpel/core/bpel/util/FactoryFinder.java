/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)$Id: FactoryFinder.java,v 1.6 2008/04/29 23:07:31 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.logging.Level;
import java.util.logging.Logger;

public class FactoryFinder {

	private static Logger LOGGER = Logger.getLogger(FactoryFinder.class.getName());

	private static SecuritySupport ss = new SecuritySupport();

	/*
	 * Try to find provider using Jar Service Provider Mechanism
	 * 
	 * @return instance of provider class if found or null
	 */
	public static Object[] findJarServiceProvider(String factoryId)
			throws ConfigurationError {

		String serviceId = "META-INF/services/" + factoryId;
        Enumeration<URL> urls = null;
        ArrayList objArray = new ArrayList();

        // First try the Context ClassLoader
        ClassLoader cl = ss.getContextClassLoader();
        if (cl != null) {
            try {
                urls = cl.getResources(serviceId);
            } catch (IOException e) {

            }

            // If no provider found then try the current ClassLoader
            if (urls == null || ! (urls.hasMoreElements())) {
                cl = FactoryFinder.class.getClassLoader();
                try {
                    urls = cl.getResources(serviceId);
                } catch (IOException e) {

                }
            }
        } else {
            // No Context ClassLoader, try the current
            // ClassLoader
            cl = FactoryFinder.class.getClassLoader();
            try {
                urls = cl.getResources(serviceId);
            } catch (IOException e) {

            }
        }

        if (urls == null) {
            // No provider found
            return null;
        }

        if (LOGGER.isLoggable(Level.FINER)) {
        	LOGGER.log(Level.FINER, I18n.loc("BPCOR-3064: Found service : {0}", serviceId));
        }

        // Read the service provider name in UTF-8 as specified in
        // the jar spec. Unfortunately this fails in Microsoft
        // VJ++, which does not implement the UTF-8
        // encoding. Theoretically, we should simply let it fail in
        // that case, since the JVM is obviously broken if it
        // doesn't support such a basic standard. But since there
        // are still some users attempting to use VJ++ for
        // development, we have dropped in a fallback which makes a
        // second attempt using the platform's default encoding. In
        // VJ++ this is apparently ASCII, which is a subset of
        // UTF-8... and since the strings we'll be reading here are
        // also primarily limited to the 7-bit ASCII range (at
        // least, in English versions), this should work well
        // enough to keep us on the air until we're ready to
        // officially decommit from VJ++. [Edited comment from
        // jkesselm]
        while (urls.hasMoreElements()) {
            URL url = urls.nextElement();
            BufferedReader rd;
            InputStream is = null;
            try {
                is = url.openStream();
                rd = new BufferedReader(new InputStreamReader(is, "UTF-8"));
            } catch (java.io.UnsupportedEncodingException e) {
                rd = new BufferedReader(new InputStreamReader(is));
            } catch (java.io.IOException e) {
                continue;
            }
            String factoryClassName = null;
            try {
                while ((factoryClassName = rd.readLine()) != null) {
                    // XXX Does not handle all possible input as specified by
                    // the
                    // Jar Service Provider specification
                    if (factoryClassName != null
                            && !"".equals(factoryClassName)) {
                    	if (LOGGER.isLoggable(Level.FINER)) {
                        	LOGGER.log(Level.FINER, I18n.loc("BPCOR-3078: Found service provider: {0}", 
                        			factoryClassName));
                    	}
                        Object newInstance = newInstance(factoryClassName, cl,
                                false);
                        objArray.add(newInstance);
                    }
                }
                rd.close();
            } catch (IOException x) {
                // No provider found
                continue;
            }
        }
        return objArray.toArray();
	}

	/**
	 * Create an instance of a class using the specified ClassLoader and
	 * optionally fall back to the current ClassLoader if not found.
	 * 
	 * @param className
	 *            Name of the concrete class corresponding to the service
	 *            provider
	 * 
	 * @param cl
	 *            ClassLoader to use to load the class, null means to use the
	 *            bootstrap ClassLoader
	 * 
	 * @param doFallback
	 *            true if the current ClassLoader should be tried as a fallback
	 *            if the class is not found using cl
	 */
	private static Object newInstance(String className, ClassLoader cl,
			boolean doFallback) throws ConfigurationError {
		// assert(className != null);

		try {
			Class providerClass;
			if (cl == null) {
				// If classloader is null Use the bootstrap ClassLoader.
				// Thus Class.forName(String) will use the current
				// ClassLoader which will be the bootstrap ClassLoader.
				providerClass = Class.forName(className);
			} else {
				try {
					providerClass = cl.loadClass(className);
				} catch (ClassNotFoundException x) {
					if (doFallback) {
						// Fall back to current classloader
						cl = FactoryFinder.class.getClassLoader();
						providerClass = Class.forName(className, true, cl);
					} else {
						throw x;
					}
				}
			}

			Object instance = providerClass.newInstance();

			if (LOGGER.isLoggable(Level.FINER)) {
				LOGGER.log(Level.FINER, I18n.loc("BPCOR-3065: Created new instance of {0}", providerClass));
			}
			return instance;
		} catch (ClassNotFoundException x) {
			throw new ConfigurationError(
					"Provider " + className + " not found", x);
		} catch (Exception x) {
			throw new ConfigurationError("Provider " + className
					+ " could not be instantiated: " + x, x);
		}
	}

	static class ConfigurationError extends Error {
		private Exception exception;

		/**
		 * Construct a new instance with the specified detail string and
		 * exception.
		 */
		ConfigurationError(String msg, Exception x) {
			super(msg);
			this.exception = x;
		}

		Exception getException() {
			return exception;
		}
	}

}
