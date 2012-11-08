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
 * @(#)JXPathContextFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Properties;

/**
 * Defines a factory API that enables applications to obtain a
 * JXPathContext instance.  To acquire a JXPathContext, first call the
 * static <code>newInstance()</code> method of JXPathContextFactory.
 * This method returns a concrete JXPathContextFactory.
 * Then call <code>newContext()</code> on that instance.  You will rarely
 * need to perform these steps explicitly: usually you can call one of the
 * <code>JXPathContex.newContext</code> methods, which will perform these steps
 * for you.
 *
 * @see JXPathContext#newContext(Object)
 * @see JXPathContext#newContext(JXPathContext,Object)
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class JXPathContextFactory {

    /** The default property */
    public static final String FACTORY_NAME_PROPERTY =
        "org.apache.commons.jxpath.JXPathContextFactory";

    /** The default factory class */
    private static final String DEFAULT_FACTORY_CLASS =
        "org.apache.commons.jxpath.ri.JXPathContextFactoryReferenceImpl";

    /** Avoid reading all the files when the findFactory
        method is called the second time ( cache the result of
        finding the default impl )
    */
    private static String factoryImplName = null;
    
    protected JXPathContextFactory () {

    }

    /**
     * Obtain a new instance of a <code>JXPathContextFactory</code>.
     * This static method creates a new factory instance.
     * This method uses the following ordered lookup procedure to determine
     * the <code>JXPathContextFactory</code> implementation class to load:
     * <ul>
     * <li>
     * Use  the <code>org.apache.commons.jxpath.JXPathContextFactory</code>
     * system property.
     * </li>
     * <li>
     * Alternatively, use the JAVA_HOME (the parent directory where jdk is
     * installed)/lib/jxpath.properties for a property file that contains the
     * name of the implementation class keyed on
     * <code>org.apache.commons.jxpath.JXPathContextFactory</code>.
     * </li>
     * <li>
     * Use the Services API (as detailed in the JAR specification), if
     * available, to determine the classname. The Services API will look
     * for a classname in the file
     * <code>META- INF/services/<i>org.apache.commons.jxpath.
     * JXPathContextFactory</i></code> in jars available to the runtime.
     * </li>
     * <li>
     * Platform default <code>JXPathContextFactory</code> instance.
     * </li>
     * </ul>
     *
     * Once an application has obtained a reference to a
     * <code>JXPathContextFactory</code> it can use the factory to
     * obtain JXPathContext instances.
     *
     * @exception JXPathFactoryConfigurationError if the implementation is not
     * available or cannot be instantiated.
     */
    public static JXPathContextFactory newInstance() {
        if (factoryImplName == null) {
            factoryImplName =
                findFactory(FACTORY_NAME_PROPERTY, DEFAULT_FACTORY_CLASS);
        }

        JXPathContextFactory factoryImpl;
        try {
            Class clazz = Class.forName(factoryImplName);
            factoryImpl = (JXPathContextFactory) clazz.newInstance();
        }
        catch (ClassNotFoundException cnfe) {
            throw new JXPathContextFactoryConfigurationError(cnfe);
        }
        catch (IllegalAccessException iae) {
            throw new JXPathContextFactoryConfigurationError(iae);
        }
        catch (InstantiationException ie) {
            throw new JXPathContextFactoryConfigurationError(ie);
        }
        return factoryImpl;
    }

    /**
     * Creates a new instance of a JXPathContext using the
     * currently configured parameters.
     *
     * @exception JXPathContextFactoryConfigurationError if a JXPathContext
     * cannot be created which satisfies the configuration requested
     */

    public abstract JXPathContext newContext(
        JXPathContext parentContext,
        Object contextBean)
        throws JXPathContextFactoryConfigurationError;

    // -------------------- private methods --------------------
    // This code is duplicated in all factories.
    // Keep it in sync or move it to a common place
    // Because it's small probably it's easier to keep it here

    /** Temp debug code - this will be removed after we test everything
     */
    private static boolean debug = false;
    static {
        try {
            debug = System.getProperty("jxpath.debug") != null;
        }
        catch (SecurityException se) {
            // This is ok
        }
    }

    /** Private implementation method - will find the implementation
        class in the specified order.
        @param property    Property name
        @param defaultFactory Default implementation, if nothing else is found
        
        @return class name of the JXPathContextFactory
    */
    private static String findFactory(String property, String defaultFactory) {
        // Use the factory ID system property first
        try {
            String systemProp = System.getProperty(property);
            if (systemProp != null) {
                if (debug) {
                    System.err.println(
                        "JXPath: found system property" + systemProp);
                }
                return systemProp;
            }

        }
        catch (SecurityException se) {
            // Ignore
       }

        // try to read from $java.home/lib/xml.properties
        try {
            String javah = System.getProperty("java.home");
            String configFile =
                javah
                    + File.separator
                    + "lib"
                    + File.separator
                    + "jxpath.properties";
            File f = new File(configFile);
            if (f.exists()) {
                Properties props = new Properties();
                props.load(new FileInputStream(f));
                String factory = props.getProperty(property);
                if (factory != null) {
                    if (debug) {
                        System.err.println(
                            "JXPath: found java.home property " + factory);
                    }
                    return factory;
                }
            }
        }
        catch (Exception ex) {
            if (debug) {
                ex.printStackTrace();
            }
        }

        String serviceId = "META-INF/services/" + property;
        // try to find services in CLASSPATH
        try {
            ClassLoader cl = JXPathContextFactory.class.getClassLoader();
            InputStream is = null;
            if (cl == null) {
                is = ClassLoader.getSystemResourceAsStream(serviceId);
            }
            else {
                is = cl.getResourceAsStream(serviceId);
            }

            if (is != null) {
                if (debug) {
                    System.err.println("JXPath: found  " + serviceId);
                }
                BufferedReader rd =
                    new BufferedReader(new InputStreamReader(is));

                String factory = rd.readLine();
                rd.close();

                if (factory != null && !"".equals(factory)) {
                    if (debug) {
                        System.err.println(
                            "JXPath: loaded from services: " + factory);
                    }
                    return factory;
                }
            }
        }
        catch (Exception ex) {
            if (debug) {
                ex.printStackTrace();
            }
        }

        return defaultFactory;
    }
}
