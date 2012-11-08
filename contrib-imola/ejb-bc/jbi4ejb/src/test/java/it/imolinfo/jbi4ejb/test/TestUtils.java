/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4ejb.test;

import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbExtension;
import it.imolinfo.jbi4ejb.webservice.generator.EJBUtils;
import it.imolinfo.jbi4ejb.webservice.generator.JarUtil;
import it.imolinfo.jbi4ejb.webservice.generator.WSDLGenerator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class TestUtils {
    
    /** Websphere constants */ 
    public static final String WEBSPHERE_6 = "ws6";
    
    /** Glassfish constants */
    public static final String GLASSFISH_v2 = "gl6";
        
    /** Jboss constants */
    public static final String JBOSS_V4 = "jboss4";
        
    /** Glassfish constants */
    public static final String BEA_V8 = "bea8";    
    
    /** Glassfish constants */
    public static final String JACORB = "jacorb";  

    /**
     * Logger.
     */
    private static Log LOG = LogFactory.getLog(TestUtils.class);

    /**
     * Returns the jar filename list to use to compile thx generated sources
     * @param xFireVersion the xfire version
     * @return
     */
    public static List<String> getJarFilesName(String xFireVersion)  {
        String repodir = System.getProperty("localRepository");
        LOG.debug("repodir=" + repodir);        

        List<String> jarFilesName = new ArrayList<String>();

        // adding needed compilation files
        //xfire
        jarFilesName.add(repodir +
                "/xfire/xfire-jsr181-api/1.0-M1/xfire-jsr181-api-1.0-M1.jar");
        jarFilesName.add(repodir + "/org/codehaus/xfire/xfire-jaxb2/" +
                xFireVersion + "/xfire-jaxb2-" + xFireVersion + ".jar");
        jarFilesName.add(repodir + "/org/codehaus/xfire/xfire-java5/" +
                xFireVersion + "/xfire-java5-" + xFireVersion + ".jar");
        jarFilesName.add(repodir + "/org/codehaus/xfire/xfire-core/" +
                xFireVersion + "/xfire-core-" + xFireVersion + ".jar");
        jarFilesName.add(repodir + "/org/codehaus/xfire/xfire-aegis/" +
                xFireVersion + "/xfire-aegis-" + xFireVersion + ".jar");
        jarFilesName.add(repodir + "/org/codehaus/xfire/xfire-annotations/" +
                xFireVersion + "/xfire-annotations-" + xFireVersion + ".jar");

        //.../.m2/repository/commons-lang/commons-lang/2.1
        String apacheCommonsVersion = "2.1";
        jarFilesName.add(repodir + "/commons-lang/commons-lang/" +
                apacheCommonsVersion + "/commons-lang-" + apacheCommonsVersion +
                ".jar");

        //java
        jarFilesName.add(repodir + "/javax/xml/bind/jaxb-api/2.0/jaxb-api-2.0.jar");

        return jarFilesName;
    }
    
    
    public static Properties getOrbProperties(String asType) {
        
        Properties props = new Properties();
        
        if (WEBSPHERE_6.equals(asType)) {
            LOG.debug("Setting Websphere6  ORB properties");
            
            // IBM Websphere 6 properties
            /*
            props.setProperty("org.omg.CORBA.ORBClass", "com.ibm.CORBA.iiop.ORB");
            props.setProperty("org.omg.CORBA.ORBSingletonClass", "com.ibm.rmi.corba.ORBSingleton");
            props.setProperty("javax.rmi.CORBA.StubClass", "com.ibm.rmi.javax.rmi.CORBA.StubDelegateImpl");
            props.setProperty("javax.rmi.CORBA.PortableRemoteObjectClass", "com.ibm.rmi.javax.rmi.PortableRemoteObject");
            props.setProperty("javax.rmi.CORBA.UtilClass", "com.ibm.ws.orb.WSUtilDelegateImpl");
            props.setProperty("com.ibm.ws.orb.transport.ConnectionInterceptorName", "com.ibm.ISecurityLocalObjectBaseL13Impl.SecurityConnectionInterceptor");
            props.setProperty("com.ibm.ws.orb.transport.WSSSLClientSocketFactoryName", "com.ibm.ws.security.orbssl.WSSSLClientSocketFactoryImpl");
            props.setProperty("com.ibm.CORBA.enableLocateRequest", "true");
            props.setProperty("com.ibm.CORBA.ORBCharEncoding", "UTF8");
            props.setProperty("com.ibm.CORBA.TransportMode", "Pluggable");
            */
            
            
        } else if (GLASSFISH_v2.equals(asType)) {
            // add nothing (Default SUN orb values)
            props.setProperty("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
        } else if (JACORB.equals(asType)) {
            props.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
            props.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
            // props.setProperty("ORBInitRef.NameService", "corbaloc::localhost:12354/StandardNS/NameServer-POA/_root");   
            //props.setProperty("org.omg.PortableInterceptor.ORBInitializerClass.TSServerInit", 
            //              "org.jacorb.transaction.TransactionInitializer");            
        } else if (JBOSS_V4.endsWith(asType)) {
            props.setProperty("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
            props.setProperty("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
        }
        return props;
    }    
    
    public static Properties getJndiProperties(String asType) {
        
        Properties props = new Properties();
        
        if (WEBSPHERE_6.equals(asType)) {
            LOG.debug("Setting Websphere6 JNDI properties");
            // IBM Websphere 6 properties
            props.setProperty("java.naming.factory.initial", "com.sun.jndi.cosnaming.CNCtxFactory");
            props.setProperty("java.naming.provider.url", "corbaname:iiop:127.0.0.1:2809/NameServiceServerRoot");

        } else if (GLASSFISH_v2.equals(asType)) {
            // To use that, add appserv-rt.jar and javaee.jar from glassfish to the classpath
            props.setProperty("java.naming.factory.initial","com.sun.enterprise.naming.SerialInitContextFactory");
            props.setProperty("java.naming.factory.url.pkgs","com.sun.enterprise.naming");
            props.setProperty("java.naming.factory.state","com.sun.corba.ee.impl.presentation.rmi.JNDIStateFactoryImpl");
            //props.setProperty("org.omg.CORBA.ORBInitialHost", "localhost");
            // props.setProperty("org.omg.CORBA.ORBInitialPort", "3700");            
        }
        return props;
    }    
    
    /**
     * Gets the classes UIDs from a jar.
     * 
     * @param remoteInterfaceName
     *          The remote interface name
     * @param ejbJarPath
     *          The jar path 
     * @return the classes id from jar
     * 
     * @throws IOException
     *              if some problem occurs
     * @throws EJBWSDLGenerationException
     *              if some problem occurs
     */
    public static Properties getClassesIdFromJar(String remoteInterfaceName, String ejbJarPath) throws IOException, EJBWSDLGenerationException{
        File tempDir = File.createTempFile("EJBCLASSES_", null);        
        tempDir.delete();
        tempDir.mkdir();    
        JarUtil.unjar(new File(ejbJarPath), tempDir);
        Properties classesId = WSDLGenerator.getClassesID(remoteInterfaceName, tempDir);
        tempDir.delete();
        return classesId;
    }
    
    /**
     * Creates a temporary directory (for unit testing pourpose).
     * @return
     */
    public static File createTempDir()  {
        // Creates the working temp dir
        File tempDir = null;
        try {
            tempDir = EJBUtils.createTempDir();
        } catch (IOException e) {
            // TODO i18n
            LOG.error(e.getMessage());
            e.printStackTrace();
        }
        return tempDir;
    }
    
    /**
     * Return the current test-classes directory.
     * @return
     */
    public static File getTestClassesDir() {
        return new File("." + File.separatorChar +"target" +  File.separatorChar + "test-classes");
    }    
    
    
    /**
     * Gets the WSDL string from the WSDL definition.
     * 
     * @param wsdl
     * 
     * @return the WSDL string from definition
     */
    public static String getWSDLStringFromDefinition(Definition wsdl) throws WSDLException {
        // Asserts...
        StringWriter strWriter = new StringWriter();   
        getExtendedWSDLWriter().writeWSDL(wsdl, strWriter);
        return strWriter.toString();                
    }
    
    /**
     * Gets the Jbi4Ejb extended WSDL reader.
     * 
     * @return the extended WSDL reader
     * 
     * @throws WSDLException
     */
    public static WSDLReader getExtendedWSDLReader() throws WSDLException {    
        WSDLFactory factory = WSDLFactory.newInstance();
        WSDLReader reader = factory.newWSDLReader();        
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();            
        Jbi4EjbExtension.register(registry);      
        reader.setExtensionRegistry(registry);
        return reader;
    }
    
    /**
     * Gets the Jbi4Ejb extended WSDL writer.
     * 
     * @return the extended WSDL writer
     * 
     * @throws WSDLException
     */
    public static WSDLWriter getExtendedWSDLWriter() throws WSDLException {    
        WSDLFactory factory = WSDLFactory.newInstance();
        WSDLWriter writer = factory.newWSDLWriter();  
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();            
        Jbi4EjbExtension.register(registry);              
        return writer;
    }
    
    /**
     * recursively removes all the directory
     * @param dir
     * @return
     */
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }    
        // The directory is now empty so delete it
        return dir.delete();
    }
    
    /**
     * Copy directory.
     * S
     * @param sourceLocation
     * @param targetLocation
     * 
     * @throws IOException
     */
    public static void copyDirectory(File sourceLocation , File targetLocation)
    throws IOException {

        if (sourceLocation.isDirectory()) {
            if (!targetLocation.exists()) {
                targetLocation.mkdir();
            }

            String[] children = sourceLocation.list();
            for (int i=0; i<children.length; i++) {
                copyDirectory(new File(sourceLocation, children[i]),
                        new File(targetLocation, children[i]));
            }
        } else {

            InputStream in = new FileInputStream(sourceLocation);
            OutputStream out = new FileOutputStream(targetLocation);

            // Copy the bits from instream to outstream
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            in.close();
            out.close();
        }
    }

}

