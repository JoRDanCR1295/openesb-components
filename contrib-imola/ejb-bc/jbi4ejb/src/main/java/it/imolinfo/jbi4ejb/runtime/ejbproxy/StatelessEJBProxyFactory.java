/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.runtime.ejbproxy;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4ejb.exception.EJBDeployException;
import it.imolinfo.jbi4ejb.exception.EJBInvokeException;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.webservice.generator.Util;

import java.io.File;
import java.net.MalformedURLException;
import java.util.List;
import java.util.Properties;

import org.omg.CORBA.ORB;

/**
 * A factory for creating StatelessEJBProxy objects.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class StatelessEJBProxyFactory {

    /** The Constant LOG. */
    private static final Logger LOG
    = LoggerFactory.getLogger(StatelessEJBProxyFactory.class);
    private static final Messages MESSAGES
    = Messages.getMessages(StatelessEJBProxyFactory.class);
    
    /**
     * Instantiates a new stateless EJB proxy factory.
     */
    private StatelessEJBProxyFactory() {}
    
    /**
     * Creates the <code>StatelessEJBProxy</code> from the service
     * description.
     * 
     * @param wsdl
     *            the wsdl <source>File</source>
     * @param serviceDescriptor
     *            the service descriotor
     * @param tempDir
     *            the temp dir where to create the EJB proxy files
     * @param jarFilesName
     *            the jars list
     * 
     * @return the <code>StatelessEJBProxy</code>
     * 
     * @throws EJBDeployException if some problem occurs in the ejb proxy creation
     */
    @SuppressWarnings("unchecked")
    public static StatelessEJBProxy createEJBProxy(File wsdl, ProviderServiceDescriptor serviceDescriptor, File tempDir, List<String> jarFilesName) throws EJBDeployException {       

        // Getst the portTypeName from the service descriptor
        String portTypeName = serviceDescriptor.getPortTypeName().getLocalPart();
        
        // Creates the EJB classes        
        EJBClasses ejbClasses = EJBProxyUtils.createEJBClasses(wsdl, serviceDescriptor.getSerialVersionUID(), tempDir, jarFilesName, null, portTypeName);
        
        // Creates the URL ClassLoader, based on the generated classes        
        ClassLoader ejbInvokeClassLoader;
        try {
            ejbInvokeClassLoader = Util.getURLClassLoader(ejbClasses.getEjbClassesPath());
        } catch (MalformedURLException e) {
        	String msg=MESSAGES.getString("EJB000917_Exception_creating_URL_ClassLoder", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }

        // Init the ORB
        ORB orb = ORB.init(new String[]{}, serviceDescriptor.getOrbProperties());

        // Uses DII to create the remote bean reference
        Object remoteBean = EJBProxyUtils.createStatelessEJBFromCorbaName(serviceDescriptor.getName(), ejbClasses.getRemoteInterfaceClassName(), ejbInvokeClassLoader, orb);

        // Creates the EJB proxy
        StatelessEJBProxy ejbProxy = new StatelessEJBProxy(ejbClasses.getRemoteInterfaceClassName(), remoteBean, ejbInvokeClassLoader, orb);

        // Try to load the EJB remote interface and set it
        try {
            Class myRemoteInterface = ejbInvokeClassLoader.loadClass(ejbClasses.getRemoteInterfaceClassName());
            ejbProxy.setRemoteInterfaceClass(myRemoteInterface);
            
        } catch (ClassNotFoundException e) {
        	String msg=MESSAGES.getString("EJB000918_Exception_getting_remote_interface_class", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }

        return ejbProxy;
    }    

    /**
     * Gets the EJB from corbaname.
     * 
     * @param wsdlPath
     *          The path to the WSDL file
     * @param remoteInterfaceClassName
     *          The remote interface class name
     * @param corbaName
     *          The corba name
     * @param classesId
     *          The classes UIDs
     * @param jarFilesName
     *          The jar list to use to compile the classes
     * @param orbParams
     *          The orb parameters
     * @return the EJB from corbaname
     * 
     * @throws EJBWSDLGenerationException
     *          if some proxy ejb generation  occurs
     */
    @SuppressWarnings("unchecked")
    public static StatelessEJBProxy getEJBFromCorbaname(String wsdlPath, String remoteInterfaceClassName, String corbaName
            , Properties classesId, List<String> jarFilesName, Properties orbParams) throws EJBWSDLGenerationException {
        return getEJBFromCorbaname(wsdlPath, remoteInterfaceClassName, corbaName, classesId, jarFilesName, orbParams, false);
    }

    /**
     * Gets the EJB from corbaname using RMI classloader.
     * 
     * @param wsdlPath
     *      The path to the WSDL file
     * @param remoteInterfaceClassName
     *      The remote interface class name
     * @param corbaName
     *       The corba name
     * @param classesId
     *        The classes UIDs
     * @param jarFilesName
     *      The jar list to use to compile the classes
     * @param orbParams
     *       The orb parameters
     * 
     * @return the EJB from corbaname using RMI classloader
     * 
     * @throws EJBWSDLGenerationException
     *          if some proxy ejb generation  occurs
     */
    @SuppressWarnings("unchecked")
    public static StatelessEJBProxy getEJBFromCorbanameUsingRMIClassloader(String wsdlPath, String remoteInterfaceClassName, String corbaName
            , Properties classesId, List<String> jarFilesName, Properties orbParams) throws EJBWSDLGenerationException {
        return getEJBFromCorbaname(wsdlPath, remoteInterfaceClassName, corbaName, classesId, jarFilesName, orbParams, true);
    }
    
    /**
     * Instantiates a new dynamic EJB proxy.
     * 
     * @param wsdlPath
     *            The complete WSDL path
     * @param remoteInterfaceClassName
     *            The remote interface name
     * @param classesId
     *            The <code>Hashtable</code> containing the clasess UIDS
     * @param jarFilesName
     *            The jar list to use to compile the generated classes
     * @param jndiName
     *            The jndi name
     * @param jndiParams
     *            The jndi properties
     * @param orbProperties
     *            The ORB properties
     * 
     * @return the EJB proxy
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static StatelessEJBProxy getEJBFromJNDIName(String wsdlPath, String remoteInterfaceClassName, String jndiName, Properties jndiParams,
            Properties orbProperties, Properties classesId, List<String> jarFilesName) throws EJBWSDLGenerationException {         

        ClassLoader ejbInvokeClassLoader = null;
        Object remoteBean = null;
        try {               

            // Creates the EJB Classes
            String classesDir = EJBProxyUtils.createEJBClasses(wsdlPath, remoteInterfaceClassName, null, classesId, jarFilesName);                       

            LOG.debug("The classes are in the directory:" + classesDir);

            // Creates the URL class Loader, based on the generated classes
            ejbInvokeClassLoader = Util.getURLClassLoader(classesDir);

            
            // Saves the previuos classloader
            ClassLoader previousClassLoader = Thread.currentThread().getContextClassLoader(); 
            Thread.currentThread().setContextClassLoader(ejbInvokeClassLoader);
            
            // Uses DII to create the remote bean reference
            org.omg.CORBA.portable.ObjectImpl remoteHome = EJBProxyUtils.createStatelessHomeFromJNDI(jndiName, jndiParams, remoteInterfaceClassName, ejbInvokeClassLoader);
            
            // Gets the ORB from the org.omg.CORBA.portable.ObjectImpl object
            ORB orb = remoteHome._orb();               

            LOG.info("EJB000911_ORB_found", new Object[]{orb});    
            // Retrieve the EJB reference
            remoteBean = EJBProxyUtils.getEJBFromCorbaHomeObject(remoteHome, orb, remoteInterfaceClassName, ejbInvokeClassLoader);        

            // Sets back the prevoius classloader
            Thread.currentThread().setContextClassLoader(previousClassLoader);
                        
            StatelessEJBProxy ejbProxy = new StatelessEJBProxy(remoteInterfaceClassName, remoteBean, ejbInvokeClassLoader, orb);

            return ejbProxy;

        } catch (Exception ex) {
            String msg=MESSAGES.getString("EJB000919_getEJBFromJNDIName", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new EJBWSDLGenerationException(msg,ex);
        }
    }        


/**
 * Instantiates a new dynamic EJB client.
 * 
 * @param wsdlPath
 *            The complete WSDL path
 * @param remoteInterfaceClassName
 *            The remote interface name
 * @param corbaName
 *            The corba name to use
 * @param classesId
 *            The <code>Hashtable</code> containing the clasess UIDS
 * @param jarFilesName
 *            The jar list to use to compile the generated classes
 * @param orbParams
 *            The ORB params
 * @param dynamicClassLoading
 *            If true, try to dinamically load stubs and classes using the
 *            RMIClassLoader
 * 
 * @return
 *      The EJB proxy
 * 
 * @throws EJBWSDLGenerationException
 *             If some problem occurs
 */
@SuppressWarnings("unchecked")
public static StatelessEJBProxy getEJBFromCorbaname(String wsdlPath, String remoteInterfaceClassName, String corbaName
        , Properties classesId, List<String> jarFilesName, Properties orbParams, boolean dynamicClassLoading) throws EJBWSDLGenerationException {

    ClassLoader ejbInvokeClassLoader = null;
    Object remoteBean = null;
    try {            
        
        ORB orb = null;
        if (orbParams != null) {
            orb = ORB.init(new String[]{}, orbParams);
        } else {
            orb = ORB.init(new String[]{}, new Properties());
        }        

        if (!dynamicClassLoading) {                                           

            // Creates the EJB Classes
            String classesDir = EJBProxyUtils.createEJBClasses(wsdlPath, remoteInterfaceClassName, null, classesId, jarFilesName);

            LOG.debug("The ejb client classes are in the directory:" + classesDir);

            // Creates the URL class Loader, based on the generated classes
            ejbInvokeClassLoader = Util.getURLClassLoader(classesDir);

            // Uses DII to create the remote bean reference
            remoteBean = EJBProxyUtils.createStatelessEJBFromCorbaName(corbaName, remoteInterfaceClassName, ejbInvokeClassLoader, orb);            
        } else {

            // Dynamic stub loading
            LOG.debug("Dynamic invocation, classes loaded using RMI");         

            Class myRemoteInterfaceClass = EJBProxyUtils.getInterfaceClass(remoteInterfaceClassName, wsdlPath, jarFilesName);

            LOG.debug("Loaded remote interface: " + myRemoteInterfaceClass);

            remoteBean = EJBProxyUtils.createStatelessEJBUsingRMIClassLoader(corbaName, remoteInterfaceClassName, myRemoteInterfaceClass, orb);
        }
        StatelessEJBProxy ejbProxy = new StatelessEJBProxy(remoteInterfaceClassName, remoteBean, ejbInvokeClassLoader, orb);

        return ejbProxy;

    } catch (Exception ex) {
        // TODO i18n
        //LOG.error(ex.getMessage());
        //throw new EJBWSDLGenerationException(ex);
        String msg=MESSAGES.getString("EJB000920_getEJBFromCorbaname", new Object[]{ex.getMessage()});
        LOG.error(msg,ex);
        throw new EJBWSDLGenerationException(msg,ex);
    }
}


}
