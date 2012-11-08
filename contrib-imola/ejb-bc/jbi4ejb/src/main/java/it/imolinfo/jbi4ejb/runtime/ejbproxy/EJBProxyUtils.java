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
import it.imolinfo.jbi4ejb.configuration.InterfaceExtractorUtil;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.exception.EJBDeployException;
import it.imolinfo.jbi4ejb.exception.EJBInvokeException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbSOAPExtensionsUtils;
import it.imolinfo.jbi4ejb.webservice.generator.EJBUtils;
import it.imolinfo.jbi4ejb.webservice.generator.Util;
import it.imolinfo.jbi4ejb.webservice.generator.WSDLGenerator;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.rmi.RMISecurityManager;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.rmi.PortableRemoteObject;

import org.codehaus.xfire.gen.Wsdl11Generator;
import org.omg.CORBA.Any;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Request;
import org.omg.CORBA.TypeCode;

/**
 * EJBProxy helper methods.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class EJBProxyUtils {

    /** The logger. */
    private static final Logger LOG
    = LoggerFactory.getLogger(EJBProxyUtils.class);  
    private static final Messages MESSAGES
    = Messages.getMessages(EJBProxyUtils.class);
    
    /**
     * Instantiates a new EJB proxy utils.
     */
    private EJBProxyUtils() {}
                  
    /**
     * Creates the EJB classes.
     * 
     * @param classesId
     *            The <code>Hashtable</code> with the classes uids
     * @param jarFilesName
     *            the jar list to compile the sources The jar list to compile
     *            the generated sources
     * @param wsdlFile
     *            the wsdl File
     * @param tempDir
     *            the temporary directory to use
     * @param remoteInterfaceName
     *            the remote interface class name if null, retrives-it using the portTypeName
     * @param portTypeName
     *            the poert-tpye name used to get the remote interface name (if not specified). 
     * @return the classes dir absolute path
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static EJBClasses createEJBClasses(File wsdlFile, 
            Properties classesId, File tempDir, List<String> jarFilesName, String remoteInterfaceName, String portTypeName) throws EJBDeployException {
               
        if (!wsdlFile.exists()) {
        	String msg=MESSAGES.getString("EJB000901_No_WSDL_exists", new Object[]{wsdlFile});
            LOG.error(msg);
            throw new EJBDeployException(msg);   

        }
        
        // Adds the SOAP extensions
        // Creates a new file containing the SOAP extensions (see EJB -17).
        File tempWSDLFile = Jbi4EjbSOAPExtensionsUtils.addSoapElements(wsdlFile);
              
        
        // From the WSDL, creates the classes
        File sourceDir = new File(tempDir, "src");
        sourceDir.mkdir();
        
        LOG.debug("Generating classes from WSDL: " + tempWSDLFile.getAbsolutePath() + " in directory:" +  sourceDir.getAbsolutePath());
        
        Wsdl11Generator gen = new Wsdl11Generator();        
        gen.setWsdl(tempWSDLFile.getAbsolutePath());        
        gen.setBinding(Wsdl11Generator.JAXB);
        gen.setOverwrite(true);
        gen.setOutputDirectory(sourceDir.getAbsolutePath());        
        try {            
            gen.setExplicitAnnotation(true);
            gen.generate();
        } catch (Exception e) {
        	String msg=MESSAGES.getString("EJB000902_Exception_generating_class_from_WSDL", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);   

        } 
        // deletes the temporary file
        tempWSDLFile.delete();

        // Compiles the generated source
        File classesDir = new File(tempDir, "classes");        
        List<String> sources =  Util.findJavaSources(sourceDir.getAbsolutePath(), null);
        try {
            Util.compileJavaClasses(sourceDir.getAbsolutePath(), classesDir.getAbsolutePath(), sources, jarFilesName, null);
        } catch (ClassGenerationException ex) {
        	String msg=MESSAGES.getString("EJB000903_Exception_compiling_generated_source", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new EJBDeployException(msg,ex);
        }
       
        String remoteInterfaceClassName  = remoteInterfaceName;
        // Gets the remote interface class name (from the WSDL PortType), if not defined
        if (remoteInterfaceClassName == null) {
         remoteInterfaceClassName
            = EJBProxyUtils.findRemoteInterfaceCompleteClassName(classesDir.getAbsolutePath(), portTypeName);
        }

        // Adds the SUIDs to the classes
        Enumeration classesToSerialize = classesId.keys();
        while (classesToSerialize.hasMoreElements()) {
            LOG.debug("Adding uid in class: " + classesToSerialize);
            String classToSer = (String)classesToSerialize.nextElement();
            Object serNumber = classesId.get(classToSer);
            Long uid = null;
            if (serNumber instanceof String) {
                uid = new Long((String)serNumber);
            } else {
                uid = (Long)serNumber;
            }            

            String classFileName 
            = classesDir.getAbsolutePath() + File.separatorChar + classToSer.replace('.', File.separatorChar) + ".class";

            try {
                LOG.debug("Insert uid: " + uid + "in class: " + classFileName);
                Util.tweakSerializableDecoration(classFileName, uid);
            } catch (ClassGenerationException e) {
            	String msg=MESSAGES.getString("EJB000904_Exception_adding_SUIDs_to_classes", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new EJBDeployException(msg,e);
            }            
        }      
        
        // Adds correct the throws clause of the remote interface (adding the application exception).
        try {
            EJBUtils.tweakRemoteInterfaceGeneratedFromWSDL(remoteInterfaceClassName, classesDir.getAbsolutePath());
        } catch (ClassGenerationException e) {
        	String msg=MESSAGES.getString("EJB000905_Exception_adding_the_application_exception", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }          
        
        // Adds javax.rmi.Remote to the interface and the throws RemoteException clause
        try {
            Util.tweakInterfaceClasses(remoteInterfaceClassName, classesDir.getAbsolutePath());
        } catch (ClassGenerationException e) {
        	String msg=MESSAGES.getString("EJB000906_Exception_adding_javaxRmiRemote_to_interface", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
         
        
        // Generates the stub for the remote interface
        EJBUtils.createStub(classesDir.getAbsolutePath(), remoteInterfaceClassName, jarFilesName);    
        
        return new EJBClasses(classesDir.getAbsolutePath(), remoteInterfaceClassName);
    }   
    
    /**
     * Creates the EJB classes, creating an external temp dir (for testing
     * pourpouse).
     * 
     * @param wsdlPath
     *            The complete path to the wsdl
     * @param classesId
     *            The <code>Hashtable</code> with the classes uids
     * @param jarFilesName
     *            the jar list to compile the sources The jar list to compile
     *            the generated sources
     * @param portTypeName
     *            The port type name (to identify the remote interface)
     * @param remoteInterfaceClassName
     *            The remote interface class name
     * @return the classes dir absolute path
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
        public static String createEJBClasses(String wsdlPath, String remoteInterfaceClassName, String portTypeName, 
                Properties classesId, List<String> jarFilesName) throws EJBDeployException {
            // Creates the working temp dir
            File tempDir;
            try {
                tempDir = EJBUtils.createTempDir();
            } catch (IOException e) {
            	String msg=MESSAGES.getString("EJB000907_Exception_creating_working_temp_dir", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new EJBDeployException(msg,e);
            }
            
            // Tests if the WSDL exists
            File wsdlFile = new File(wsdlPath);

            if (!wsdlFile.exists()) {
            	String msg=MESSAGES.getString("EJB000901_No_WSDL_exists", new Object[]{wsdlFile});
                LOG.error(msg);
                throw new EJBDeployException(msg);  
            }                                  
                          
            EJBClasses ejbClasses =  createEJBClasses(wsdlFile, classesId, tempDir, jarFilesName, remoteInterfaceClassName, portTypeName);
            
            return ejbClasses.getEjbClassesPath();    
        }    
        

    /**
     * Creates the stateless EJB and the stub.
     * 
     * @param corbaname
     *            The corbaname to locate the <code>EJBHome</code>
     * @param remoteInterfaceName
     *            The remote interface class name
     * @param ejbClassLoader
     *            The classloader to use
     * @param orb
     *            The ORB
     * @return the object The stateless EJB remote stub
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Object createStatelessEJBFromCorbaName(final String corbaname, final String remoteInterfaceName,
                final ClassLoader ejbClassLoader, ORB orb)
    throws EJBDeployException {

        // Saves the previous classloader
        ClassLoader previousClassLoader = Thread.currentThread().getContextClassLoader(); 
        Thread.currentThread().setContextClassLoader(ejbClassLoader);                    
                             
        org.omg.CORBA.Object home = orb.string_to_object(corbaname);
        
        if (home == null) {
            String msg=MESSAGES.getString("EJB000908_Null_object_retrieved_with_corbaname", new Object[]{corbaname});
            LOG.error(msg);
            throw new EJBDeployException(msg); 
        }
                       
        // Retrieve the EJB reference
        Object remoteObjectBean = getEJBFromCorbaHomeObject(home, orb, remoteInterfaceName, ejbClassLoader);

        // Sets back the prevoius classloader
        Thread.currentThread().setContextClassLoader(previousClassLoader);

        return remoteObjectBean;
    }
    
    /**
     * Creates the stateless EJB, using JNDI to lookup the home object.
     * 
     * @param remoteInterfaceName
     *            The remote interface class name
     * @param jndiName
     *            The Jndi name
     * @param jndiParams
     *            The Jndi params
     * @param ejbClassLoader
     *            The ejb class loader
     * @return the object The stateless EJB remote stub
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Object createStatelessEJBFromJNDI(final String jndiName, Properties jndiParams, final String remoteInterfaceName, ClassLoader ejbClassLoader)
    throws EJBDeployException {
        
        // Saves the previuos classloader
        ClassLoader previousClassLoader = Thread.currentThread().getContextClassLoader(); 
        Thread.currentThread().setContextClassLoader(ejbClassLoader);

        InitialContext jndiContext = null;
        try {
            if (jndiParams == null) {            
                jndiContext = new InitialContext();            
            } else {            
                jndiContext = new InitialContext(jndiParams);            
            }
        } catch (NamingException e) {
        	String msg=MESSAGES.getString("EJB000909_createStatelessEJBFromJNDI", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        
        LOG.debug("jndiContext retrieved:" + jndiContext);

        org.omg.CORBA.portable.ObjectImpl home = null;
        try {
            home = (org.omg.CORBA.portable.ObjectImpl)jndiContext.lookup(jndiName);
        } catch (NamingException e) {
        	String msg=MESSAGES.getString("EJB000909_createStatelessEJBFromJNDI", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
       
        LOG.info("EJB000910_Home_object_found", new Object[]{home});

        // Gets the ORB from the org.omg.CORBA.portable.ObjectImpl object
        ORB orb = home._orb();               

        LOG.info("EJB000911_ORB_found", new Object[]{orb});
        // Retrieve the EJB reference
        Object remoteObjectBean = getEJBFromCorbaHomeObject(home, orb, remoteInterfaceName, ejbClassLoader);        

        // Sets back the prevoius classloader
        Thread.currentThread().setContextClassLoader(previousClassLoader);
        
        return remoteObjectBean;
    }    
    
    /**
     * Creates the stateless EJB, using JNDI to lookup the home object.
     * 
     * @param remoteInterfaceName
     *            The remote interface class name
     * @param jndiName
     *            The jndi name
     * @param jndiParams
     *            The jndi params
     * @param ejbClassLoader
     *            The classloader to use
     * @return the object The stateless EJB remote stub
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static org.omg.CORBA.portable.ObjectImpl createStatelessHomeFromJNDI(final String jndiName, Properties jndiParams, final String remoteInterfaceName, ClassLoader ejbClassLoader)
    throws EJBDeployException {
        


        InitialContext jndiContext = null;
        try {
            if (jndiParams == null) {            
                jndiContext = new InitialContext();            
            } else {            
                jndiContext = new InitialContext(jndiParams);            
            }
        } catch (NamingException e) {
        	String msg=MESSAGES.getString("EJB000912_createStatelessHomeFromJNDI", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        
        LOG.debug("jndiContext retrieved:" + jndiContext);

        org.omg.CORBA.portable.ObjectImpl home = null;
        try {
            home = (org.omg.CORBA.portable.ObjectImpl)jndiContext.lookup(jndiName);
        } catch (NamingException e) {
        	String msg=MESSAGES.getString("EJB000912_createStatelessHomeFromJNDI", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        
        LOG.info("EJB000910_Home_object_found", new Object[]{home});
        
        return home;
    }     
    
    
    /**
     * Gets the EJB from corba home object.
     * 
     * @param home
     *            The CORBA home remote object
     * @param orb
     *            The ORB
     * @param remoteInterfaceName
     *            The remote interface name
     * @param ejbClassLoader
     *            The classloader to use to get the remomte interface class
     * 
     * @return the EJB from corba home object
     * 
     * @throws EJBDeployException
     *              if some problem occurs 
     */
    @SuppressWarnings("unchecked")
    public static Object getEJBFromCorbaHomeObject(org.omg.CORBA.Object home, ORB orb, 
                String remoteInterfaceName, ClassLoader ejbClassLoader) throws EJBDeployException {
        // call create method and obtain a pointer to real stateless bean        
        Any result = orb.create_any();
        
        NamedValue resultVal = orb.create_named_value("result", result, org.omg.CORBA.ARG_OUT.value);
        Request createRequest = home._create_request(null, "create", orb.create_list(0), resultVal);

        String interfaceName = EJBProxyUtils.getInterfaceName(remoteInterfaceName);
        String interfaceId = EJBProxyUtils.getInterfaceId(remoteInterfaceName);
        TypeCode remoteInterfaceType = orb.create_interface_tc(interfaceId, interfaceName);
        
        createRequest.set_return_type(remoteInterfaceType);
        createRequest.invoke();
        result = createRequest.return_value();
        org.omg.CORBA.Object reference=result.extract_Object();

        Class remoteClass = null;
        try {
            remoteClass = ejbClassLoader.loadClass(remoteInterfaceName);
        } catch (ClassNotFoundException e) {
        	String msg=MESSAGES.getString("EJB000913_getEJBFromCorbaHomeObject", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        LOG.debug("Remote class: " + remoteClass);

        // Narrow
        Object remoteObjectBean = PortableRemoteObject.narrow(reference, remoteClass);        
        return remoteObjectBean;         
    }

    /**
     * Creates the Class interface (to narrow in the dynamic case). NO STUB IS
     * CREATED
     * 
     * @param wsdlPath
     *            The complete path to the wsdl
     * @param jarFilesName
     *            the jar list to compile the sources The jar list to compile
     *            the generated sources
     * @param remoteInterfaceClassName
     *            the remote interface class name
     * 
     * @return the string
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Class getInterfaceClass(String remoteInterfaceClassName, String wsdlPath, List<String> jarFilesName) throws EJBDeployException {

        // Creates the working temp dir
        File tempDir;
        try {
            tempDir = EJBUtils.createTempDir();
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }

        // Tests if the WSDL exists
        File wsdlFile = new File(wsdlPath);

        if (!wsdlFile.exists()) {
        	String msg=MESSAGES.getString("EJB000901_No_WSDL_exists", new Object[]{wsdlFile});
            LOG.error(msg);
            throw new EJBDeployException(msg); 
        }

        // From the WSDL, creates the classes
        File sourceDir = new File(tempDir, "src");
        sourceDir.mkdir();
        Wsdl11Generator gen = new Wsdl11Generator();        
        gen.setWsdl(wsdlFile.getAbsolutePath());

        gen.setOutputDirectory(sourceDir.getAbsolutePath());        
        try {
            gen.generate();
        } catch (Exception e) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }                        

        // Compiles the generated source
        File classesDir = new File(tempDir, "classes");        

        List<String> sources =  Util.findJavaSources(sourceDir.getAbsolutePath(), null);

        try {
            Util.compileJavaClasses(sourceDir.getAbsolutePath(), classesDir.getAbsolutePath(), sources, jarFilesName, null);
        } catch (ClassGenerationException ex) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new EJBDeployException(msg,ex);
        }

        // Adds javax.rmi.Remote to the interface and generates the stubs
        try {
            Util.tweakInterfaceClasses(remoteInterfaceClassName, classesDir.getAbsolutePath());
        } catch (ClassGenerationException e) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }               

        ClassLoader ejbClassesClassLoader = null;
        try {
            ejbClassesClassLoader = Util.getURLClassLoader(classesDir.getAbsolutePath());
        } catch (MalformedURLException e) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        Class myRemoteClass = null;
        try {
            myRemoteClass = ejbClassesClassLoader.loadClass(remoteInterfaceClassName);
        } catch (ClassNotFoundException e) {
        	String msg=MESSAGES.getString("EJB000914_getInterfaceClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBDeployException(msg,e);
        }
        return myRemoteClass;
    }    

    /**
     * Creates the stateless EJB and try to get a reference using the
     * <code>RMIClassLoader</code> and trying to dinamically load the stub and
     * the paramete/return classes.
     * 
     * @param corbaname
     *            The corbaname to locate the <code>EJBHome</code>
     * @param remoteInterfaceName
     *            The remote interface class name
     * @param myRemoteInterfaceClass
     *            The remote interface class
     * @param orb
     *              The ORB
     * @return the object The stateless EJB remote stub
     * 
     * @throws EJBDeployException
     *             If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Object createStatelessEJBUsingRMIClassLoader(final String corbaname, final String remoteInterfaceName, Class myRemoteInterfaceClass,  ORB orb)
    throws EJBDeployException {

        // Sets the RMISecurityManager
        // SecurityManager previousSystemSecurityManager = System.getSecurityManager();
        if (System.getProperty("java.security.policy") != null) {
            LOG.debug("Using security policy: " + System.getProperty("java.security.policy"));    
        }        
        RMISecurityManager rmiSecurityManager = new RMISecurityManager();        
        System.setSecurityManager(rmiSecurityManager);

        org.omg.CORBA.Object ejbHome = orb.string_to_object(corbaname);

        LOG.debug("ejbHome: " + ejbHome);
        LOG.debug("ejbHome.class: " + ejbHome.getClass());               

        // call create method and obtain a pointer to real stateless bean
        Any result = orb.create_any();
        LOG.debug("result: "+result);
        NamedValue resultVal = orb.create_named_value("result", result, org.omg.CORBA.ARG_OUT.value);
        Request createRequest = ejbHome._create_request(null, "create", orb.create_list(0), resultVal);

        String interfaceName = EJBProxyUtils.getInterfaceName(remoteInterfaceName);
        String interfaceId = EJBProxyUtils.getInterfaceId(remoteInterfaceName);
        TypeCode remoteInterfaceType = orb.create_interface_tc(interfaceId, interfaceName);
        createRequest.set_return_type(remoteInterfaceType);
        createRequest.invoke();
        result = createRequest.return_value();
        org.omg.CORBA.Object reference=result.extract_Object();        
        LOG.debug("reference.class: " + reference.getClass());              
        LOG.debug("GetCodebase: " + javax.rmi.CORBA.Util.getCodebase(ejbHome.getClass()));

        // The stub now should be dynamically downloaded...
        Object remoteObjectBean = PortableRemoteObject.narrow(reference, myRemoteInterfaceClass);        
        LOG.debug("remoteObjectBean found: " + remoteObjectBean);      

        // sets the prervious security manager
        // System.setSecurityManager(previousSystemSecurityManager);

        return remoteObjectBean;
    }    


    /**
     * Gets the interface id.
     * 
     * @param completeClassName
     *            The class name
     * 
     * @return the interface id
     */
    public static String getInterfaceId(String completeClassName) {
        String id = "RMI:" + completeClassName + ":0000000000000000";
        return id;
    }

    /**
     * Gets the interface name.
     * 
     * @param completeClassName
     *            The class name
     * 
     * @return the interface name
     */
    public static String getInterfaceName(String completeClassName) {
        int lastDot = completeClassName.lastIndexOf(".");
        String className = completeClassName.substring(lastDot + 1, completeClassName.length());
        return className;
    }
    
    /**
     * Invoke method on the object (the proxied EJB).
     * 
     * @param remoteBean
     *          The ejb remoteobject
     * @param method
     *          The method to call
     * @param params
     *          The method params
     * @param ejbClassLoader
     *          The classloader to use
     * @param orb
     *          The orb
     * @return the object
     * 
     * @throws EJBInvokeException
     *          if some problem in the EJB invocation occurs
     * @throws InvocationTargetException 
     *              If an exception is thrown in the invokation
     * @throws IllegalAccessException 
     *              If there are access problems
     */
    public static Object invokeMethod(Object remoteBean, Method method, Object[] params, ClassLoader ejbClassLoader, ORB orb) 
    throws EJBInvokeException, IllegalAccessException, InvocationTargetException {
        // org.omg.CosTransactions.Current ts_current = null;        
        
        LOG.info("Invoking using ORB class: " + orb.getClass().getName());
                
        // OTSManager.setORB(myORB);
        // OTSManager.setPOA(myOA);         
        // Current current = OTSManager.get_current();                              
        //        try {            
        //            org.omg.CORBA.Object test =  orb.resolve_initial_references("TransactionFactory");
        //            org.omg.CORBA.Object test =  orb.string_to_object("TransactionFactory");
        //            System.err.println("*************");
        //            System.err.println(test);            
        //            System.err.println("*************");
        //            ts_current = 
        //                org.omg.CosTransactions.CurrentHelper.narrow(test);
        //        }
        //        catch (Exception e) {
        //            e.printStackTrace();
        //          return null;
        //        }

        // Begin transaction
        //        try {
        //            // ts_current.begin();                                              
        //            current.begin();   
        //            System.err.println("Transaction Name: " + current.get_transaction_name());            
        //        }
        //        catch (Exception e) {
        //          e.printStackTrace();
        //        }        


        if (remoteBean == null) {
            String msg = "The bean is null";
            LOG.debug(msg);
            throw new EJBInvokeException(msg);
        }

        // Saves the previuos classloader
        ClassLoader previousClassLoader = Thread.currentThread().getContextClassLoader(); 
        Thread.currentThread().setContextClassLoader(ejbClassLoader);
                
        Object returnObj = method.invoke(remoteBean, params);     
        
        // Sets back the previous classloader
        Thread.currentThread().setContextClassLoader(previousClassLoader);        
        return returnObj;                       
    }

    /**
     * Invoke method on the EJB.
     * 
     * @param methodName
     *            The method to invoke
     * @param params
     *            The params for the method invocation
     * @param remoteBean
     *            The remote bean object
     * @param ejbClassLoader
     *            The classloader to use
     * @param orb
     *            The orb
     * 
     * @return the object
     * 
     * @throws EJBInvokeException
     *             If some problems occurs in method invocation
     * @throws InvocationTargetException 
     *              If the call throws an exception
     * @throws IllegalAccessException 
     *              Problem in accesing object
     */    
    public static Object invokeMethod(Object remoteBean, String methodName, Object[] params, ClassLoader ejbClassLoader, ORB orb) throws EJBInvokeException, IllegalAccessException, InvocationTargetException {
                
        Method method = getMethodFromName(remoteBean, methodName, params);        
        
        return invokeMethod(remoteBean, method, params, ejbClassLoader, orb);
    }          

//    /**
//     *  Return a string representation of the given status code.
//     */
//    private static String toString(int status) {
//      switch (status) {
//        case javax.transaction.Status.STATUS_PREPARING:
//          return "STATUS_PREPARING";
//        case javax.transaction.Status.STATUS_PREPARED:
//          return "STATUS_PREPARED";
//        case javax.transaction.Status.STATUS_ROLLING_BACK:
//          return "STATUS_ROLLING_BACK";
//        case javax.transaction.Status.STATUS_ROLLEDBACK:
//          return "STATUS_ROLLEDBACK";
//        case javax.transaction.Status.STATUS_COMMITTING:
//          return "STATUS_COMMITING";
//        case javax.transaction.Status.STATUS_COMMITTED:
//          return "STATUS_COMMITED";
//        case javax.transaction.Status.STATUS_NO_TRANSACTION:
//          return "STATUS_NO_TRANSACTION";
//        case javax.transaction.Status.STATUS_UNKNOWN:
//          return "STATUS_UNKNOWN";
//        case javax.transaction.Status.STATUS_MARKED_ROLLBACK:
//          return "STATUS_MARKED_ROLLBACK";
//        case javax.transaction.Status.STATUS_ACTIVE:
//          return "STATUS_ACTIVE";
//        default:
//          return "STATUS_UNKNOWN(" + status + ")";
//      }
//    }

    
 /**
 * Gets the <code>Method</code> from the name.
 * 
 * @param remoteBean
 *          The object that defines the method
 * @param methodName
 *          The method name
 * @param params
 *          The method params
 * @return the method from name
 * 
 * @throws EJBInvokeException
 *          If some problem occurs in getting the method name
 */
@SuppressWarnings("unchecked")
public static Method getMethodFromName(Object remoteBean, String methodName, Object[] params) 
        throws EJBInvokeException {
        Class[] parameterTypes = new Class[params.length];
        for (int i = 0; i < params.length; i++) {
            parameterTypes[i] = params[i].getClass();
        }
                
        Method method = null;                
        try {
            Class objClass = remoteBean.getClass();
            method = objClass.getMethod(methodName, parameterTypes);            
        } catch (NoSuchMethodException e) {
        	String msg=MESSAGES.getString("EJB000915_getMethodFromName", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new EJBInvokeException(e);
        }
        return method;
    }
    
    
    /**
     * Retrieve the remote interface class name from the class name (without
     * package). The class name must be unique for the whole package.
     * 
     * @param sourcesDir
     *          Directory where to look for the class name
     * @param className
     *          The class name
     * @return
     *          The class name (without the package)
     * 
     * @throws EJBDeployException
     *          If some proble occurs in classes search
     */
    public static String findRemoteInterfaceCompleteClassName(String sourcesDir, String className) throws EJBDeployException {
        LOG.debug("Looking for class: " + className + " in directory: " + sourcesDir);
        List<File> files = Util.findFilesFromSourceDirectory(sourcesDir, className + ".class");
        
        if (files.size() != 1) {
        	String msg=MESSAGES.getString("EJB000916_Retrieve_remote_interface_class_name", new Object[]{files.size()}, 
        			new Object[]{className}, new Object[]{sourcesDir});
            LOG.error(msg);
            throw new EJBDeployException(msg);
        } else {
            LOG.debug("Found class: " + files.get(0).getAbsolutePath());
            LOG.debug("The sources directory is: " + sourcesDir);
        }
        File myClassFile = files.get(0);
        String myClassFileAbsolutePath = myClassFile.getAbsolutePath();                                 
        String relativeFileNamePath = myClassFileAbsolutePath.substring(sourcesDir.length() + 1, myClassFileAbsolutePath.length());       
        relativeFileNamePath = relativeFileNamePath.replace(File.separatorChar, '.');
        // Removes the ".class"
        relativeFileNamePath = relativeFileNamePath.substring(0, relativeFileNamePath.length() - ".class".length());
        LOG.debug("The remote interface class name is: " + relativeFileNamePath);
        return relativeFileNamePath;        
    }

}
