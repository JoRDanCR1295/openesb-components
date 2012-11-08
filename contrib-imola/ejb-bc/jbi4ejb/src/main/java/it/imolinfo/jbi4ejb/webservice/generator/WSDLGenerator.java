/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.configuration.InterfaceExtractorUtil;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbAddress;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbBinding;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbExtension;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbTypes;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectStreamClass;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Fault;
import javax.wsdl.Input;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import org.codehaus.xfire.aegis.AegisBindingProvider;
import org.codehaus.xfire.aegis.type.Configuration;
import org.codehaus.xfire.aegis.type.DefaultTypeMappingRegistry;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.service.binding.ObjectServiceFactory;
import org.codehaus.xfire.soap.SoapConstants;
import org.codehaus.xfire.util.ServiceUtils;
import org.codehaus.xfire.wsdl.AbstractWSDL;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;

/**
 * Class to generate WSDL file with Jbi4Ejb extensions.
 * <p>
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class WSDLGenerator {
    
    /** Logger. */
    private static final Logger LOG = LoggerFactory.getLogger(EJBUtils.class);

    /**
     * Creates an instance of this class.
     */
    private WSDLGenerator() {}
    

    /**
     * Creates the wsdl interface, getting the classes from the jar.
     * 
     * @param className
     *          The interface class name
     * @param ejbJarPath
     *          The ejb jar path
     * @param wsdlFileName
     *          The WSDL filename (to create)
     * @param descriptor
     *          The WSDL descriptor
     * @param tempDir
     *          The temporary directory to use
     * 
     * @return the WSDL file
     * 
     * @throws EJBWSDLGenerationException
     *              If some problem occurs
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    public static File createWsdlFromJar(String className, String ejbJarPath,
                String wsdlFileName, WSDLDescriptor descriptor, File tempDir)            
            throws EJBWSDLGenerationException {

        // Unzip the jar in the temp directory
        try {
            JarUtil.unjar(new File(ejbJarPath), tempDir);
        } catch (IOException e) {
            // TODO i18n
            LOG.error(e.getMessage());
            throw new EJBWSDLGenerationException(e);
        }
        
        // Creates the WSDL
        return createWsdlFromClassesDirectory(className, tempDir, wsdlFileName, descriptor);
    }
    
    /**
     * Creates the wsdl interface, getting the classes from a Ear.
     * 
     * @param className
     *            The interface class name
     * @param earJarPath
     *            The ear jar path
     * @param wsdlFileName
     *            The WSDL filename (to create)
     * @param descriptor
     *            The WSDL descriptor
     * 
     * @return the WSDL file
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    public static File createWsdlFromEar(String className, String earJarPath,
                String wsdlFileName, WSDLDescriptor descriptor)            
            throws EJBWSDLGenerationException {

        // Extract the classes form the ear in a temporary directory
        String tempDirPath = InterfaceExtractorUtil.extractEarClassesInTempDirectory(earJarPath);
       
        // Creates the WSDL
        return createWsdlFromClassesDirectory(className, new File(tempDirPath), wsdlFileName, descriptor);
    }        
    

    /**
     * Creates the wsdl interface, getting the classes from a directory.
     * The classes are copied to the temporary directory to omake all the needed bytecode transformations.
     * 
     * @param className
     *           The interface class name
     * @param classesDirectory
     *           The directory where the classes are
     * @param wsdlFileName
     *          The WSDL filename (to create)
     * @param descriptor
     *          The WSDL descriptor
     * @param tempDir
     *          The temporary directory to use
     * 
     * @return the WSDL file
     * 
     * @throws EJBWSDLGenerationException
     *              If some problem occurs
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    public static File createWsdlFromClassesDirectory(String className, File classesDirectory, 
            String wsdlFileName, WSDLDescriptor descriptor, File tempDir)
            throws EJBWSDLGenerationException {

        // From the remote interface, creates the WSDL
        try {
            // Copy all the classes in the temporary directory
            EJBUtils.copyDirectory(classesDirectory, tempDir);
            
            // Creates the WSDL
            return createWsdlFromClassesDirectory(className, tempDir, wsdlFileName, descriptor);
                  
        } catch (IOException ioex) {
            // TODO i18n
            LOG.error(ioex.getMessage());
            throw new EJBWSDLGenerationException(ioex);
        }
    }    
    

    /**
     * Creates the wsdl from classes directory.
     * 
     * @param className
     *           The interface class name
     * @param classesDirectory
     *           The directory where the classes are
     * @param wsdlFileName
     * @param wsdlFileName
     *          The WSDL filename (to create)
     * @param descriptor
     *          The WSDL descriptor
     *          
     * @return the WSDL file
     *          
     * @throws EJBWSDLGenerationException
     *              If some problem occurs
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    private  static File createWsdlFromClassesDirectory(String className, File classesDirectory,
            String wsdlFileName, WSDLDescriptor descriptor)
            throws EJBWSDLGenerationException {

        // From the remote interface, creates the WSDL
        try {

            // Gets the SUIDs
            Properties classesIDs = getClassesID(className, classesDirectory);                        
            
            // Removes the java.rmi.Remote/javax.ejb.EJBObject interface extension and the java.rmi.RemoteException
            EJBUtils.removeEJBRemoteInterface(className, classesDirectory.getAbsolutePath());
                                                        
            // Get the classes classloader, setting the parent to null        
            ClassLoader classLoader = null;
            try {
                classLoader = Util.getURLClassLoader(classesDirectory.getAbsolutePath(), null);
            } catch (MalformedURLException ex) {
                // TODO i18n
                String msg = ex.getMessage();
                LOG.error(msg);
                throw new ClassGenerationException(msg);
            }
                        
            LOG.debug("Class name: " + className);
            
            // Loads the class using the URL classloader
            Class remoteInterfaceClass = classLoader.loadClass(className);
                                                                  
            return createWsdlFromClass(remoteInterfaceClass, wsdlFileName, descriptor, classesIDs);            

        } catch (ClassNotFoundException cex) {
            // TODO i18n
            LOG.error(cex.getMessage());
            throw new EJBWSDLGenerationException(cex);
        } catch (ClassGenerationException cex) {
            // TODO i18n
            LOG.error(cex.getMessage());
            throw new EJBWSDLGenerationException(cex);
        }
    }        
        
    
    /**
     * Creates the wsdl from class name in the tempDir.
     * 
     * @param wsdlFileName
     *            The wsdl file name
     * @param remoteInterfaceClass
     *            The remote interface class
     * @param descriptor
     *            The WSDL extensions descriptor
     * @param classesId
     *            The classes UIDs
     * @return the file
     * 
     * @throws EJBWSDLGenerationException
     *             If some problems occurs
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    private static File createWsdlFromClass(Class remoteInterfaceClass,
            String wsdlFileName,  WSDLDescriptor descriptor, Properties classesId)
            throws EJBWSDLGenerationException {

        // From the remote interface, creates the WSDL
        try {

            ObjectServiceFactory factory = new ObjectServiceFactory();
            

            Map<String, Object> props = new HashMap<String, Object>();
            
            // The portTypeName must be the same of the class name and of the service name
            QName interfaceName = ServiceUtils.makeQualifiedNameFromClass(remoteInterfaceClass);
            props.put(ObjectServiceFactory.PORT_TYPE, interfaceName);
           
            props.put(AbstractWSDL.GENERATE_IMPORTS, "true");
            props.put(ObjectServiceFactory.STYLE, SoapConstants.STYLE_WRAPPED);
            props.put(ObjectServiceFactory.USE, SoapConstants.USE_LITERAL);
            
            factory.getSoap11Transports().clear();
            factory.getSoap12Transports().clear();
            // No binding!!!!! Generates only the abstract part.
            
            AegisBindingProvider binder = (AegisBindingProvider)factory.getBindingProvider();
            DefaultTypeMappingRegistry tmr = (DefaultTypeMappingRegistry)binder.getTypeMappingRegistry();
            // here we disuade XFire from its rather annoying tendency to assume that, just because
            // anything in Java can be null, that we want to advertise all that nullity all over.
            Configuration configuration = tmr.getConfiguration();
            configuration.setDefaultMinOccurs(1);           
            configuration.setDefaultNillable(true);            

            Service service = factory.create(remoteInterfaceClass, interfaceName.getLocalPart(), interfaceName.getNamespaceURI(), props);
            
            File wsdlFile = new File(wsdlFileName);
            wsdlFile.createNewFile();

            FileOutputStream f = new FileOutputStream(wsdlFile);
            service.getWSDLWriter().write(f);                        
            
            addJbi4EjbExtensionsToWSDL(wsdlFile, descriptor, classesId, service);
            
            // hack the wsdl (subs minoccurs=0 to minoccurs=1).
            // hackWsdl(wsdlFile);
            
            return wsdlFile;
        } catch (MalformedURLException mex) {
            // TODO i18n
            LOG.error(mex.getMessage());
            throw new EJBWSDLGenerationException(mex);
        }  catch (IOException ioex) {
            // TODO i18n
            LOG.error(ioex.getMessage());
            throw new EJBWSDLGenerationException(ioex);
        } catch (WSDLException ex) {
            // TODO i18n
            LOG.error(ex.getMessage());
            throw new EJBWSDLGenerationException(ex);
        }
    }          
    

    
    /**
     * Adds jbi4ejb extensions to wsdl. This code expects that the PortTypeName
     * is
     * 
     * @param wsdlDescriptor
     *            The wsdl descriptor
     * @param wsdlFile
     *            The WSDL file
     * @param xfireService
     *            The xfire service
     * @param classesId
     *            The classes UIDs
     * @throws EJBWSDLGenerationException
     *             if something go wrong
     * @throws WSDLException
     *             if there are problem in WSDL resding/writing
     */
    @SuppressWarnings( { "deprecation", "unchecked" })
    private static void addJbi4EjbExtensionsToWSDL(File wsdlFile, WSDLDescriptor wsdlDescriptor, Properties classesId, Service xfireService)
            throws EJBWSDLGenerationException, WSDLException {
        
        // Creates the address extensibility elements
        Jbi4EjbAddress ejbAddress = new Jbi4EjbAddress();
        ejbAddress.setName(wsdlDescriptor.getName());
        ejbAddress.setLocalizationType(wsdlDescriptor.getLocalizationType());
        ejbAddress.setElementType(Jbi4EjbExtension.Q_ELEM_JBI4EJB_ADDRESS);
        
        // Creates the binding extensibility elements
        Jbi4EjbBinding ejbBinding = new Jbi4EjbBinding();
        if (wsdlDescriptor.isCorbaName()) {
            ejbBinding.setOrbProperties(wsdlDescriptor.getOrbProperties());
        } else {
            // JNDI
            ejbBinding.setJndiProperties(wsdlDescriptor.getJndiProperties());
        }
        ejbBinding.setElementType(Jbi4EjbExtension.Q_ELEM_JBI4EJB_BINDING);
        
        // Creates the types extensibility elements
        Jbi4EjbTypes ejbTypes = new Jbi4EjbTypes();
        ejbTypes.setTypesSerialVersionUIDs(classesId);
        ejbTypes.setElementType(Jbi4EjbExtension.Q_ELEM_JBI4EJB_TYPES);

        Definition wsdl = readWsdl(wsdlFile);               
        
        // Adds the WSDL extensions
        wsdl.addNamespace(Jbi4EjbExtension.DEFAULT_PREFIX,
                Jbi4EjbExtension.NS_URI_JBI4EJB);
        
        javax.wsdl.Service service = wsdl.getService(xfireService.getName());
        
        // The port name is the name of the service + "Port"
        String portName = service.getQName().getLocalPart() + "Port";
        Port port = wsdl.createPort();
        port.setName(portName);
        service.addPort(port);                

        // The binding name is the name of the service + "Binding"
        String bindingName = service.getQName().getLocalPart() + "Binding";
        Binding binding = wsdl.createBinding();
        wsdl.addBinding(binding);
                
        QName bindingQName = new QName(service.getQName().getNamespaceURI(), bindingName);
        binding.setQName(bindingQName);
        port.setBinding(binding);
                                                 
        // Gets the portType reference        
        QName portTypeQName = xfireService.getServiceInfo().getPortType();
        PortType portType = wsdl.getPortType(portTypeQName);
        if (portType == null) {
            // TODO i18n
            String msg = "No PortType found wth name: " + portTypeQName;
            LOG.error(msg);
            throw new EJBWSDLGenerationException(msg);
        }
        
        binding.setPortType(portType);        
        // Add the bindingOperations (with input,output and faults)
        List<Operation>  operations = portType.getOperations();
        for (Operation operation: operations) {            
            BindingOperation bop = wsdl.createBindingOperation();
            bop.setName(operation.getName());
            Input input = operation.getInput();
            if (input != null) {
                BindingInput bindingInput = wsdl.createBindingInput();
                bindingInput.setName(input.getName());
                bop.setBindingInput(bindingInput);
            }
            Output output = operation.getOutput();
            if (output != null) {
                BindingOutput bindingOutput = wsdl.createBindingOutput();
                bindingOutput.setName(output.getName());
                bop.setBindingOutput(bindingOutput);
            }
                        
            Map faults = operation.getFaults();
            Iterator faultIt = faults.entrySet().iterator();
            while (faultIt.hasNext()) {                
                Fault fault = (Fault)((Map.Entry)faultIt.next()).getValue();                
                BindingFault bindingFault = wsdl.createBindingFault();
                bindingFault.setName(fault.getName());
                bop.addBindingFault(bindingFault);
            }
            binding.addBindingOperation(bop);                
        }        
        
        binding.setUndefined(false);
        
        // Adds the ejb extensibility elements
        port.addExtensibilityElement(ejbAddress);
        binding.addExtensibilityElement(ejbBinding);
        wsdl.addExtensibilityElement(ejbTypes);
        
        // Deletes the previous file 
        wsdlFile.delete();        
        
        try {
            writeWsdl(wsdl, wsdlFile);
        } catch (IOException e) {
            // TODO i18n
            String msg = "Error in writing definition on file system";
            LOG.error(msg);
            throw new EJBWSDLGenerationException(msg);
        }
       
    }    
    
    /**
     * Reads a <code>Definition</code> from a <code>File</code>.
     * @param f the file to read
     * @return the WSDL definition
     * @throws javax.wsdl.WSDLException if there are problem in reading the WSDL
     */
    private static Definition readWsdl(final File f) throws WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        ExtensionRegistry registry = wsdlFactory
        .newPopulatedExtensionRegistry();        
        final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory)
        .newWSDLReader();
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        Jbi4EjbExtension.register(registry);
        LOG.debug("Extension QName: " + Jbi4EjbExtension.NS_URI_JBI4EJB);
        reader.setExtensionRegistry(registry);
        final Definition def = reader.readWSDL(f.getAbsolutePath());
        return def;
    }  
    
    /**
     * Reads a <code>Definition</code> from a <code>File</code>.
     * 
     * @param wsdlFile
     *            the file to read
     * @param wsdl
     *            the WSDL definition to write
     * 
     * @throws javax.wsdl.WSDLException
     *             if there are problem in reading the WSDL
     * @throws IOException
     *             if somethong go wrong in wile writing
     * @throws WSDLException
     *             if something go wrong in WSDL writing
     */
    private static void writeWsdl(Definition wsdl, final File wsdlFile) throws WSDLException, IOException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        
        ExtensionRegistry registry = wsdlFactory
            .newPopulatedExtensionRegistry();
        Jbi4EjbExtension.register(registry);
        wsdl.setExtensionRegistry(registry);
        final WSDLWriter writer = wsdlFactory.newWSDLWriter();        
        writer.writeWSDL(wsdl, new FileWriter(wsdlFile));        
    }     
    

    /**
     * Gets the classes ID.
     * 
     * @param remoteInterface
     *              The remote interface
     * @param classesDir
     *              Where the classes are
     * @return the classes ID
     * 
     * @throws EJBWSDLGenerationException
     *              If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Properties getClassesID(String remoteInterface, File classesDir) throws EJBWSDLGenerationException {
        
        LOG.debug("looking for " + remoteInterface + " in directory: " + classesDir.getAbsolutePath());
        
        Properties classesID = new Properties();

        String remoteInterfaceFileName = remoteInterface.replace('.',File.separatorChar);

        LOG.debug("remoteInterfaceFileName: " + remoteInterfaceFileName);
        List<File> list = new ArrayList<File>();
        list.add(new File(classesDir.getAbsolutePath() + File.separatorChar + remoteInterfaceFileName));

        // Find the classes used by the remote interface (with recursion).
        Set<Class> classesToSerialize;
        try {
            classesToSerialize = Util.findClassUsed(classesDir.getAbsolutePath(), list);
        } catch (ClassGenerationException e) {
            // TODO i18n
            LOG.error(e.getMessage());
            throw new EJBWSDLGenerationException(e);
        }

        Iterator iter = classesToSerialize.iterator();
        while (iter.hasNext()) {
            Class classToSerialize = (Class)iter.next();
            LOG.debug("Looking for class: " + classToSerialize.getName());
                        
            ObjectStreamClass objectStreamClass = ObjectStreamClass.lookup(classToSerialize);
            
            if (objectStreamClass == null) {
                // TOSO i18n
                LOG.warn("The objectStreamClass is null for class: " + classToSerialize.getName()+ 
                        " the class is Serializable?");
                
            } 
            if (objectStreamClass != null) {

                long uid = objectStreamClass.getSerialVersionUID();

                /*
                // With com.sun.corba.se.impl.io.ObjectStreamClass;
                long uid = ObjectStreamClass.getSerialVersionUID(classToSerialize);
    
                if (uid == 0L) {
                    uid = ObjectStreamClass.getActualSerialVersionUID(classToSerialize);
                }
                 */            

                LOG.debug(classToSerialize.getName() + " uid: " + uid);                
                classesID.put(classToSerialize.getName() , Long.valueOf(uid));
            }
        }            

        return classesID;        
    }           
        
}
