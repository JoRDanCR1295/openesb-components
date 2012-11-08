/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/


package it.imolinfo.jbi4ejb.jbi.component;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.exception.Jbi4EjbDeployException;
import it.imolinfo.jbi4ejb.exception.Jbi4EjbException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.jbi.component.runtime.ComponentRuntime;
import it.imolinfo.jbi4ejb.jbi.component.runtime.DefaultServiceUnitManager;
import it.imolinfo.jbi4ejb.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint;
import it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbProviderEndpoint;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbAddress;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbBinding;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbExtension;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbExtensionUtils;
import it.imolinfo.jbi4ejb.jbi.wsdl.Jbi4EjbTypes;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;

/**
 * Jbi4Ejb Service Unit Manager. 
 * Redefines: deploy/implement (no init).
 * @see Jbi4EjbLifeCycle  for more details of the generated code.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbSUManager extends DefaultServiceUnitManager {

    /** The Logger. */
    private static final Logger LOG = LoggerFactory
            .getLogger(Jbi4EjbSUManager.class);    
    private static final Messages MESSAGES = Messages.getMessages(Jbi4EjbSUManager.class);
    
    /** The deployed endpoints. */
    private List<Jbi4EjbEndpoint> deployedEndpoints = new ArrayList<Jbi4EjbEndpoint>();

    /** The activated endpoints. */
    private List<Jbi4EjbEndpoint> activatedEndpoints = new ArrayList<Jbi4EjbEndpoint>();
    
    /** The life cycle. */
    private Jbi4EjbLifeCycle lifeCycle = null;
    
    /**
     * Constructor that takes the ComponentRuntime parameter.
     * 
     * @param ctx the component runtime context
     */
    public Jbi4EjbSUManager(ComponentRuntime ctx) {        
        super(ctx);        
        this.lifeCycle = (Jbi4EjbLifeCycle)ctx.getLifeCycle();
    }

    /**
     * Deploy a Service Unit to the component.
     * 
     * @param suName the service unit name
     * @param suZipPath the service unit unzip path
     * 
     * @return the xml message string
     * 
     * @throws DeploymentException if some deploy problem occurs
     * 
     * @see javax.jbi.component.ServiceUnitManager#deploy(String, String);
     */
    public String deploy(String suName, String suZipPath) 
    throws DeploymentException {
        
        final String taskName = "deploy";
        boolean isSuccess = true;

        LOG.info("EJB000109_Deploying_su_in_SA", new Object[]{suName}, 
        		new Object[]{suZipPath});
        // DO nothing...
                
        String retMsg = createComponentTaskResultXML(taskName, isSuccess);        
        return retMsg;
    }
    
    /**
     * Start the deployed service unit.
     * 
     * @param serviceUnitName the service unit name
     * 
     * @throws DeploymentException if some problem occurs
     * 
     * @see javax.jbi.component.ServiceUnitManager#start(String);
     */
    public void start(String serviceUnitName) throws javax.jbi.management.DeploymentException {        

    	LOG.info("EJB000110_Starting_serviceUnit", new Object[]{serviceUnitName});
        
    	LOG.info("EJB000111_Deployed_Endpoints_number", new Object[]{deployedEndpoints.size()});
        
        // activate the SU deploy
        for (Jbi4EjbEndpoint endpoint : deployedEndpoints) {
            LOG.debug("endpoint.getSuName():" + endpoint.getSuName() );
            if (endpoint.getSuName().equals(serviceUnitName)) {
                try {
                    // Register the service
                    LOG.debug("Registering the service: " + endpoint.getServiceName());
                    endpoint.registerService();
                    // Activate the endpoint
                    LOG.debug("Activating the service: " + endpoint.getServiceName());
                    activateEndpoint(endpoint);
                    // Add the endpoint to the activated list
                    activatedEndpoints.add(endpoint);
                } catch (Jbi4EjbException e) {
                	String msg=MESSAGES.getString("EJB000112_Failure_in_starting_endpoint", 
                			new Object[] {endpoint});
                    LOG.error(msg,e);
                    throw new DeploymentException(msg,e);
                }
            }
        }

    }

    /**
     * Undeploy a service unit from the component.
     * 
     * @param suZipPath
     *            the service unit unzip path
     * @param serviceUnitName
     *            the service unit name
     * @return the xml result message
     * 
     * @throws DeploymentException
     *             if some problem occurs
     * 
     * @see javax.jbi.component.ServiceUnitManager#undeploy(String, String);
     */
    public String undeploy(String serviceUnitName, String suZipPath)
            throws DeploymentException {
        final String taskName = "undeploy";
        boolean isSuccess = true;
        // Do nothing...
        String retMsg = createComponentTaskResultXML(taskName, isSuccess);
        return retMsg;
    }
    
    
    /**
     * Stop the service unit.
     * 
     * @param serviceUnitName the service unit name
     * @throws DeploymentException if some problem occurs
     */
    @SuppressWarnings("unchecked")
    public void stop(String serviceUnitName)
    throws javax.jbi.management.DeploymentException {        
                
        List<Jbi4EjbEndpoint> endpointsToRemove = new ArrayList<Jbi4EjbEndpoint>();
       
        // deactivate the SU deploy
        for (Jbi4EjbEndpoint ejbEndpoint : activatedEndpoints) {
            if (ejbEndpoint.getSuName().equals(serviceUnitName))   {          
                try {
                    // Unregister the service
                    ejbEndpoint.unregisterService();
                    // Deactivate the endpoint
                    deactivateEndpoint(ejbEndpoint);
                    // Removes the endpoint from the activated endpoint list
                    endpointsToRemove.add(ejbEndpoint);       

                } catch (Jbi4EjbException e) {
                	String msg=MESSAGES.getString("EJB000113_Failure_in_stopping_endpoint", 
                			new Object[] {ejbEndpoint});
                    LOG.error(msg,e);
                    throw new DeploymentException(msg,e);
                }
            }
        }
        activatedEndpoints.removeAll(endpointsToRemove);        
    }    
    
    ///////////////////////////////////////////////////////////////////////////
    // Service Unit Lifecycle Management methods implementation
    ///////////////////////////////////////////////////////////////////////////

    
    /* (non-Javadoc)
     * @see it.imolinfo.jbi4ejb.jbi.component.runtime.DefaultServiceUnitManager#init(java.lang.String, java.lang.String)
     */
    
    /**
     * Service unit init, process the Endpoint deploy.
     * @param serviceUnitName the service unit name
     * @param serviceUnitRootPath the service unit root path
     * @throws DeploymentException if some problem occurs
     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
    throws javax.jbi.management.DeploymentException {
        
        // Gets the deployed endpoints and adds to the deployed endpoint list        
        deployedEndpoints.addAll(processDeploy(serviceUnitName, serviceUnitRootPath));
        
        LOG.debug("Init: do nothing");
        
    }
    
    /**
     * Service unit shutdown, removes the deployed endpoint.
     * @param serviceUnitName the service unit name
     * @see javax.jbi.component.ServiceUnitManager#shutdown(String);
     * @throws DeploymentException if some problem occurs
     */
    public void shutDown(String serviceUnitName)
    throws javax.jbi.management.DeploymentException {

        // Removes the deployed endpoint
        List<Jbi4EjbEndpoint> deployedEndpointsToRemove = new ArrayList<Jbi4EjbEndpoint>();
        // remove the SU deployed endpoints
        for (Jbi4EjbEndpoint ejbEndpoint : deployedEndpoints) {
            if (ejbEndpoint.getSuName().equals(serviceUnitName))   {          
                deployedEndpointsToRemove.add(ejbEndpoint);       
            }
        }
        // REmoves t he endpoints from the deployed endpoint list
        deployedEndpoints.removeAll(deployedEndpointsToRemove);
    }       
    
    /**
     * Gets the started endpoint from the <code>ServiceEndpoint</code> description. 
     * @param endpoint the <code>Jbi4EjbEndpoint</code>
     * @return the endpoint if found, null otherwise
     */
    public Jbi4EjbEndpoint getStartedEndpoint(ServiceEndpoint endpoint) {
     // deactivate the SU deploy
        for (Jbi4EjbEndpoint ejbEndpoint : activatedEndpoints) {
                       
            // Two endpoits are the same if the Service and the Endpoint name is the same
            if ((ejbEndpoint.getServiceName().equals(endpoint.getServiceName())) &&
                    (ejbEndpoint.getEndpointName().equals(endpoint.getEndpointName()))) {
                return ejbEndpoint;
            }                
        }     
        // endpoint not found
        return null;
    }

    /**
     * Process deploy.
     * 
     * @param suName the service unit name
     * @param suZipPath the unzip path
     * 
     * @return the list< jbi4 ejb endpoint>
     * 
     * @throws DeploymentException if some problem occurs
     * @throws  
     */
    private List<Jbi4EjbEndpoint> processDeploy(String suName, String suZipPath)
            throws DeploymentException {

        final File suDir = new File(suZipPath);

        LOG.debug("The SU dir path is: " + suDir.getAbsolutePath());

        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
                .newInstance();
        DocumentBuilder documentBuilder = null;

        try {
            documentBuilder = docBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
        	String msg=MESSAGES.getString("EJB000115_Failure_in_creating_document_builder", 
        			new Object[] {docBuilderFactory});
            LOG.error(msg,ex);
            throw new DeploymentException(msg,ex);
        }

        // read all wsdl files to see if
        final FileToDefinitionInfo[] fileToDefs = readAllDefinitions(suDir);

        final ArrayList<Jbi4EjbEndpoint> endpoints = new ArrayList<Jbi4EjbEndpoint>();

        if (SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING) {
        	LOG.info("EJB000116_Parse_jbi.xml_to_resolve_service_end_point_in_given_path", 
        			new Object[]{suDir.getAbsolutePath()});

            EndpointIdentifier[] svcs = null;

            SUDescriptorSupport descSupport = null;
            try {
                descSupport = new SUDescriptorSupport(
                        suDir.getAbsolutePath());                        
				// Gets the service description from the jbi.xml
                svcs = descSupport.getServices();
            } catch (ConfigurationException ex) {
            	String msg=MESSAGES.getString("EJB000117_Failure_in_getting_service_description");
                LOG.error(msg,ex);
                throw new DeploymentException(msg,ex);
            }

            final int len = svcs.length;   
            
            LOG.info("EJB000118_Found_endpoint_declared_in_the_jbi.xml", 
            		new Object[]{svcs.length});

            for (int i = 0; i < len; i++) {
                Definition matchedDef = null;
                File matchedWSDL = null;
                Jbi4EjbBinding ejbBinding = null;
                Jbi4EjbAddress ejbAddress = null;
                Jbi4EjbTypes ejbTypes = null;

                // For each endpoint, analyzes the Definition.
                EndpointIdentifier epDesc = svcs[i];
                for (FileToDefinitionInfo element : fileToDefs) {
                    ejbBinding = Jbi4EjbExtensionUtils.getEjbBinding(element
                            .getDefinition(), epDesc.getServiceName()
                            .toString(), epDesc.getEndpointName());                    
                    if (ejbBinding != null) {
                        matchedDef = element.getDefinition();
                        // this is the wsdl that contains the service
                        // endpoint, save it
                        matchedWSDL = element.getFile();

                        LOG.debug("Found the ejb-binding in file:" + element.getFile());
                        break;
                    }
                }
                if (ejbBinding != null) {
                    ejbAddress = Jbi4EjbExtensionUtils.getEjbAddress(
                            matchedDef, epDesc.getServiceName().toString(),
                            epDesc.getEndpointName());

                    if (ejbAddress == null) {
                    	String msg=MESSAGES.getString("EJB000119_No_address_found");
                        LOG.error(msg);
                        throw new DeploymentException(msg);
                    }
                    
                    ejbTypes = Jbi4EjbExtensionUtils.getEjbTypes(matchedDef);
                    if (ejbTypes == null) {
                        LOG.warn("EJB000120_No_types_found");
                    }                   
                    
                    PortType portType = Jbi4EjbExtensionUtils.getPortType(matchedDef, epDesc.getServiceName().toString(),
                            epDesc.getEndpointName());                                       
                   
                    // Gets the endpoint data
                    final String endpoint = epDesc.getEndpointName();
                    final String endpointNameLocalPart = QName
                            .valueOf(endpoint).getLocalPart();
                    
                    // For this BC, all are Providers endpoint                    
                    Jbi4EjbProviderEndpoint ejbEndpoint;
                    try {
                        ejbEndpoint = new Jbi4EjbProviderEndpoint(epDesc.getServiceName(), endpointNameLocalPart);
                    } catch (Jbi4EjbException e1) {
                    	String msg=MESSAGES.getString("EJB000121_Failure_in_creating_new_provider_endpoint", 
                    			new Object[] {epDesc.getServiceName()});
                        LOG.error(msg,e1);
                        throw new DeploymentException(msg,e1);
                    }
                    
                    // Gets the endpoint data                    
                    ejbEndpoint.setDefinition(matchedDef);
                    ejbEndpoint.setState(Jbi4EjbEndpoint.SHUTDOWN);
                    ejbEndpoint.setSuName(suName);
                    ejbEndpoint.setSuManager(this);                    
                    ejbEndpoint.setEndpointWSDL(matchedWSDL);                    
                    
                    // Gets the service data
                    ProviderServiceDescriptor providerServiceDescriptor = new ProviderServiceDescriptor();                    
                    providerServiceDescriptor.setComponentRootPath(suZipPath);
                    providerServiceDescriptor.setName(ejbAddress.getName());
                    providerServiceDescriptor.setLocalizationType(ejbAddress.getLocalizationType());
                    providerServiceDescriptor.setJndiProperties(ejbBinding.getJndiProperties());
                    providerServiceDescriptor.setOrbProperties(ejbBinding.getOrbProperties());
                    if (ejbTypes != null) {
                        providerServiceDescriptor.setSerialVersionUID(ejbTypes.getTypesSerialVersionUIDs());
                    }
                    providerServiceDescriptor.setPortTypeName(portType.getQName());
                    providerServiceDescriptor.setServiceName(epDesc.getServiceName());
                    ejbEndpoint.setServiceDescriptor(providerServiceDescriptor);

                    LOG.info("EJB000122_Parses_the_matched_WSDL");
                    Document result;
                    try {
                        result = documentBuilder.parse(matchedWSDL);
                    } catch (SAXException e) {
                    	String msg=MESSAGES.getString("EJB000123_Error_in_parsing_the_deploy_WSDL", 
                    			new Object[] {e.getMessage()});
                        LOG.error(msg,e);
                        throw new DeploymentException(msg,e);
                    } catch (IOException e) {
                    	String msg=MESSAGES.getString("EJB000123_Error_in_parsing_the_deploy_WSDL", 
                    			new Object[] {e.getMessage()});
                        LOG.error(msg,e);
                        throw new DeploymentException(msg,e);
                    }
                    ejbEndpoint.setServiceDescription(result);

                    endpoints.add(ejbEndpoint);
                }
            }
        }

        return endpoints;
    }
    
    /**
     * Activate the endpoint.
     * @param endpoint the ednpoint to activate
     * @throws Jbi4EjbDeployException if some deployment problem occurs
     */
    private void activateEndpoint(final Jbi4EjbEndpoint endpoint) throws Jbi4EjbDeployException {
                
        LOG.info("EJB000124_Activating_endpoint", new Object[]{endpoint.getUniqueName()});
        
        if (activatedEndpoints.indexOf(endpoint) != -1) {            
        	String msg=MESSAGES.getString("EJB000125_Failed_to_deploy_endpoint_because_already_registered", 
        			new Object[] {endpoint.getUniqueName()});
            LOG.error(msg);
            throw new Jbi4EjbDeployException(msg);
        }
        try {
            QName serviceName = (QName) endpoint.getServiceName();
            ServiceEndpoint serviceEndpoint 
                = RuntimeHelper.getComponentContext().activateEndpoint(endpoint.getServiceName(), endpoint.getEndpointName());
                        
            endpoint.setServiceEndpoint(serviceEndpoint);
            LOG.info("EJB000126_Activated_endpoint", new Object[]{serviceName});
            endpoint.setState(Jbi4EjbEndpoint.RUNNING);          
            
        } catch (final JBIException me) {
        	String msg=MESSAGES.getString("EJB000127_Cannot_activate_endpoint", 
        			new Object[] {endpoint.getServiceName()}, 
        			new Object[] {me.getMessage()});
            LOG.error(msg);
            throw new Jbi4EjbDeployException(msg);
        }        
    }
    
    /**
     * Activate the endpoint array.
     * @param endpoint the endpoints to deactivate
     * @throws Jbi4EjbDeployException if some deployment problem occurs
     */
    public void deactivateEndpoint(final Jbi4EjbEndpoint endpoint) throws Jbi4EjbDeployException {
    	LOG.info("EJB000128_Deactivating_endpoint", new Object[]{endpoint.getUniqueName()});
        

        if(!activatedEndpoints.contains(endpoint)) {
        	LOG.error("EJB000129_Endpoint_not_active", new Object[] {endpoint.getUniqueName()});
        } else  {
            try {
                RuntimeHelper.getComponentContext().deactivateEndpoint(endpoint.getServiceEndpoint());                
                LOG.info("EJB000130_Endpoint_deactivated", new Object[]{endpoint.getServiceEndpoint()});
                endpoint.setState(Jbi4EjbEndpoint.STOPPED);                
            } catch (JBIException me) {
                //String msg = "Cannot deactivate endpoint " + endpoint.getServiceName() +", reason: " + me.getMessage();
                //LOG.error(msg);
                //throw new Jbi4EjbDeployException(msg);
            	String msg=MESSAGES.getString("EJB000131_Cannot_deactivate_endpoint", 
            			new Object[] {endpoint.getServiceName()}, 
            			new Object[] {me.getMessage()});
                LOG.error(msg);
                throw new Jbi4EjbDeployException(msg);   
	
            }            
        }           
        
   }    
    

    /**
     * Get all the WSDL file recursively in a directory.
     * @param currentDir the directory where to search for the WSDLs
     * @return the list of wsdl files
     */
    private List<File> listWSDLFiles(final File currentDir) {
        final List<File> cumulativeResults = new ArrayList<File>();
        final File[] filesInCurrentDir = currentDir.listFiles();

        for (File element : filesInCurrentDir) {

            if (element.isFile()) {

                if (element.getName().toLowerCase().endsWith(".wsdl")) {
                    if (!(element.getName().contains("Wrapper"))) {
                        cumulativeResults.add(element);
                    }
                }
            } else if (element.isDirectory()) {
                final List<File> wsdlsInSubDirectories = listWSDLFiles(element);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    /**
     * Return all the files an definition in a <code>FileToDefinitionInfo</code>
     * array.
     *      
     * @param dir the directory where the WSDLs are
     * 
     * @return the  <code>FileToDefinitionInfo</code> array
     * 
     * @throws DeploymentException if some problem occurs
     */
    FileToDefinitionInfo[] readAllDefinitions(final File dir)
            throws DeploymentException {

        final List<File> wsdls = listWSDLFiles(dir);
        final File[] wsdlFiles = (File[]) wsdls.toArray(new File[0]);

        // read all wsdl files to see if
        FileToDefinitionInfo[] fileToDefs = null;

        if (wsdlFiles != null) {
            fileToDefs = new FileToDefinitionInfo[wsdlFiles.length];

            for (int i = 0; i < wsdlFiles.length; i++) {
                Definition def;
                try {
                    def = readWsdl(wsdlFiles[i]);
                } catch (WSDLException e) {
                    // TODO i18n
                    //LOG.error(e.getMessage());
                    //throw new DeploymentException(e);
                	String msg=MESSAGES.getString("EJB000132_Error_in_reading_wsdl_file", new Object[]{e.getMessage()});
                    LOG.error(msg,e);
                    throw new DeploymentException(msg,e);   

                }
                fileToDefs[i] = new FileToDefinitionInfo(wsdlFiles[i], def);
            }
        }

        return fileToDefs;
    }
    
  

    /**
     * Reads a <code>Definition</code> from a <code>File</code>.
     * @param f the file to read
     * @return the WSDL definition
     * @throws javax.wsdl.WSDLException if there are problem in reading the WSDL
     */
    public static Definition readWsdl(final File f) throws WSDLException {
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
     *  A value type for the file/definitions pairs.   
     */
    static class FileToDefinitionInfo {
        
        /** The file. */
        private File mFile;

        /** The definition. */
        private Definition mDefinition;

        /**
         * Instantiates a new file to definition info.
         * 
         * @param file the file
         * @param definition the definition
         */
        FileToDefinitionInfo(final File file, final Definition definition) {
            mFile = file;
            mDefinition = definition;
        }

        /**
         * Gets the file.
         * 
         * @return the file
         */
        public File getFile() {
            return mFile;
        }

        /**
         * Gets the definition.
         * 
         * @return the definition
         */
        public Definition getDefinition() {
            return mDefinition;
        }
    }

    /**
     * Gets the life cycle.
     * 
     * @return the life cycle
     */
    public Jbi4EjbLifeCycle getLifeCycle() {
        return lifeCycle;
    }

    /**
     * Sets the life cycle.
     * 
     * @param lifeCycle
     *            the new life cycle
     */
    public void setLifeCycle(Jbi4EjbLifeCycle lifeCycle) {
        this.lifeCycle = lifeCycle;
    }

    /**
     * Gets the deployed endpoints.
     * 
     * @return the deployed endpoints
     */
    public List<Jbi4EjbEndpoint> getDeployedEndpoints() {
        return deployedEndpoints;
    }
    
    
}
