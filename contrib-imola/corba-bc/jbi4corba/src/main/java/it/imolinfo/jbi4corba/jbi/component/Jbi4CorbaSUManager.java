/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.component;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaDeployException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.ComponentRuntime;
import it.imolinfo.jbi4corba.jbi.component.runtime.DefaultServiceUnitManager;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeConfiguration;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeHelper;
import it.imolinfo.jbi4corba.jbi.endpoint.ConsumerEndpoint;
import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;
import it.imolinfo.jbi4corba.jbi.endpoint.ProviderEndpoint;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaAddress;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaBinding;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtPreprocessDeserializer;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtension;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtensionRegistry;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtensionUtils;
import it.imolinfo.jbi4corba.utils.HelperFileUtil;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ChildFirstClassLoader;
import it.imolinfo.jbi4corba.webservice.generator.ClientCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.ProviderServiceClassesGenerator;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanException;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.SAXException;

import com.ibm.wsdl.Constants;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaIDLEntry;
import java.util.Arrays;
import java.util.Map.Entry;
import java.util.Set;
import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensibilityElement;
import org.apache.commons.io.FileUtils;

/**
 * Jbi4Corba Service Unit Manager.
 * Redefines: deploy/implement (no init).
 * @see Jbi4CorbaLifeCycle  for more details of the generated code.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaSUManager extends DefaultServiceUnitManager {

    /** The Logger. */
    private static final Logger LOG = LoggerFactory.getLogger(Jbi4CorbaSUManager.class);
    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES = Messages.getMessages(Jbi4CorbaSUManager.class);
    /** The deployed endpoints. */
    private List<Jbi4CorbaEndpoint> deployedEndpoints = new ArrayList<Jbi4CorbaEndpoint>();
    /** The activated endpoints. */
    private List<Jbi4CorbaEndpoint> activatedEndpoints = new ArrayList<Jbi4CorbaEndpoint>();
    /** The life cycle. */
    private Jbi4CorbaLifeCycle lifeCycle = null;
    private TransformerPool mTransformerPool;
    private RuntimeConfiguration runtimeConfiguration;
    /**This Hashtable contains the genrated classes **/
    private Hashtable<String, List<ClientCorbaClassesHolder>> classesTable;
    private URLClassLoader tmpURLclassLoader = null;
    private ChildFirstClassLoader tmpOriginaclassLoader = null;

    /**
     * Constructor that takes the ComponentRuntime parameter.
     *
     * @param ctx the component runtime context
     */
    public Jbi4CorbaSUManager(ComponentRuntime ctx) {
        super(ctx);
        this.lifeCycle = (Jbi4CorbaLifeCycle) ctx.getLifeCycle();
        mTransformerPool = new TransformerPool();
        classesTable = new Hashtable<String, List<ClientCorbaClassesHolder>>();
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
    public String deploy(String suName, String suZipPath) throws DeploymentException {

        final String taskName = "deploy";
        boolean isSuccess = true;

        LOG.info("CRB000214_Deploying_the_su_in_SA",
                new java.lang.Object[]{suName, suZipPath});

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

        LOG.debug("Starting the serviceUnit: " + serviceUnitName);
        LOG.debug("Deployed Endpoints number: " + deployedEndpoints.size());

        // activate the SU deploy
        for (Jbi4CorbaEndpoint endpoint : deployedEndpoints) {

            LOG.debug("endpoint.getSuName():" + endpoint.getSuName());
            if (endpoint.getSuName().equals(serviceUnitName)) {
                try {
                    // Register the service
                    LOG.debug("Registering the service: " + endpoint.getServiceName());
                    endpoint.registerService();

                    // Activate the endpoint
                    endpoint.activate();

                    // Activate the endpoint
                    LOG.debug("Activating the service for the bus: " + endpoint.getServiceName());
                    activateEndpoint(endpoint);
                    // Add the endpoint to the activated list
                    activatedEndpoints.add(endpoint);

                } catch (Jbi4CorbaException e) {
                    LOG.error(e.getMessage());
                    throw new DeploymentException(e);

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

        // Removes all unused classes from hashtable
        List<String> keys = new ArrayList<String>();
        //Remove the generated classes
        for (String key : classesTable.keySet()) {
            if (key.startsWith(serviceUnitName)) {
                keys.add(key);
            }
        }
        for (String key : keys) {
            classesTable.remove(key);
        }

        //classesTable.remove(LOG);
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
    public void stop(String serviceUnitName)
            throws javax.jbi.management.DeploymentException {

        List<Jbi4CorbaEndpoint> endpointsToRemove = new ArrayList<Jbi4CorbaEndpoint>();

        // deactivate the SU deploy
        for (Jbi4CorbaEndpoint corbaEndpoint : activatedEndpoints) {
            if (corbaEndpoint.getSuName().equals(serviceUnitName)) {
                try {

                    // Unregister the service
                    corbaEndpoint.deactivate();
                    // Deactivate the endpoint
                    deactivateEndpoint(corbaEndpoint);
                    // Removes the endpoint from the activated endpoint list
                    endpointsToRemove.add(corbaEndpoint);

                } catch (Jbi4CorbaException e) {
                    LOG.error(e.getMessage());
                    throw new DeploymentException(e);

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

        final StatusProviderHelper statusProviderHelper = lifeCycle.getStatusProviderHelper();
        try {
            // Gets the deployed endpoints and adds to the deployed endpoint list

            deployedEndpoints.addAll(processDeploy(serviceUnitName, serviceUnitRootPath, statusProviderHelper));
            LOG.debug("Init: do nothing");
        } catch (Exception ex) {
            // TODOi18n
            LOG.error("Failed to Deploy Service Unit:" + serviceUnitName + "\n" + ex, ex);

            throw new DeploymentException(ex);
        }

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
        List<Jbi4CorbaEndpoint> deployedEndpointsToRemove = new ArrayList<Jbi4CorbaEndpoint>();
        // remove the SU deployed endpoints
        for (Jbi4CorbaEndpoint corbaEndpoint : deployedEndpoints) {
            if (corbaEndpoint.getSuName().equals(serviceUnitName)) {
                deployedEndpointsToRemove.add(corbaEndpoint);
            }
        }
        // Removes t he endpoints from the deployed endpoint list
        deployedEndpoints.removeAll(deployedEndpointsToRemove);
    }

    /**
     * Returns the endpoint from using <code>ServiceEndpoint</code> from the started endpoint list.
     * @param endpoint the ServiceEndpoint
     * @return the <code>Jbi4CorbaEndpoint</code> if found, null otherwise
     */
    public Jbi4CorbaEndpoint getStartedEndpoint(ServiceEndpoint endpoint) {
        LOG.debug("Activated endpoint size: " + activatedEndpoints.size());
        return getEndpointFromList(endpoint, activatedEndpoints);
    }

    /**
     * Returns the endpoint from using <code>ServiceEndpoint</code> from the deployed endpoint list.
     * @param endpoint the ServiceEndpoint
     * @return the <code>Jbi4CorbaEndpoint</code> if found, null otherwise
     */
    public Jbi4CorbaEndpoint getDeployedEndpoint(ServiceEndpoint endpoint) {
        LOG.debug("Deployed endpoint size: " + activatedEndpoints.size());
        return getEndpointFromList(endpoint, deployedEndpoints);
    }

    private boolean containsTrueCorbaWsdl(File file, EntityResolver resolver, Map envVariableMap) throws WSDLException {
        final WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
        final WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
        reader.setEntityResolver(resolver);
        reader.setFeature(Constants.FEATURE_VERBOSE, true);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        reader.setExtensionRegistry(new Jbi4CorbaExtensionRegistry(envVariableMap));
        //LOG.debug("Extension QName: " + Jbi4CorbaExtension.NS_URI_JBI4CORBA);
        final Definition def = reader.readWSDL(file.getAbsolutePath());
        Map serviceMap = def.getServices();
        LOG.debug("servizi: " + Arrays.toString(serviceMap.keySet().toArray()));
        for (Entry entry : (Set<Entry>) serviceMap.entrySet()) {
            Service service = (Service) entry.getValue();
            for (Entry portEntry : (Set<Entry>) service.getPorts().entrySet()) {
                Port port=(Port)portEntry.getValue();
                List extElems = port.getExtensibilityElements();
                LOG.debug("extensibility elements: " + Arrays.toString(extElems.toArray()));
                for (ExtensibilityElement extensibilityElement : (List<ExtensibilityElement>) extElems) {
                    if (Jbi4CorbaAddress.class.isInstance(extensibilityElement)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Look for the endpoint in th endpoint list.
     * @param endpoint the ServiceEndpoint
     * @param endpointList the list to look for
     * @return the <code>Jbi4CorbaEndpoint</code> if found, null otherwise
     */
    private Jbi4CorbaEndpoint getEndpointFromList(ServiceEndpoint endpoint, List<Jbi4CorbaEndpoint> endpointList) {

        for (Jbi4CorbaEndpoint corbaEndpoint : endpointList) {
            LOG.debug("Looking for: " + endpoint.getServiceName() + "|" + endpoint.getEndpointName());

            // Two endpoits are the same if the Service and the Endpoint name is the same
            if ((corbaEndpoint.getServiceName().equals(endpoint.getServiceName())) &&
                    (corbaEndpoint.getEndpointName().equals(endpoint.getEndpointName()))) {
                LOG.debug("getEndpointFromList: Returning:" + corbaEndpoint);
                LOG.debug("getEndpointFromList: with service description:" + corbaEndpoint.getServiceDescription());
                return corbaEndpoint;
            }

        }
        LOG.debug("getEndpointFromList: Returning null");

        // endpoint not found
        return null;
    }

    private JbiServiceDescriptor getJbidesc(EndpointIdentifier epDesc, List<JbiServiceDescriptor> jbdescs) {
        for (int i = 0; i < jbdescs.size(); i++) {
            QName intname = new QName(jbdescs.get(i).getServiceNameSpace(), jbdescs.get(i).getServiceName());
            if (epDesc.getInterfaceName().equals(intname)) {
                return jbdescs.get(i);
            }
        }
        return jbdescs.get(0);
    }

    private ProviderServiceDescriptor getProviderdesc(List<ProviderServiceDescriptor> psdesc, String serviceName) {
        for (int i = 0; i < psdesc.size(); i++) {

            if (psdesc.get(i).getServiceName().equals(serviceName)) {
                return psdesc.get(i);
            }
        }
        LOG.debug("getProviderdesc: Returning null");
        return null;
    }

    /**
     * @throws ClassGenerationException
     * Refactory
     * Process deploy.
     *
     * @param suName the service unit name
     * @param suZipPath the unzip path
     *
     * @return the list< jbi4 corba endpoint>
     *
     * @throws DeploymentException if some problem occurs
     * @throws
     */
    private List<Jbi4CorbaEndpoint> processDeploy(String suName, String suZipPath, final StatusProviderHelper statusProviderHelper)
            throws DeploymentException, IOException, Jbi4CorbaRuntimeException, ClassGenerationException {

        final File suDir = new File(suZipPath);

        //Generate the JbiServiceDescriptor and Populate the configuration Map
        List<Jbi4CorbaSUInfo> jbisuInfo = new ArrayList<Jbi4CorbaSUInfo>();
        List<EndpointIdentifier> epidlist = new ArrayList<EndpointIdentifier>();

        for (int i = 0; i < jbisuInfo.size(); i++) {
            epidlist.add(jbisuInfo.get(i).getEpIdentifier());

        }

        List<JbiServiceDescriptor> jbdescs = getJbiDescriptors(suName, suZipPath, jbisuInfo);

        List<Jbi4CorbaEndpoint> endpoints = new ArrayList<Jbi4CorbaEndpoint>();

        ProviderServiceClassesGenerator serviceClassesGenerator = new ProviderServiceClassesGenerator();

        File rootDir = null;
        if (jbdescs.size() > 0) {
            rootDir = new File(
                    suZipPath + File.separator + jbdescs.get(0).hashCode());


            boolean resultMk = rootDir.mkdir();
            if (!resultMk) {
                throw new DeploymentException(MESSAGES.getString(
                        "CRB000201_Unable_to_create_working_directory",
                        rootDir.getAbsolutePath()));
            } else {
                LOG.debug("created working directory:" + rootDir.getAbsolutePath());
            }

        }

        List<ProviderServiceDescriptor> psdesc = null;


        //Collect all Descriptors and generate the classes
        if (rootDir != null) {
            psdesc = getProviderServiceDescriptor(suName, jbdescs, suZipPath, serviceClassesGenerator, rootDir);
        }

        Jbi4CorbaEndpoint corbaEndpoint = null;
        LOG.debug("The SU dir path is: " + suDir.getAbsolutePath());

        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();

        docBuilderFactory.setNamespaceAware(true);

        DocumentBuilder documentBuilder = null;

        try {
            documentBuilder = docBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            String msg = MESSAGES.getString("CRB000216_Failure_in_creating_document_builder",
                    new Object[]{ex.getMessage()});
            LOG.error(msg, ex);
            throw new DeploymentException(msg, ex);
        }

        //if(SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING){
        EndpointIdentifier epDesc;
        File matchedWSDL;
        Definition matchedDef;

        for (int i = 0; i < jbdescs.size(); i++) {

            epDesc = jbisuInfo.get(i).getEpIdentifier();
            matchedWSDL = jbisuInfo.get(i).getMatchedWSDL();
            matchedDef = jbisuInfo.get(i).getMatchedDef();


            rootDir = new File(
                    suZipPath + File.separator + jbdescs.get(i).hashCode());


            if (epDesc.isProvider()) {

                //PROVIDER
                corbaEndpoint = generateProvider(suZipPath,
                        epDesc,
                        getJbidesc(epDesc, jbdescs),
                        getProviderdesc(psdesc, jbdescs.get(i).getServiceName()),
                        rootDir);

            } else {

                // CONSUMER
                // Gets the consumer service descriptor
                ConsumerServiceDescriptor consumerServiceDescriptor = getConsumerServiceDescriptor(epDesc, jbdescs.get(i), rootDir);                        //
                consumerServiceDescriptor.setServiceWSDLDefinition(matchedDef);
                String endpointNameLocalPart = QName.valueOf(epDesc.getEndpointName()).getLocalPart();
                corbaEndpoint = new ConsumerEndpoint(epDesc.getServiceName(), endpointNameLocalPart, consumerServiceDescriptor);
            }

            // Reporting Stuff for monitoring Endpoint status
            if (statusProviderHelper != null) {
                StatusReporting reporting = statusProviderHelper.getStatusReporter();
                String uniqueName = null;
                //if(reporting!=null){
                uniqueName = corbaEndpoint.getUniqueName();
                if (epDesc.isProvider()) {
                    reporting.addProvisioningEndpoint(uniqueName);
                } else {
                    reporting.addConsumingEndpoint(uniqueName);
                }
                EndpointStatus stat = reporting.getEndpointStatus(uniqueName);
                corbaEndpoint.setEndpointStatus(stat);
            } else {
                LOG.warn("StatusProviderHelper is null.StatusReporting not created.");
            }

            // Gets the endpoint data
            corbaEndpoint.setDefinition(matchedDef);
            corbaEndpoint.setState(Jbi4CorbaEndpoint.SHUTDOWN);
            corbaEndpoint.setSuName(suName);
            corbaEndpoint.setSuManager(this);
            corbaEndpoint.setEndpointWSDL(matchedWSDL);


            LOG.debug("Parses the matched WSDL");
            Document result;
            try {
                LOG.debug("factory.isNamespaceAware=" + documentBuilder.isNamespaceAware());
                result = documentBuilder.parse(matchedWSDL);
            } catch (SAXException e) {
                String msg = MESSAGES.getString("CRB000219_Error_in_parsing_the_deploy_WSDL", new Object[]{e.getMessage()});
                LOG.error(msg);
                throw new DeploymentException(msg);
            } catch (IOException e) {
                String msg = MESSAGES.getString("CRB000219_Error_in_parsing_the_deploy_WSDL", new Object[]{e.getMessage()});
                LOG.error(msg);
                throw new DeploymentException(msg);
            }
            if (corbaEndpoint != null) {
                try {
                    corbaEndpoint.setServiceDescription(result);
                    endpoints.add(corbaEndpoint);

                    populateEndpointWSDLInfo(corbaEndpoint.getEndpointStatus(), corbaEndpoint);
                } catch (Exception ex) {
                    java.util.logging.Logger.getLogger(Jbi4CorbaSUManager.class.getName()).log(Level.SEVERE, null, ex);
                }
            } else {
                LOG.debug("CorbaEndpoint is Null");
            }
        }
        //Remove The class from the cache

        return endpoints;
    }

    @SuppressWarnings("unchecked")
    public Map<String, String[]> parseForEnvironmentVariables(String suPath, Map<String, String[]> envVariableMap)
            throws Jbi4CorbaRuntimeException {

        Map<String, String[]> envVariables =
                new HashMap<String, String[]>(envVariableMap);

        File catalog = new File(suPath +
                File.separator + "META-INF" + File.separator +
                "catalog.xml");

        EntityResolver resolver = null;

        if (catalog.exists()) {
            CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(catalog.getAbsolutePath());
            catalogManager.setRelativeCatalogs(true);
            catalogManager.setUseStaticCatalog(false);
            resolver = new CatalogResolver(catalogManager);
        }

        for (File file : listWSDLFiles(new File(suPath))) {
            try {
                envVariables.putAll(readWSDLForEnvVariables(file, resolver, envVariables));
            } catch (WSDLException ex) {
                String msg = MESSAGES.getString("CRB000233_Error_in_parsing_environment_variables");
                LOG.error(msg, ex.getMessage());
                throw new Jbi4CorbaRuntimeException(ex);
            }

        }

        return envVariables;
    }

    @SuppressWarnings("unchecked")
    private Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map<String, String[]> envVariableMap)
            throws WSDLException {

        WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
        WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
        reader.setEntityResolver(resolver);
        Jbi4CorbaExtPreprocessDeserializer preProcessDeserializer = new Jbi4CorbaExtPreprocessDeserializer(envVariableMap);
        reader.setExtensionRegistry(new Jbi4CorbaExtensionRegistry(preProcessDeserializer));
        reader.readWSDL(f.getAbsolutePath());
        return preProcessDeserializer.getEnvVariableMap();
    }

    @SuppressWarnings("unchecked")
    private Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap)
            throws WSDLException {

        final WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
        final WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
        reader.setEntityResolver(resolver);
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        reader.setExtensionRegistry(new Jbi4CorbaExtensionRegistry(envVariableMap));
        LOG.debug("Extension QName: " + Jbi4CorbaExtension.NS_URI_JBI4CORBA);
        final Definition def = reader.readWSDL(f.getAbsolutePath());
        return def;

    }

    /**
     * Activate the endpoint.
     * @param endpoint the ednpoint to activate
     * @throws Jbi4CorbaDeployException if some deployment problem occurs
     */
    private void activateEndpoint(final Jbi4CorbaEndpoint endpoint) throws Jbi4CorbaDeployException {

        LOG.debug("Activating endpoint: " + endpoint.getUniqueName());

        if (activatedEndpoints.indexOf(endpoint) != -1) {
            String msg = MESSAGES.getString("CRB000220_Failed_to_deploy_endpoint_because_already_registered",
                    new Object[]{endpoint.getUniqueName()});
            LOG.error(msg);
            throw new Jbi4CorbaDeployException(msg);
        }
        try {
            QName serviceName = (QName) endpoint.getServiceName();
            ServiceEndpoint serviceEndpoint = RuntimeHelper.getComponentContext().activateEndpoint(endpoint.getServiceName(), endpoint.getEndpointName());

            endpoint.setServiceEndpoint(serviceEndpoint);
            LOG.info("CRB000221_Endpoint_activated",
                    new Object[]{serviceName});
            endpoint.setState(Jbi4CorbaEndpoint.RUNNING);

        } catch (final JBIException me) {
            String msg = MESSAGES.getString("CRB000222_Cannot_activate_endpoint",
                    new Object[]{endpoint.getServiceName()}, new Object[]{me.getMessage()});
            LOG.error(msg, me);
            throw new Jbi4CorbaDeployException(msg, me);
        }
    }

    /**
     * Activate the endpoint array.
     * @param endpoint the endpoints to deactivate
     * @throws Jbi4CorbaDeployException if some deployment problem occurs
     */
    public void deactivateEndpoint(final Jbi4CorbaEndpoint endpoint) throws Jbi4CorbaDeployException {
        LOG.debug("Deactivating endpoint: " + endpoint.getUniqueName());

        if (!activatedEndpoints.contains(endpoint)) {
            String msg = MESSAGES.getString("CRB000223_Endpoint_not_active",
                    new Object[]{endpoint.getUniqueName()});
            LOG.error(msg);
            throw new Jbi4CorbaDeployException(msg);
        } else {
            try {
                RuntimeHelper.getComponentContext().deactivateEndpoint(endpoint.getServiceEndpoint());
                LOG.debug("Endpoint " + endpoint.getServiceEndpoint() + " deactivated");
                endpoint.setState(Jbi4CorbaEndpoint.STOPPED);
            } catch (JBIException me) {
                String msg = MESSAGES.getString("CRB000224_Cannot_deactivate_endpoint",
                        new Object[]{endpoint.getServiceName()}, new Object[]{me.getMessage()});
                LOG.error(msg, me);
                throw new Jbi4CorbaDeployException(msg, me);
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
                    cumulativeResults.add(element);
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

        final CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles(dir.getAbsolutePath() +
                File.separator + "xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);

        final EntityResolver resolver = new CatalogResolver(catalogManager);

        final List<File> wsdls = listWSDLFiles(dir);
        //final File[] wsdlFiles = (File[]) wsdls.toArray(new File[0]);

        // read all wsdl files to see if
        //FileToDefinitionInfo[] fileToDefs = null;

        List<FileToDefinitionInfo> fileToDefsList = new ArrayList<FileToDefinitionInfo>();

        if (wsdls.size() > 0) {
            //fileToDefs = new FileToDefinitionInfo[wsdlFiles.length];

            for (File file : wsdls) {
                Definition def = null;
                try {
                    LOG.debug("esamino il file: " + file.getName() + " per vedere se è un vero wsdl corba ... ");
                    boolean isTrueCorba = containsTrueCorbaWsdl(file, resolver, runtimeConfiguration.retrieveApplicationVariablesMap());
                    LOG.debug("esaminato il file: " + file.getName() + " è un vero wsdl corba: " + isTrueCorba);
                    if (isTrueCorba) {
                        def = readWSDL(file, resolver, runtimeConfiguration.retrieveApplicationVariablesMap());
                        fileToDefsList.add(new FileToDefinitionInfo(file, def));
                    }
                } catch (WSDLException e) {
                    String msg = MESSAGES.getString("CRB000225_Error_in_reading_wsdl_file",
                            new Object[]{e.getMessage()});
                    LOG.error(msg, e);
                    throw new DeploymentException(msg, e);
                }

            }
        }

        return fileToDefsList.toArray(new FileToDefinitionInfo[fileToDefsList.size()]);
    }

    /**
     * Reads a <code>Definition</code> from a <code>File</code>.
     * @param f the file to read
     * @return the WSDL definition
     * @throws javax.wsdl.WSDLException if there are problem in reading the WSDL
     */
    public static Definition readWsdl(final File f) throws WSDLException {
        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        ExtensionRegistry registry = wsdlFactory.newPopulatedExtensionRegistry();
        final WSDLReader reader = wsdlFactory.newWSDLReader();
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        Jbi4CorbaExtension.register(registry);
        LOG.debug("Extension QName: " + Jbi4CorbaExtension.NS_URI_JBI4CORBA);
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
    public Jbi4CorbaLifeCycle getLifeCycle() {
        return lifeCycle;
    }

    /**
     * Sets the life cycle.
     *
     * @param lifeCycle
     *            the new life cycle
     */
    public void setLifeCycle(Jbi4CorbaLifeCycle lifeCycle) {
        this.lifeCycle = lifeCycle;
    }

    /**
     * Gets the deployed endpoints.
     *
     * @return the deployed endpoints
     */
    public List<Jbi4CorbaEndpoint> getDeployedEndpoints() {
        return deployedEndpoints;
    }

    /**
     * Creates the <code>JbiServiceDescriptor</code> from the extended
     * WSDL-element. Creates also the idl file loaded from WSDL in the roortpath
     * populating the JbiServiceDescriptor with the idlFileName and the
     * idlFileNameDirectory.
     *
     * @param service               The service
     * @param port                  The port
     * @param binding               The binding
     * @param addressExtension      The address extension
     * @param bindingExtension      The binding extension
     * @param rootPath              The root path
     *
     * @return                      The JbiServiceDescriptor created.
     *
     * @throws DeploymentException  The deployment exception
     */
    protected JbiServiceDescriptor createServiceDescriptor(
            Service service,
            Binding binding,
            PortType portType,
            Jbi4CorbaAddress addressExtension,
            Jbi4CorbaBinding bindingExtension,
            String role,
            String rootPath) throws DeploymentException {

        JbiServiceDescriptor serviceDescriptor = new JbiServiceDescriptor();

        LOG.debug("Creating a JbiServiceDescriptor ... ");
        // Raff: temp hack to make it compile


        boolean idlFirstConsumer = (JbiServiceDescriptor.CONSUMER.equals(role) &&
                !bindingExtension.getJbi4CorbaDLEntryList().isEmpty());

        if (JbiServiceDescriptor.PROVIDER.equals(role) || idlFirstConsumer) {

            /*
             * for every idl entry extract the idl and writes it into the root path
             * if many wsdl are present in the su, all idl will be stored in the root path
             * so the idlj an classes generation can run just once
             */

            String idlFileName = null;

            for (Jbi4CorbaIDLEntry jbi4CorbaIDLEntry : bindingExtension.getJbi4CorbaDLEntryList()) {

                // Calculate the IDL filename using the binding name (the local part)
                // and the rootPath. Replaces ":" (is a path separator) with "."
                String idl = jbi4CorbaIDLEntry.getIDL();
                String fileName = jbi4CorbaIDLEntry.getIdlFilename();
                String relativePath = jbi4CorbaIDLEntry.getRelativePath();
                boolean root = jbi4CorbaIDLEntry.isRoot();
                if (root) {
                    idlFileName = fileName;
                    relativePath = ".";
                }

                LOG.debug("processing ideEntry: " + jbi4CorbaIDLEntry.getRelativePath() + File.separator + jbi4CorbaIDLEntry.getIdlFilename());

                String idlAbsoluteFileName = rootPath + File.separator + relativePath + File.separator + fileName;

                // Creates the IDL file in the SU directory
                LOG.debug("CRB000203_Producing_idl_file_to", new Object[]{idlAbsoluteFileName});
                FileWriter fr;
                try {
                    File file = new File(idlAbsoluteFileName);
                    File dir = new File(rootPath + File.separator + relativePath);
                    //the file is created only the first time it appears
                    if (!file.exists()) {
                        if (!dir.exists()) {
                            LOG.debug("creating directory: " + dir.getAbsolutePath());
                            FileUtils.forceMkdir(dir);
                        }
                        file.createNewFile();
                        fr = new FileWriter(file);
                        fr.write(idl);
                        fr.close();
                    }
                } catch (IOException e) {
                    String msg = MESSAGES.getString(
                            "CRB000204_Unable_to_open_filename",
                            idlAbsoluteFileName, e.getMessage());
                    LOG.error(msg, e);
                    throw new DeploymentException(msg, e);
                }

            }

            if (idlFileName == null) {
                String msg = MESSAGES.getString("CRB000236_No_Root_IDL_Entry_Defined");
                LOG.error(msg);
                throw new DeploymentException(msg);
            }



            serviceDescriptor.setIdlFileName(idlFileName);
            serviceDescriptor.setIdlFileNameDirectory(rootPath);

            serviceDescriptor.setPortTypeName(portType.getQName());

        } else if (JbiServiceDescriptor.CONSUMER.equals(role)) {
            // Do nothing (all the configuration parameters has been depreceted...)
        } else {
            // Error: No recognized role
            LOG.error("CRB000205_Invalid_role=", role);
            throw new DeploymentException(
                    MESSAGES.getString("CRB000205_Invalid_role=", role));
        }

        // Commons configuration
        serviceDescriptor.setRole(role);

        // Sets the namespace using the Service Namespace
        serviceDescriptor.setServiceNameSpace(service.getQName().getNamespaceURI());
        serviceDescriptor.setServiceName(service.getQName().getLocalPart());

        //This is for manage Endpoint that don't have ImolaCorba Address Extension
        if (addressExtension != null) {
            serviceDescriptor.setCorbaServiceName(addressExtension.getName());
            serviceDescriptor.setLocalizationType(
                    addressExtension.getLocalizationType());
            serviceDescriptor.setOrbProperties(addressExtension.getOrbProperties());
        }


        LOG.debug("Created a JbiServiceDescriptor=" + serviceDescriptor);
        return serviceDescriptor;
    }

    /**
     * This method gets an endpoint with a consumer role.
     *
     * @param   jbiServiceDescriptor  The jbi service descriptor
     * @param   rootDir               The root Dir
     */
    private ConsumerServiceDescriptor getConsumerServiceDescriptor(
            EndpointIdentifier epDesc,
            JbiServiceDescriptor jbiServiceDescriptor,
            File rootDir) {

        // here i must add endpoints for consumer role
        ConsumerServiceDescriptor consumerServiceDescriptor = new ConsumerServiceDescriptor();

        consumerServiceDescriptor.setRootPath(
                rootDir.getAbsolutePath());
        consumerServiceDescriptor.setServiceName(
                jbiServiceDescriptor.getServiceName());
        consumerServiceDescriptor.setCorbaServiceName(
                jbiServiceDescriptor.getCorbaServiceName());

        LOG.debug("Consumer CORBA name:" + consumerServiceDescriptor.getCorbaServiceName());

        consumerServiceDescriptor.setOrbProperties(
                jbiServiceDescriptor.getOrbProperties());
        consumerServiceDescriptor.setLocalizationType(
                jbiServiceDescriptor.getLocalizationType());

        // Sets the target endpoint (from the EndpointIdentifier)
        consumerServiceDescriptor.setTargetEndpoint(epDesc.getEndpointName());
        consumerServiceDescriptor.setTargetInterfaceName(epDesc.getInterfaceName());
        consumerServiceDescriptor.setTargetService(epDesc.getServiceName());

        consumerServiceDescriptor.setJbiServiceDescriptor(jbiServiceDescriptor);

        return consumerServiceDescriptor;

    }

    private void populateEndpointWSDLInfo(EndpointStatus endpointStatus, Jbi4CorbaEndpoint endpoint) throws Exception {
        //Set the resource info on the endpoint status if it is available
        Transformer transformer = mTransformerPool.retrieve();
        if (endpointStatus != null) {
            WSDLFactory wsdlFactory = (WSDLFactory) WSDLFactory.newInstance();
            @SuppressWarnings("unused")
            WSDLWriter writer = (WSDLWriter) wsdlFactory.newWSDLWriter();

            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            StringWriter sw = new StringWriter();
            StreamResult result = new StreamResult(sw);
            DOMSource source = new DOMSource(endpoint.getServiceDescription());
            transformer.transform(source, result);
            endpointStatus.setWSDLDefinition(sw.toString());

            mTransformerPool.relinquish(transformer);
        }

    }

    /**
     * This method extract all jbiDescriptor from SU name and SU-zipPath
     * @param suName Service Unit name
     * @suZipPath
     * @infolist The holder of Jbi4CorbaSUInfo
     *
     * @throws  DeploymentException       The deployment exception
     **/
    @SuppressWarnings("unchecked")
    private List<JbiServiceDescriptor> getJbiDescriptors(String suName, String suZipPath, List<Jbi4CorbaSUInfo> infolist) throws DeploymentException {


        List<JbiServiceDescriptor> descList = new ArrayList<JbiServiceDescriptor>();

        final File suDir = new File(suZipPath);
        LOG.info("SERVICE UNIT DIRECTORY" + suDir.getAbsolutePath());

        // create application configuration object map from runtimeconfig
        if (runtimeConfiguration == null) {
            runtimeConfiguration = lifeCycle.getRuntimeConfiguration();
        }

        // Populate the Application Variables

        Map<String, String[]> envVariableMap =
                parseForEnvironmentVariables(suZipPath, runtimeConfiguration.retrieveApplicationVariablesMap());
        if (envVariableMap.size() > runtimeConfiguration.countVariables()) {
            // number of environment variable tokens used in WSDLs
            // is greater than the ones defined
            try {
                runtimeConfiguration.updateApplicationVariablesMap(envVariableMap);
            } catch (MBeanException e) {
                String msg = MESSAGES.getString("CRB000234_Error_in_populating_the_application_variables");
                LOG.error(msg, e.getMessage());
                throw new Jbi4CorbaRuntimeException(e);
            }
        }


        // read all wsdl files.

        final FileToDefinitionInfo[] fileToDefs = readAllDefinitions(suDir);



        if (SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING) {
            LOG.debug("About to parse jbi.xml to resolve service end point in path:" + suDir.getAbsolutePath());

            EndpointIdentifier[] svcs = null;

            SUDescriptorSupport descSupport = null;

            try {
                descSupport = new SUDescriptorSupport(
                        suDir.getAbsolutePath());
                // Gets te service description from the jbi.xml

                svcs = descSupport.getServices();
            } catch (ConfigurationException ex) {
                String msg = MESSAGES.getString("CRB000217_Failure_in_getting_service_description",
                        new Object[]{ex.getMessage()});
                LOG.error(msg, ex);

            }

            //Serrvice's number
            final int len = svcs.length;

            Map appConfigObjectMap = new HashMap();
            if (runtimeConfiguration != null) {
                appConfigObjectMap = runtimeConfiguration.retrieveApplicationConfigurationsMap();
            }

            LOG.debug("Found " + svcs.length + " endpoint declared in the jbi.xml");

            //for each endpoint it create the jbidesc
            for (int i = 0; i < len; i++) {
                Definition matchedDef = null;
                File matchedWSDL = null;

                Jbi4CorbaBinding corbaBinding = null;
                Jbi4CorbaAddress corbaAddress = null;

                // For each endpoint, analyzes the Definition.
                EndpointIdentifier epDesc = svcs[i];

                String configName = epDesc.getApplicationConfigurationName();
                LOG.debug("Retrieved Application configuration Object name " + configName + " from Endpoint");
                // this application configuration name is not yet defined in the config MBean,
                // add it to the application configuration data structure

                LOG.debug("Retrieved Application configuration Object name " + configName + " from Endpoint");
                // this application configuration name is not yet defined in the config MBean,
                // add it to the application configuration data structure
                if (configName != null && !"".equals(configName) &&
                        !appConfigObjectMap.containsKey(configName)) {
                    appConfigObjectMap.put(configName, new String[3]);
                }
                String appconfigLocalizationType = null;
                String appconfigaddressName = null;
                String appconfigOrb = null;
                if (configName != null && !"".equals(configName)) {
                    // there is a config object defined for the endpoint.
                    // use this instead of the values defined in the WSDL

                    String[] values = (String[]) appConfigObjectMap.get(configName);
                    if (values != null) {
                        //appconfigIdl = values[0] ;
                        appconfigLocalizationType = values[0];
                        appconfigaddressName = values[1];
                        appconfigOrb = values[2];

                        LOG.debug("The values of " + configName + " Application Configuration Object are " + appconfigLocalizationType + ", " + appconfigaddressName + ", " + appconfigOrb);
                    }

                }

                //For each wsdl link wsdl to endpoint
                for (FileToDefinitionInfo element : fileToDefs) {


                    corbaBinding = Jbi4CorbaExtensionUtils.getCorbaBinding(element.getDefinition(), epDesc.getServiceName().toString(), epDesc.getEndpointName());

                    if (corbaBinding != null) {
                        matchedDef = element.getDefinition();
                        // this is the wsdl that contains the service
                        // endpoint, save it
                        matchedWSDL = element.getFile();

                        break;
                    }

                }

                corbaAddress = Jbi4CorbaExtensionUtils.getCorbaAddress(
                        matchedDef, epDesc.getServiceName().toString(),
                        epDesc.getEndpointName());

                if (corbaAddress != null) {

                    if (appconfigLocalizationType != null && !appconfigLocalizationType.equals("")) {
                        LOG.debug("Replacing Localization Type " + corbaAddress.getLocalizationType() + " with " + appconfigLocalizationType);
                        corbaAddress.setLocalizationType(appconfigLocalizationType);
                    }
                    if (appconfigaddressName != null && !appconfigaddressName.equals("")) {
                        LOG.debug("Replacing Address Name " + corbaAddress.getName() + " with " + appconfigaddressName);
                        corbaAddress.setName(appconfigaddressName);
                    }

                    //Set the Orb properties
                    //************************
                    if (appconfigOrb != null && !appconfigOrb.equals("")) {
                        LOG.debug("Replacing ORB properties " + corbaAddress.getOrbProperties() + " with " + appconfigOrb);
                        Properties orbProp = corbaAddress.getOrbProperties();
                        // Process the name value pairs returned by the orb field
                        String[] nameValuePairs = appconfigOrb.split(",");

                        for (String nameValuePair : nameValuePairs) {
                            String[] nameAndValue = nameValuePair.split("=");
                            String key = nameAndValue[0];
                            String value = null;
                            if (nameAndValue.length > 1) {
                                value = nameAndValue[1];
                            }
                            if (key != null && value != null && !key.equals("") && !value.equals("")) {
                                String oldValue = (String) orbProp.put(key, value);
                                LOG.debug("Set ORB property " + key + "=" + value + ".Previous value was" + oldValue);

                            }
                        }

                        corbaAddress.setOrbProperties(orbProp);

                    }
                }



                // Gets the endpoint data from the WSDL
                Binding binding = Jbi4CorbaExtensionUtils.getBinding(matchedDef, epDesc.getServiceName().toString(), epDesc.getEndpointName());
                Service service = Jbi4CorbaExtensionUtils.getService(matchedDef, epDesc.getServiceName().toString());
                PortType portType = Jbi4CorbaExtensionUtils.getPortType(matchedDef, epDesc.getServiceName().toString(),
                        epDesc.getEndpointName());
                String role = epDesc.isProvider() ? JbiServiceDescriptor.PROVIDER : JbiServiceDescriptor.CONSUMER;

                // creates the service descriptor

                JbiServiceDescriptor jbiServiceDescriptor = createServiceDescriptor(service,
                        binding,
                        portType,
                        corbaAddress,
                        corbaBinding,
                        role,
                        suZipPath);

                Jbi4CorbaSUInfo info = new Jbi4CorbaSUInfo(epDesc, matchedDef, matchedWSDL, jbiServiceDescriptor);
                infolist.add(info);
                descList.add(jbiServiceDescriptor);


            }
        }
        LOG.info("JBI_DESC------> "+descList.toString());
        return descList;

    }

    /**
     * Generate The Provider Endpoint
     * @param suZipPath the unzip path
     * @param epDesc the endpoint Identifier
     * @param jbiServiceDescriptor
     * @param rootDir
     *
     * @return the provider jbi4CorbaEndpoint
     * @throws IOException, DeploymentException if some deployment problem occurs
     */
    private Jbi4CorbaEndpoint generateProvider(String suZipPath, EndpointIdentifier epDesc,
            JbiServiceDescriptor jbiServiceDescriptors, ProviderServiceDescriptor serviceDescriptor, File rootDir)
            throws IOException, DeploymentException {



        final String endpointNameLocalPart = QName.valueOf(epDesc.getEndpointName()).getLocalPart();

        Jbi4CorbaEndpoint corbaEndpoint = null;

        ProviderServiceDescriptor providerServiceDescriptor = null;

        providerServiceDescriptor = serviceDescriptor;


        try {
            corbaEndpoint = new ProviderEndpoint(epDesc.getServiceName(), endpointNameLocalPart, providerServiceDescriptor);
        } catch (Jbi4CorbaException e1) {
            String msg = MESSAGES.getString("CRB000235_Fail_to_generate_corba_endpoint");
            LOG.error(msg, e1.getMessage());

            throw new DeploymentException(e1);
        }

        return corbaEndpoint;


    }

    /**
     * Refactoring
     * This Class Return All the provider service descriptor from  multiple interface idl
     * Creates the provider service descriptor.
     *
     * @param   suName                    The service unit name
     * @param   jbiServiceDescriptor      The service unit root path
     * @param   serviceClassesGenerator   The service classes generator
     * @param   serviceUnitRootPath       The service unit root path
     * @param   rootDir                   The root Dir
     *
     * @throws  ClassGenerationException  The class generation exception
     * @throws  DeploymentException       The deployment exception
     *
     * @return the provider service descriptor
     */
    private List<ProviderServiceDescriptor> getProviderServiceDescriptor(String suName,
            List<JbiServiceDescriptor> jbiServiceDescriptor,
            String serviceUnitRootPath,
            ProviderServiceClassesGenerator serviceClassesGenerator,
            File rootDir) throws ClassGenerationException, IOException,
            DeploymentException {

        List<ProviderServiceDescriptor> providerServiceDescriptorList = new ArrayList<ProviderServiceDescriptor>();
        //*************************CLASS GENERATION**************************************************
        for (int i=0;i<jbiServiceDescriptor.size();i++){
        jbiServiceDescriptor.get(i).setIdlFileNameDirectory(serviceUnitRootPath);
        }

        //*************************Produce the classes only on the First Deploy********************
        //If the classes produced by an idl exists its take its from hashatable

        List<ClientCorbaClassesHolder> classes = null;
        List<JbiServiceDescriptor> tmpJbidescList = new ArrayList<JbiServiceDescriptor>(jbiServiceDescriptor);
        String idl = null;
        for (int i = 0; i < jbiServiceDescriptor.size(); i++) {
            if (jbiServiceDescriptor.get(i).getRole().equals(JbiServiceDescriptor.PROVIDER)) {

                File idlFile = new File(jbiServiceDescriptor.get(i).getIdlFileNameDirectory() + File.separator +
                        jbiServiceDescriptor.get(i).getIdlFileName());

                idl = HelperFileUtil.readFileAsString(idlFile).trim();

                //if the classes is already generated takes it from hashtable
                //The classKey is the union of the SUname and the hashcode of the idl

                String classKey = suName + idl.hashCode();
                tmpJbidescList.remove(jbiServiceDescriptor.get(i));
                tmpJbidescList.add(0, jbiServiceDescriptor.get(i));
                if (classesTable.get(classKey) != null) {
                    classes = classesTable.get(classKey);

                } else {

                    classes = serviceClassesGenerator.generateProviderServiceClasses(
                            tmpJbidescList,
                            rootDir.getAbsolutePath(),
                            RuntimeContext.getInstance().getComponentContext().getInstallRoot(), null);
                    classesTable.put(classKey, classes);
                }


        //*********************************************************************
        //List<ProviderServiceDescriptor> providerServiceDescriptorList = new ArrayList<ProviderServiceDescriptor>();
        //There is a CorbaClassesHolder for each idl interface
        //If the idl not has more than one interface  classes.size() = 1




        //For Each class return the correct providerServiceDescriptor
        //Bind class to relative providerServiceDescriptor
        if (classes != null) {


            for (ClientCorbaClassesHolder corbaClasses : classes) {

                        //Associate the correct jbidescriptor if there are more than 1
                        int start=corbaClasses.getOperationsClass().getName().lastIndexOf(".")+1;
                        int end=corbaClasses.getOperationsClass().getName().lastIndexOf("Operations");
                        String portypeName=corbaClasses.getOperationsClass().getName().substring(start, end);
                        String nameSpace="http://"+corbaClasses.getOperationsClass().getName().substring(0, end);
                        if (jbiServiceDescriptor.get(i).getPortTypeName().getLocalPart().equals(portypeName) &&
                                jbiServiceDescriptor.get(i).getPortTypeName().getNamespaceURI().equals(nameSpace)
                                || classes.size() == 1) {
                            LOG.debug("Provider -->"+jbiServiceDescriptor.get(i).getPortTypeName());
                            LOG.debug("ClientCorbaClassesHolder=" + corbaClasses);

                            ProviderServiceDescriptor providerServiceDescriptor = new ProviderServiceDescriptor();


                            providerServiceDescriptor.setValueTypeIdAndInstance(
                                    corbaClasses.getValueTypeIdAndInstance());

                            LOG.debug(corbaClasses.getValueTypeIdAndInstance().toString());

                            providerServiceDescriptor.setMethodSignatures(
                                    corbaClasses.getMethodSignatures());

                            providerServiceDescriptor.setAllUniontypes(
                                    corbaClasses.getAllUnionTypesMap());

                            providerServiceDescriptor.setSubstitutedUnionFields(
                                    corbaClasses.getSubstitutedUnionFields());
                            //Take Metadata Relative to Interface Type
                            providerServiceDescriptor.setAllInterfacetypes(
                                    corbaClasses.getAllInterafceTypesMap());

                            LOG.debug("********************************** InterfaceType *****======>" + corbaClasses.getAllInterafceTypesMap());

                            providerServiceDescriptor.setSubstitutedInterfaceFields(
                                    corbaClasses.getSubstitutedInterfaceFields());

                            providerServiceDescriptor.setAllIDLTypes(corbaClasses.getAllIDLTypes());

                            providerServiceDescriptor.setCorbaEnumMap(corbaClasses.getCorbaEnumMap());

                            providerServiceDescriptor.setIdToClassNameMap(corbaClasses.getIdToClassMap());
                            
                            providerServiceDescriptor.setTypeDefs(corbaClasses.getTypeDefs());

                            providerServiceDescriptor.setServiceNameSpace(
                                    jbiServiceDescriptor.get(i).getServiceNameSpace());

                            //Set the portTypeName of the descriptor with the name f the generated Class
                            providerServiceDescriptor.setPortTypeName(new QName(portypeName));

                            providerServiceDescriptor.setServiceInterface(
                                    corbaClasses.getOperationsClass());
                            providerServiceDescriptor.setCorbaHelperClass(
                                    corbaClasses.getHelperClass());
                            providerServiceDescriptor.setWsdlRootDirectory(serviceUnitRootPath);
                            //*****************************************************************************
                            if (serviceClassesGenerator.getUrlClassLoader() != null) {
                                providerServiceDescriptor.setUrlClassLoader(
                                        serviceClassesGenerator.getUrlClassLoader());
                                tmpURLclassLoader = serviceClassesGenerator.getUrlClassLoader();
                            } else {
                                providerServiceDescriptor.setUrlClassLoader(tmpURLclassLoader);
                            }

                            LOG.debug("************ URL CLASS LOADER*******" + providerServiceDescriptor.getUrlClassLoader());


                            //*************Add the OriginalClassLoader For InvokeService********************

                            if (serviceClassesGenerator.getOriginalClassLoader() != null) {
                                providerServiceDescriptor.setOriginalClassLoader(
                                        serviceClassesGenerator.getOriginalClassLoader());
                                tmpOriginaclassLoader = serviceClassesGenerator.getOriginalClassLoader();
                            } else {
                                providerServiceDescriptor.setOriginalClassLoader(tmpOriginaclassLoader);
                            }

                            LOG.debug("************ORIGINAL CLASS LOADER*******" + providerServiceDescriptor.getOriginalClassLoader());

                            //*********************************************************************************
                            String serviceName = jbiServiceDescriptor.get(i).getServiceName();
                            LOG.debug("ServiceName: " + serviceName);
                            if (serviceName == null) {
                                LOG.error("CRB000206_No_ServiceName_found");
                                throw new DeploymentException(MESSAGES.getString(
                                        "CRB000206_No_ServiceName_found"));
                            }

                            providerServiceDescriptor.setServiceName(serviceName);
                            providerServiceDescriptor.setCorbaServiceName(
                                    jbiServiceDescriptor.get(i).getCorbaServiceName());

                            LOG.debug("Provider CORBA name:" + providerServiceDescriptor.getCorbaServiceName());

                            providerServiceDescriptor.setOrbProperties(
                                    jbiServiceDescriptor.get(i).getOrbProperties());
                            providerServiceDescriptor.setLocalizationType(
                                    jbiServiceDescriptor.get(i).getLocalizationType());

                            providerServiceDescriptorList.add(providerServiceDescriptor);

                        }
                    }

            }

        }
        }
        //Remove the idl for regenerate the class
        return providerServiceDescriptorList;

    }
}
