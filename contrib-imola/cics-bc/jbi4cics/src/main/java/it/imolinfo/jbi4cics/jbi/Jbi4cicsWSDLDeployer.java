/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.connection.jca.cics.CICSInteractionDescription;
import it.imolinfo.jbi4cics.exception.LocationException;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsAddress;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsBinding;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.security.J2CAccount;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.servicemix.common.AbstractDeployer;
import org.apache.servicemix.common.BaseComponent;
import org.apache.servicemix.common.Endpoint;
import org.apache.servicemix.common.ServiceUnit;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import com.ibm.wsdl.Constants;

/**
 * Deployer for the Jbi4cics JBI component. Deploys all the WSDL containing a
 * Jbi4Cics extension.
 *
 * @author marcopiraccini
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class Jbi4cicsWSDLDeployer extends AbstractDeployer {

    /**
     * Default for input/output bean class name.
     */
    private static final String IO_BEAN_NAME = "InputOutputBean";

    /**
     * Default input bean class name when input and output bean names differ.
     */
    private static final String INPUT_BEAN_NAME = "InputBean";

    /**
     * Default output bean class name when input and output bean names differ.
     */
    private static final String OUTPUT_BEAN_NAME = "OutputBean";

    /**
     * The task name, used for error messages.
     */
    private static final String TASK = "deploy";

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4cicsWSDLDeployer.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4cicsWSDLDeployer.class);

    /**
     * The deployable WSDL file filter.
     *
     * @see #getWSDL(String)
     */
    private static final FilenameFilter DEPLOYABLE_FILTER = new WsdlFilter();

    /**
     * Creates a <code>Jbi4cicsWSDLDeployer</code> for the specified component.
     *
     * @param  component  the component to be deployed by this instance.
     */
    public Jbi4cicsWSDLDeployer(final BaseComponent component) {
        super(component);
    }

    /**
     * Check if this deployer is able to handle a given artifact.
     *
     * @param   suName      the name of the service unit.
     * @param   suRootPath  the path of the exploded service unit.
     * @return  <code>true</code> if this deployer can handle the given
     *          artifact.
     */
    public boolean canDeploy(final String suName, final String suRootPath) {
        File[] wsdls = getWSDL(suRootPath);

        if (wsdls.length == 0) {
            LOG.info("CIC001032_No_WSDLs_file_found", suRootPath, suName);
            return false;
        }
        return true;
    }

    /**
     * Actually deploys the given service unit and build a ServiceUnit object
     * that contains endpoints.
     *
     * @param suName
     *            the name of the service unit
     * @param suRootPath
     *            the path of the exploded service unit
     *
     * @return a service unit containing endpoints
     *
     * @throws DeploymentException
     *             if an error occurs
     */
    public ServiceUnit deploy(final String suName, final String suRootPath)
            throws DeploymentException {
        File[] wsdls = getWSDL(suRootPath);
        ServiceUnit serviceUnit;
        List<Jbi4cicsEndpoint> endpoints = new ArrayList<Jbi4cicsEndpoint>();

        if (wsdls.length == 0) {
            throw failure(TASK,
                    MESSAGES.getString("CIC001033_No_valid_wsdl_found"), null);
        }

        // Creates the ServiceUnit
        serviceUnit = new ServiceUnit();
        serviceUnit.setComponent(component);
        serviceUnit.setName(suName);
        serviceUnit.setRootPath(suRootPath);

        // For each WSDL, add the Endpoits to the Service unit
        for (File wsdl : wsdls) {
            endpoints.addAll(getEndpointFromWsdl(wsdl));
        }
        if (endpoints.isEmpty()) {
            throw failure(TASK, MESSAGES.getString(
                    "CIC001034_Invalid_wsdl_no_valid_endpoints_found"), null);
        }

        for (Jbi4cicsEndpoint endpoint : endpoints) {
            endpoint.setServiceUnit(serviceUnit);
            try {
                endpoint.registerService();
            } catch (Exception e) {
                LOG.error("CIC001035_Could_not_register_endpoint", e);
                throw failure(TASK, MESSAGES.getString(
                        "CIC001035_Could_not_register_endpoint"), e);
            }
            serviceUnit.addEndpoint(endpoint);
        }
        return serviceUnit;
    }

    /**
     * Validates the specified endpoint. This implementation does nothing.
     *
     * @param   endpoint             the endpoint to validate.
     * @throws  DeploymentException  in general, if <code>endpoint</code> can't
     *                               be validated but this implementation never
     *                               throw this kind of exception.
     */
    @Override
    protected void validate(final Endpoint endpoint)
            throws DeploymentException {
    }

    /**
     * Return a <code>WSDLReader</code> and registers the
     * <code>Jbi4CicsExtension</code>.
     *
     * @return  a new <code>WSDLReader</code>.
     * @throws  WSDLException  No description provided.
     */
    private WSDLReader createJbi4CicsWsdlReader() throws WSDLException {
        WSDLFactory factory = WSDLFactory.newInstance();
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();
        WSDLReader reader = factory.newWSDLReader();

        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        Jbi4CicsExtension.register(registry);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Extension QName: "
                      + Jbi4CicsExtension.Q_ELEM_JBI4CICS_BINDING);
        }
        reader.setExtensionRegistry(registry);
        return reader;
    }

    /**
     * Reads the <code>Jbi4cicsEndpoint</code> list from the WSDL.
     *
     * @param   wsdl  the WSDL file. Must be not <code>null</code>.
     * @return  the loaded endpoint list, never empty.
     * @throws  DeploymentException  in case of errors.
     */
    public List<Jbi4cicsEndpoint> getEndpointFromWsdl(final File wsdl)
            throws DeploymentException {
        Document document = readXMLFile(wsdl);
        Definition definition;
        List<Jbi4cicsEndpoint> endpoints;

        try {
            definition = createJbi4CicsWsdlReader().readWSDL(null, document);
        } catch (WSDLException e) {
            throw createParseFailure(wsdl, e);
        }

        if (definition.getServices().isEmpty()) {
            throw failure(TASK, MESSAGES.getString(
                    "CIC001037_Invalid_wsdl_no_defined_services", wsdl), null);
        }

        endpoints = getJbi4CicsEndpoints(definition, document);
        if (endpoints.isEmpty()) {
            LOG.warn("CIC001038_No_EstensibilityElement_found_from_WSDL_file",
                     wsdl);
        }
        return endpoints;
    }

    /**
     * Reads the XML document from the specified file.
     *
     * @param   file  the file to read. Must be not <code>null</code>.
     * @return  the XML document read from the received file.
     * @throws  DeploymentException  if it's not possible to parse
     *                               <code>file</code> as a XML file.
     */
    private Document readXMLFile(File file) throws DeploymentException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

        factory.setNamespaceAware(true);
        try {
            return factory.newDocumentBuilder().parse(file);
        } catch (IOException e) {
            throw createParseFailure(file, e);
        } catch (ParserConfigurationException e) {
            throw createParseFailure(file, e);
        } catch (SAXException e) {
            throw createParseFailure(file, e);
        }
    }

    /**
     * Creates a new <code>DeploymentException</code> caused by the specified
     * error obtained while parsing the indicated file. This method also do a
     * log of the received exception.
     *
     * @param   file   the file that caused the error during its parsing. Must
     *                 be not <code>null</code>.
     * @param   cause  the original error obtained while parsing the file
     *                 <code>file</code>. Must be not <code>null</code>.
     * @return  a newly constructed <code>DeploymentException</code>, obtained
     *          from parameters passed to this method.
     */
    private DeploymentException createParseFailure(File file, Exception cause) {
        String errorMsgKey = "CIC001036_Could_not_parse";
        String errorMsg = MESSAGES.getString(errorMsgKey, file);

        LOG.error(errorMsgKey, new Object[] { file }, cause);
        return failure(TASK, errorMsg, cause);
    }

    /**
     * Collects all <code>Jbi4cicsEndpoint</code> reading from the specified
     * WSDL.
     *
     * @param   definition  the WSDL to read. Must be not <code>null</code>.
     * @param   document    the XML representation of <code>definition</code>.
     *                      Must be not <code>null</code>.
     * @return  the list of all Cics endpoints read from the specified WSDL
     *          document. The returned list is never <code>null</code>.
     * @throws  DeploymentException  if the operation number read from the
     *                               binding differs from 1.
     */
    private List<Jbi4cicsEndpoint> getJbi4CicsEndpoints(Definition definition,
            Document document) throws DeploymentException {
        List<Jbi4cicsEndpoint> endpoints = new ArrayList<Jbi4cicsEndpoint>();

        // For each service, look for a extended bindings
        for (Object i : definition.getServices().values()) {
            Service svc = (Service) i;

            for (Object j : svc.getPorts().values()) {
                Port port = (Port) j;

                for (Object k : port.getExtensibilityElements()) {

                    // If the address extension element is found, look for
                    // binding extension
                    if (k instanceof Jbi4CicsAddress) {
                        Jbi4CicsAddress cicsAddress = (Jbi4CicsAddress) k;
                        Binding binding = port.getBinding();

                        // Reads the estensibility element of the binding
                        for (Object l : binding.getExtensibilityElements()) {
                            if (l instanceof Jbi4CicsBinding) {

                                // We found a binding estensibility element
                                Jbi4cicsEndpoint endpoint
                                        = createServiceDescriptor(svc, port,
                                                binding, cicsAddress,
                                                (Jbi4CicsBinding) l);

                                endpoint.setDescription(document);
                                endpoint.setDefinition(definition);
                                endpoints.add(endpoint);
                            }
                        }
                    }
                }
            }
        }
        return endpoints;
    }

    /**
     * Creates the <code>Jbi4cicsEndpoint</code> from the extended
     * WSDL-element.
     *
     * @param   service              the WSDL service.
     * @param   port                 the WSDL port.
     * @param   binding              the WSDL binding.
     * @param   addressExtension     the Jbi4Cics address extension.
     * @param   bindingExtension     the Jbi4Cics binding extension.
     * @return  the new <code>Jbi4cicsEndpoint</code> created from the extended
     *          WSDL-element.
     * @throws  DeploymentException  if the operation number read from the
     *                               binding differs from 1.
     */
    protected Jbi4cicsEndpoint createServiceDescriptor(final Service service,
            final Port port, final Binding binding,
            final Jbi4CicsAddress addressExtension,
            final Jbi4CicsBinding bindingExtension) throws DeploymentException {
        ServiceDescriptor serviceDescriptor = new ServiceDescriptor();
        Jbi4cicsEndpoint endpoint = new Jbi4cicsEndpoint();
        List bindingOperations = binding.getBindingOperations();
        CICSInteractionDescription cicsDescription
                = new CICSInteractionDescription();
        J2CAccount account = new J2CAccount();
        SimpleLocation location = new SimpleLocation();
        Boolean sameCopyCobol = bindingExtension.getSameCopyCobol();

        // CopyCobol and Code Page
        endpoint.setCopyCobol(bindingExtension.getCopyCobol());
        endpoint.setCodePage(bindingExtension.getCodePage());

        // ServiceName and ServiceNamespace
        serviceDescriptor.setServiceName(service.getQName().getLocalPart());
        serviceDescriptor.setServiceNameSpace(
                service.getQName().getNamespaceURI());

        // Gets the FIRST operation of the binding.
        // If more than one operation is found an exception is thrown.
        if (bindingOperations.size() != 1) {
            throw new DeploymentException(MESSAGES.getString("CIC001039_More_"
                        + "than_one_operation_find_in_extension_binding"));
        }
        serviceDescriptor.setOperationName(
                ((BindingOperation) bindingOperations.get(0)).getName());
        serviceDescriptor.setServiceInterfacePackageName(
                bindingExtension.getServicePackageName());

        serviceDescriptor.setServiceInterfaceName(
                binding.getPortType().getQName().getLocalPart());

        // Back compatibility: default is same copy Cobol for input and output
        if ((sameCopyCobol == null) || Boolean.FALSE.equals(sameCopyCobol)) {
            serviceDescriptor.setInputBeanClassName(IO_BEAN_NAME);
            serviceDescriptor.setOutputBeanClassName(IO_BEAN_NAME);
        } else {
            serviceDescriptor.setInputBeanClassName(INPUT_BEAN_NAME);
            serviceDescriptor.setOutputBeanClassName(OUTPUT_BEAN_NAME);
        }

        // Account
        account.setUsername(addressExtension.getUsername());
        account.setPassword(addressExtension.getPassword());
        serviceDescriptor.setAccount(account);

        // Location
        try {
            location.setConnectionTypeName(
                    addressExtension.getConnectionType());
        } catch (LocationException e) {
            String msg = e.getLocalizedMessage();

            LOG.error("CIC001040_Error_loading_location_type",
                      new Object[] { msg }, e);
            throw new DeploymentException(MESSAGES.getString(
                    "CIC001040_Error_loading_location_type", msg), e);
        }
        location.setLocationName(addressExtension.getJNDIConnectionName());
        serviceDescriptor.setServiceLocation(location);

        cicsDescription.setProgramName(addressExtension.getProgramName());
        cicsDescription.setTpn(addressExtension.getTpn());
        cicsDescription.setTransactionName(
                addressExtension.getTransactionName());

        serviceDescriptor.setInteractionDescription(cicsDescription);
        endpoint.setServiceDescriptor(serviceDescriptor);
        return endpoint;
    }

    /**
     * Returns all the deployable WSDL files contained in the specified
     * directory or in all first-level childs.
     *
     * @param   suRootPath  the service unit root path. Must be not
     *                      <code>null</code>.
     * @return  all the deployable WSDL files contained in the directory named
     *          by <code>serviceUnitRootPath</code> and inside its first-level
     *          subdirectories.
     */
    private File[] getWSDL(final String suRootPath) {
        File suRoot = new File(suRootPath);
        File[] firstLevelWsdls = suRoot.listFiles(DEPLOYABLE_FILTER);
        File[] firstLevelDirs = suRoot.listFiles(DirectoryFilter.THE_INSTANCE);
        List<File> files = new ArrayList<File>();
        boolean debug = LOG.isDebugEnabled();

        files.addAll(Arrays.asList(firstLevelWsdls));
        for (File dir : firstLevelDirs) {
            File[] secondLevelWsdls = dir.listFiles(DEPLOYABLE_FILTER);

            if (debug) {
                LOG.debug("Found directory: " + dir.getName());
            }
            files.addAll(Arrays.asList(secondLevelWsdls));
        }
        return files.toArray(new File[files.size()]);
    }


    /**
     * WSDL file filter class.
     */
    private static final class WsdlFilter implements FilenameFilter {

        /**
         * Prepares a new instance of this class.
         */
        private WsdlFilter() {
        }

        /**
         * Tests if a specified file is a WSDL file and contains the Jbi4Cics
         * extension namespace.
         *
         * @param   dir    the directory in which the file was found.
         * @param   name   the name of the file.
         * @return  <code>true</code> if and only if the file name ends with
         *          <i>.WSDL</i> and the file contains the Jbi4Cics extension
         *          namespace; <code>false</code> otherwise.
         */
        public boolean accept(final File dir, final String name) {
            boolean isWSDL = name.toUpperCase().endsWith(".WSDL");

            if (isWSDL) {
                try {
                    WSDLFactory factory = WSDLFactory.newInstance();
                    WSDLReader reader = factory.newWSDLReader();
                    Definition def;

                    reader.setFeature(Constants.FEATURE_VERBOSE, false);
                    reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
                    def = reader.readWSDL(dir.getAbsolutePath(), name);
                    if (containsExtensionNamespace(def)) {
                        if (LOG.isDebugEnabled()) {
                            LOG.debug("Found namespace "
                                      + Jbi4CicsExtension.NS_URI_JBI4CICS
                                      + " in file " + name);
                        }
                        return true;
                    }
                } catch (WSDLException e) {
                    Object[] args = new Object[] {
                            name, Jbi4CicsExtension.NS_URI_JBI4CICS };

                    LOG.warn("CIC001041_File_doesnt_contain_the_estension_"
                             + "jbi4corba", args, e);
                }
            }
            return false;
        }

        /**
         * Tests if the definition contains the
         * <code>Jbi4CicsExtension.NS_URI_JBI4CICS</code> namespace.
         *
         * @param   definition  the definition to check.
         * @return  <code>true</code> if the definition contains the
         *          <code>Jbi4CicsExtension.NS_URI_JBI4CICS</code> namespace.
         */
        private static boolean containsExtensionNamespace(
                final Definition definition) {
            for (Object obj : definition.getNamespaces().values()) {
                String namespace = (String) obj;

                if (namespace.equalsIgnoreCase(
                        Jbi4CicsExtension.NS_URI_JBI4CICS)) {
                    return true;
                }
            }
            return false;
        }
    }


    /**
     * Accept directories that are NOT "META-INF".
     *
     * @author marco
     */
    private static final class DirectoryFilter implements FileFilter {

        /**
         * An instance of this class. This class may be used as a
         * <i>singleton</i>, so this field can be accessed by the outer class.
         */
        static final DirectoryFilter THE_INSTANCE = new DirectoryFilter();

        /**
         * Constructs a new instance of this class.
         */
        private DirectoryFilter() {
        }

        /**
         * Accepts only directories not called <i>META-INF</i>, <i>META-INF</i>
         * and so on considering case.
         *
         * @param   file  the file to test.
         * @return  <code>true</code> if and only if <code>file</code> is a
         *          directory and its name is not <i>META-INF</i>, ignoring
         *          case.
         */
        public boolean accept(final File file) {
            return file.isDirectory()
                   && !(file.getName().equalsIgnoreCase("META-INF"));
        }
    }
}
