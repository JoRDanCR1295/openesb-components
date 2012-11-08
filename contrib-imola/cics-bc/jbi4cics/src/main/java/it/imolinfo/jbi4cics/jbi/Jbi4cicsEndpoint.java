/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.exception.Jbi4cicsException;
import it.imolinfo.jbi4cics.exception.ParseException;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.util.Collection;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.servicemix.common.ExchangeProcessor;
import org.apache.servicemix.common.endpoints.ProviderEndpoint;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.service.Service;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import antlr.ANTLRException;

public final class Jbi4cicsEndpoint extends ProviderEndpoint {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4cicsEndpoint.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4cicsEndpoint.class);

    /**
     * The code page.
     */
    private String codePage;

    /**
     * The input copy Cobol file name.
     */
    private String copyCobolFileName;

    /**
     * The output copy Cobol file name.
     */
    private String outputCopyCobolFileName;

    /**
     * The input copy Cobol, used also for output if {@link #outputCopyCobol} is
     * <code>null</code> or empty.
     */
    private String copyCobol;

    /**
     * The output copy Cobol.
     */
    private String outputCopyCobol;

    /**
     * The service descriptor.
     */
    private ServiceDescriptor serviceDescriptor;

    /**
     * The XFire service.
     */
    private Service xfireService;

    private ExchangeProcessor processor = new Jbi4cicsExchangeProcessor(this);

    /**
     * Initializes a new instance of this class.
     */
    public Jbi4cicsEndpoint() {
    }

    /**
     * Returns the xfire <code>Service</code>.
     *
     * @return the xfire <code>Service</code>.
     */
    public Service getXFireService() {
        return xfireService;
    }

    /**
     * Starts the <code>ExchangeProcessor</code>.
     *
     * @throws  Exception  if caused by the call to <code>super.start()</code>
     *                     or the exchange processor startup.
     */
    @Override
    public void start() throws Exception {                      // Overridden
        super.start();
        processor.start();
    }

    /**
     * Stops the <code>ExchangeProcessor</code>.
     *
     * @throws  Exception  if caused by the call to <code>super.stop()</code>
     *                     or the exchange processor stop.
     */
    @Override
    public void stop() throws Exception {                       // Overridden
        super.stop();
        processor.stop();
    }

    /**
     * Registers the service.
     */
    void registerService() throws Jbi4cicsException, IOException,
            ANTLRException, SAXException, ParserConfigurationException,
            WSDLException, TransformerException {
        XFire xfire = getXFire();

        createServiceBeansAndInterface();
        if (definition != null) {
            Port port;

            // Gets service/endpoint from the WSDL definition
            LOG.debug("Using WSDL definition to load endpoint data");

            // Gets the endpoint
            service = new QName(serviceDescriptor.getServiceNameSpace(),
                                serviceDescriptor.getServiceName());
            port = initEndpointIfIsNull(definition.getService(service));
            checkEndpointIsNotNull();

            // XXX What if 'port' is null? Look at initEndpointIfIsNull()...
            interfaceName = port.getBinding().getPortType().getQName();

            if (LOG.isDebugEnabled()) {
                LOG.debug("Loaded endpoint data, service: " + service
                          + ", endpoint: " + endpoint + ", interface:"
                          + interfaceName);
            }
        }

        // Create and register the xfire service
        xfireService = new ServiceCreator().createJbiService(
                serviceDescriptor, xfire, interfaceName);
        xfire.getServiceRegistry().register(xfireService);

        if (definition == null) {
            QName serviceQName;
            QName interfaceQName;
            Definition def;
            javax.wsdl.Service wsdlService;

            // Check service name, endpoint name ad description from xfire
            description = generateWsdl();
            serviceQName = xfireService.getName();
            interfaceQName = xfireService.getServiceInfo().getPortType();
            if (service == null) {
                service = serviceQName;
            } else if (!service.equals(serviceQName)) {
                LOG.warn("CIC001018_Service_name_not_matched", serviceQName,
                         service);
            }
            if (interfaceName == null) {
                interfaceName = interfaceQName;
            } else if (!interfaceName.equals(interfaceQName)) {
                LOG.warn("CIC001019_Interface_name_not_matched", interfaceQName,
                         interfaceName);
            }
            def = WSDLFactory.newInstance().newWSDLReader().readWSDL(
                    null, description);
            wsdlService = def.getService(serviceQName);
            if (wsdlService != null) {
                initEndpointIfIsNull(wsdlService);
            }
            checkEndpointIsNotNull();
        }

        writeWsdlToFile();              // Writes out the description on file
    }

    /**
     * Verifies and updates {@link #serviceDescriptor} creating the input bean,
     * the output bean and the service interface.
     *
     * @throws  Jbi4cicsException  if the service name stored in the service
     *                             descriptor is <code>null</code>.
     * @throws  ANTLRException     if an error occurs while parsing the
     *                             copy/copies Cobol.
     */
    private void createServiceBeansAndInterface()
            throws Jbi4cicsException, ANTLRException {
        CommareaBeanMappingDescriptor descriptor;
        BCELClassLoader classLoader = ((Jbi4cicsLifeCycle)
                serviceUnit.getComponent().getLifeCycle()).getBCELClassLoader();
        ServiceBeanGenerator beanGenerator;
        ServiceInterfaceGenerator interfGenerator;

        // Set the defaults
        if (serviceDescriptor.getServiceName() == null) {
            throw new Jbi4cicsException(
                    "CIC001013_Service_name_not_initialized");
        }
        serviceDescriptor.setDefaultValuesIfNecessary();

        // Generate the mapping descriptor from copy/copies Cobol. If only
        // 'copyCobol' is found, the same CommareaBeanMappingDescriptor is used
        // for both the input and the output step
        descriptor = parseCommarea(copyCobol);
        serviceDescriptor.setInputMappingDescriptor(descriptor);
        if ((outputCopyCobol != null) && (outputCopyCobol.length() > 0)
                && !outputCopyCobol.equals(copyCobol)) {
            descriptor = parseCommarea(outputCopyCobol);
        }
        serviceDescriptor.setOutputMappingDescriptor(descriptor);

        // Creates the service inputBean
        beanGenerator = new ServiceBeanGenerator(serviceDescriptor, true);
        beanGenerator.generateBeanClass(classLoader);

        // Creates the service outputBean
        beanGenerator = new ServiceBeanGenerator(serviceDescriptor, false);
        beanGenerator.generateBeanClass(classLoader);

        // Creates the service interface
        interfGenerator = new ServiceInterfaceGenerator(serviceDescriptor);
        interfGenerator.generateServiceInterface(classLoader);

        // FIXME questa gestione del codepage non e' elegante
        serviceDescriptor.setCodePage(codePage);
    }

    /**
     * Parses the specified commarea.
     *
     * @param   cpyCobol         the copy Cobol describing the commarea to
     *                           parse. Must be not <code>null</code>.
     * @return  the descriptor related to the received commarea.
     * @throws  ANTLRException   if thrown by the ANTLR lexer or parser.
     * @throws  FormatException  if a field contained in the commarea has a
     *                           wrong format.
     * @throws  ParseException   if a field type is not supported or contains
     *                           erroneous value(s).
     */
    CommareaBeanMappingDescriptor parseCommarea(final String cpyCobol)
            throws ANTLRException, FormatException, ParseException {
        CommareaLexer lexer = new CommareaLexer(new StringReader(copyCobol));
        CommareaParser parser = new CommareaParser(lexer);
        CommareaBeanMappingDescriptor desc = parser.commarea_definition();

        LOG.info("CIC001014_Commarea_parsed", getEndpoint(), desc);
        return desc;
    }

    /**
     * Tries to set the <code>enpoint</code> field value if is currently
     * <code>null</code>, getting its value from the specified WSDL service.
     *
     * @param   wsdlService  the WSDL service to get the endpoint name. Must be
     *                       not <code>null</code>.
     * @return  the port used to obtain the endpoint name, even if the endpoint
     *          was also initialized and has not been modified. May be
     *          <code>null</code>, in which case the <code>endpoint</code> field
     *          has been left unchanged.
     */
    private Port initEndpointIfIsNull(javax.wsdl.Service wsdlService) {
        Collection ports = wsdlService.getPorts().values();
        Port port = null;

        if (ports.size() == 1) {

            // Check if this is the same as defined in endpoint spec
            port = (Port) ports.iterator().next();
            if (endpoint == null) {
                endpoint = port.getName();
            } else if (!endpoint.equals(port.getName())) {
                LOG.warn("CIC001016_Endpoint_name_not_matched", port.getName(),
                         endpoint);
            }
        }
        return port;
    }

    /**
     * Verifies that the current endpoint value is not <code>null</code>.
     *
     * @throws  IllegalArgumentException  if the stored endpoint is
     *                                    <code>null</code>.
     */
    private void checkEndpointIsNotNull() {
        if (endpoint == null) {
            throw new IllegalArgumentException(MESSAGES.getString(
                    "CIC001017_Endpoint_name_not_provided"));
        }
    }

    private Document generateWsdl() throws SAXException, IOException,
            ParserConfigurationException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream(4096);
        DocumentBuilderFactory factory =
            DocumentBuilderFactory.newInstance();

        getXFire().generateWSDL(xfireService.getSimpleName(), baos);
        if (LOG.isDebugEnabled()) {
            LOG.debug(baos.toString());
        }
        factory.setNamespaceAware(true);
        return factory.newDocumentBuilder().parse(
                new ByteArrayInputStream(baos.toByteArray()));
    }

    /**
     * Write the generated WSDL to a file.
     *
     * @throws  IOException                   if the named file exists but is a
     *                                        directory rather than a regular
     *                                        file, does not exist but cannot be
     *                                        created, or cannot be opened for
     *                                        any other reason.
     * @throws  TransformerException          if an unrecoverable error occurs
     *                                        during the course of the XML
     *                                        transformation.
     * @throws  SAXException                  if any parse errors occur.
     * @throws  ParserConfigurationException  if a <code>javax.xml.parsers.DocumentBuilder</code>
     *                                        cannot be created which satisfies
     *                                        the configuration requested.
     */
    private void writeWsdlToFile() throws IOException, TransformerException,
            SAXException, ParserConfigurationException {
        String wsdlFileName = getServiceUnit().getRootPath() + "/"
                              + xfireService.getSimpleName() + ".wsdl";
        FileWriter file;

        LOG.info("CIC001020_Producing_wsdl", wsdlFileName);
        file = new FileWriter(wsdlFileName);
        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transormer = factory.newTransformer();

            transormer.transform(new DOMSource(generateWsdl()),
                                 new StreamResult(file));
        } finally {
            file.close();
        }
    }

    XFire getXFire() {
        Jbi4cicsLifeCycle lifeCycle
                = (Jbi4cicsLifeCycle) serviceUnit.getComponent().getLifeCycle();

        return lifeCycle.getXFire();
    }

    /**
     * Returns the exchange processor.
     *
     * @return  the exchange processor.
     */
    @Override
    public ExchangeProcessor getProcessor() {                   // Overridden
        return processor;
    }

    /**
     * Returns the input copy Cobol.
     *
     * @return  the input copy Cobol.
     */
    public String getCopyCobol() {
        return copyCobol;
    }

    /**
     * Sets the input copy Cobol.
     *
     * @param copyCobol  the input copy Cobol to set.
     */
    public void setCopyCobol(final String copyCobol) {
        this.copyCobol = copyCobol;
    }

    /**
     * Returns the output copy Cobol.
     *
     * @return  the output copy Cobol.
     */
    public String getOutputCopyCobol() {
        return outputCopyCobol;
    }

    /**
     * Sets the output copy Cobol.
     *
     * @param  outputCopyCobol  the output copy Cobol to set.
     */
    public void setOutputCopyCobol(final String outputCopyCobol) {
        this.outputCopyCobol = outputCopyCobol;
    }

    /**
     * Returns the service descriptor.
     *
     * @return the service descriptor.
     */
    public ServiceDescriptor getServiceDescriptor() {
        return serviceDescriptor;
    }

    /**
     * Sets the service descriptor.
     *
     * @param  serviceDescriptor  the service descriptor to set.
     */
    public void setServiceDescriptor(final ServiceDescriptor serviceDescriptor) {
        this.serviceDescriptor = serviceDescriptor;
    }

    /**
     * Returns the input copy Cobol file name.
     *
     * @return  the input copy Cobol file name.
     */
    public String getCopyCobolFileName() {
        return copyCobolFileName;
    }

    /**
     * Sets the input copy Cobol file name.
     *
     * @param  copyCobolFileName  the input copy Cobol file name to set.
     */
    public void setCopyCobolFileName(final String copyCobolFileName) {
        this.copyCobolFileName = copyCobolFileName;
    }

    /**
     * Returns the output copy Cobol file name.
     *
     * @return  the output copy Cobol file name.
     */
    public String getOutputCopyCobolFileName() {
        return outputCopyCobolFileName;
    }

    /**
     * Sets the output copy Cobol file name.
     *
     * @param  outputCopyCobolFileName  the output copy Cobol file name to set.
     */
    public void setOutputCopyCobolFileName(
            final String outputCopyCobolFileName) {
        this.outputCopyCobolFileName = outputCopyCobolFileName;
    }

    /**
     * Returns the code page.
     *
     * @return the code page.
     */
    public String getCodePage() {
        return codePage;
    }

    /**
     * Sets the code page.
     *
     * @param  codePage  the code page to set.
     */
    public void setCodePage(final String codePage) {
        this.codePage = codePage;
    }
}
