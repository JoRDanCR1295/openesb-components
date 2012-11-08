/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.webservices.runtime;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.connection.jca.cics.CICSInteractionDescription;
import it.imolinfo.jbi4cics.exception.Jbi4cicsException;
import it.imolinfo.jbi4cics.exception.LocationException;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsAddress;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsBinding;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension;
import it.imolinfo.jbi4cics.jbi.xfire.JbiTransport;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.security.Account;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import org.codehaus.xfire.DefaultXFire;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.aegis.AegisBindingProvider;
import org.codehaus.xfire.aegis.type.TypeMapping;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.service.binding.ObjectServiceFactory;
import org.codehaus.xfire.soap.SoapConstants;
import org.codehaus.xfire.transport.Transport;
import org.codehaus.xfire.transport.TransportManager;
import org.codehaus.xfire.wsdl.AbstractWSDL;
import org.xml.sax.InputSource;

/**
 * Helper class to create XFire service starting from {@link ServiceDescriptor}.
 */
public final class ServiceCreator {

    /**
     * Initial buffer size, expressed in bytes.
     */
    private static final int BUFFER_SIZE = 4096;

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(ServiceCreator.class);

    /**
     * Creates an instance of this class.
     */
    public ServiceCreator() {
    }

    /**
     * Creates the XFire service from specified parameters.
     *
     * @param   desc   the service descriptor.
     * @param   xfire  the XFire instance to use for creation.
     * @return  the newly created XFire service.
     */
    public Service createService(final ServiceDescriptor desc,
                                 final XFire xfire) {
        QName interfaceName = new QName(desc.getServiceNameSpace(),
                                        desc.getServiceInterfaceName());

        return doCreateService(desc, xfire, interfaceName, false);
    }

    /**
     * Creates the JBI XFire service from specified parameters.
     *
     * @param   desc           the service descriptor.
     * @param   xfire          the XFire instance to use for creation.
     * @param   interfaceName  the interface qualified name.
     * @return  the newly created JBI XFire service.
     */
    public Service createJbiService(final ServiceDescriptor desc,
            final XFire xfire, final QName interfaceName) {
        return doCreateService(desc, xfire, interfaceName, true);
    }

    /**
     * Creates the XFire service from specified parameters.
     *
     * @param   desc           the service descriptor.
     * @param   xfire          the XFire instance to use for creation.
     * @param   interfaceName  the interface qualified name.
     * @param   isJbiService   <code>true</code> to create a JBI service (i.e.
     *                         under a JBI container), <code>false</code>
     *                         otherwise (outside a JBI environment).
     * @return  the newly created XFire service.
     */
    private Service doCreateService(final ServiceDescriptor desc,
            final XFire xfire, final QName interfaceName,
            final boolean isJbiService) {
        ObjectServiceFactory factory
                = new ObjectServiceFactory(xfire.getTransportManager());
        AegisBindingProvider bindingProvider
                = (AegisBindingProvider) factory.getBindingProvider();
        Map<String, Object> props = null;
        Service service;
        TypeMapping typeMapping;

        if (isJbiService) {
            props = new HashMap<String, Object>();
            props.put(ObjectServiceFactory.PORT_TYPE, interfaceName);
            props.put(ObjectServiceFactory.STYLE, SoapConstants.STYLE_WRAPPED);
            props.put(ObjectServiceFactory.USE, SoapConstants.USE_LITERAL);

            factory.getSoap12Transports().clear();
            factory.getSoap11Transports().clear();
            factory.getSoap11Transports().add(JbiTransport.JBI_BINDING);
        }

        service = factory.create(desc.getServiceInterface(),
                desc.getServiceName(),  desc.getServiceNameSpace(), props);
        service.setInvoker(new ServiceInvoker(desc));

        // Adds the import in WSDL schema
        service.setProperty(AbstractWSDL.GENERATE_IMPORTS, "true");

        typeMapping = bindingProvider.getTypeMapping(service);
        typeMapping.register(new BigIntegerType());
        typeMapping.register(new BigDecimalType());

        return service;
    }

    /**
     * Creates a WSDL object from specified copy Cobol. This method may be used
     * to generate a WSDL (file) starting from a CPY file, for example.
     *
     * @param   copyCobol          the copy Cobol to generates the WSDL. Must be
     *                             not <code>null</code>.
     * @param   outputCopyCobol    the optional output copy Cobol to generates
     *                             the WSDL. If <code>null</code> or blank, is
     *                             like to not specify this parameter, so only
     *                             <code>copyCobol</code> will be considered and
     *                             will be used for input and for output during
     *                             CICS call.
     * @param   desc               the WSDL descriptor. Must be not
     *                             <code>null</code>.
     * @return  the WSDL document corresponding to the copy Cobol received.
     * @throws  Jbi4cicsException  in case of errors that prevent WSDL creation.
     */
    public Definition createWsdlFromCopyCobol(final String copyCobol,
            final String outputCopyCobol, final ServiceDescriptor desc)
            throws Jbi4cicsException {
        BCELClassLoader loader
                = new BCELClassLoader(getClass().getClassLoader());
        Service service;
        ByteArrayOutputStream buffer = new ByteArrayOutputStream(BUFFER_SIZE);

        // Creates service input bean
        new ServiceBeanGenerator(desc, true).generateBeanClass(loader);

        // Creates service output bean
        new ServiceBeanGenerator(desc, false).generateBeanClass(loader);

        // Creates the service interface
        new ServiceInterfaceGenerator(desc).generateServiceInterface(loader);

        // Creates the service
        service = createJbiService(desc, createXFire(null), new QName(
                desc.getServiceNameSpace(), desc.getServiceInterfaceName()));

        // Modifies the service adding CICS elements
        try {
            service.getWSDLWriter().write(buffer);
            return bindToCics(new ByteArrayInputStream(buffer.toByteArray()),
                              copyCobol, outputCopyCobol, desc);
        } catch (WSDLException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw new Jbi4cicsException(e.getMessage(), e);
        } catch (IOException e) {
            LOG.error(e.getLocalizedMessage(), e);
            throw new Jbi4cicsException(e.getMessage(), e);
        }
    }

    // Leave this method here and don't put it inside Jbi4cicsLifeCycle class,
    // because that class uses ServiceMix classes and will cause another library
    // to be added to Netbeans plugin
    /**
     * Creates a new XFire instance.
     *
     * @param   componentContext  the JBI component context. May be
     *                            <code>null</code>.
     * @return  the newly created XFire instance, with a JBI transport related
     *          to <code>componentContext</code>.
     */
    public static XFire createXFire(final ComponentContext componentContext) {
        XFire xfire = new DefaultXFire();
        TransportManager manager = xfire.getTransportManager();

        // Iterates over the array instead of the Collection to avoid
        // java.util.ConcurrentModificationException
        for (Object o : manager.getTransports().toArray()) {
            manager.unregister((Transport) o);
        }
        manager.register(new JbiTransport(componentContext));
        return xfire;
    }

    private Definition bindToCics(final InputStream wsdl,
            final String copyCobol, final String outputCopyCobol,
            final ServiceDescriptor desc)
            throws WSDLException, IOException, LocationException {
        WSDLFactory factory = WSDLFactory.newInstance();
        WSDLReader reader = factory.newWSDLReader();
        ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();
        Definition def;
        javax.wsdl.Service service;
        Port port;
        Binding binding;

        Jbi4CicsExtension.register(registry);
        reader.setExtensionRegistry(registry);
        def = reader.readWSDL(null, new InputSource(wsdl));
        def.setExtensionRegistry(registry);
        def.addNamespace(Jbi4CicsExtension.DEFAULT_PREFIX,
                         Jbi4CicsExtension.NS_URI_JBI4CICS);

        service = def.getService(new QName(desc.getServiceNameSpace(),
                                           desc.getServiceName()));

        // Remove all ports
        service.getPorts().clear();

        // Adds the port
        port = def.createPort();
        port.setName(desc.getServiceName() + "CicsPort");
        service.addPort(port);

        // Adds the extended address
        port.addExtensibilityElement(createJbi4CicsAddressElement(desc));

        // Adds the binding, using an existing port type
        binding = def.createBinding();
        binding.setUndefined(false);
        binding.setQName(new QName(desc.getServiceNameSpace(),
                                   desc.getServiceName() + "CicsBinding"));
        binding.setPortType(def.getPortType(new QName(
                desc.getServiceNameSpace(), desc.getServiceInterfaceName())));
        port.setBinding(binding);
        def.addBinding(binding);
        removeWsdlSoapElements(def, binding, desc);

        // Adds the extended binding
        binding.addExtensibilityElement(
                createJbi4CicsBindingElement(copyCobol, outputCopyCobol, desc));

        return def;
    }

    /**
     * Creates a new <code>Jbi4CicsAddress</code> element from the specified
     * service descriptor.
     *
     * @param   desc  the service descriptor.
     * @return  the newly created Cics address element, representing the service
     *          described by <code>desc</code>.
     * @throws  LocationException  if the CICS connection type contained in the
     *                             service descriptor is not valid.
     */
    private static Jbi4CicsAddress createJbi4CicsAddressElement(
            final ServiceDescriptor desc) throws LocationException {
        Jbi4CicsAddress addr = new Jbi4CicsAddress();
        Account account = desc.getAccount();

        // XXX Now we use only SimpleLocation, but in the future...
        SimpleLocation location = (SimpleLocation) desc.getServiceLocation();

        // XXX InteractionDescription has no methods: design problem?
        CICSInteractionDescription interactionDesc
                = (CICSInteractionDescription) desc.getInteractionDescription();

        addr.setElementType(Jbi4CicsExtension.Q_ELEM_JBI4CICS_ADDRESS);
        addr.setUsername(account.getUsername());
        addr.setPassword(account.getPassword());
        addr.setConnectionType(location.getConnectionTypeName());
        addr.setJNDIConnectionName(location.getLocationName());
        addr.setProgramName(interactionDesc.getProgramName());
        addr.setTransactionName(interactionDesc.getTransactionName());
        addr.setTpn(Boolean.valueOf(interactionDesc.isTpn()));
        return addr;
    }

    /**
     * Removes any SOAP element from the specified WSDL service.
     *
     * @param  def      the WSDL definition.
     * @param  binding  the new CICS binding added to <code>def</code>.
     * @param  desc     the service descriptor used to create the WSDL
     *                  definition <code>def</code>.
     */
    private static void removeWsdlSoapElements(final Definition def,
            final Binding binding, final ServiceDescriptor desc) {
        QName oldBindingName = new QName(desc.getServiceNameSpace(),
                                         desc.getServiceName() + "JBIBinding");

        for (Object o : def.getBinding(oldBindingName).getBindingOperations()) {
            BindingOperation bindingOp = (BindingOperation) o;
            Operation operation = bindingOp.getOperation();
            BindingInput input = bindingOp.getBindingInput();
            BindingOutput output = bindingOp.getBindingOutput();

            // Clones the <wsdl:operation> element to remove inner elements
            // contained in the "wsdlsoap" namespace
            BindingOperation newBindingOp = def.createBindingOperation();
            Operation newOperation = def.createOperation();
            BindingInput newInput = def.createBindingInput();
            BindingOutput newOutput = def.createBindingOutput();

            newBindingOp.setName(bindingOp.getName());
            newOperation.setName(operation.getName());
            newInput.setName(input.getName());
            newOutput.setName(output.getName());
            newBindingOp.setOperation(newOperation);
            newBindingOp.setBindingInput(newInput);
            newBindingOp.setBindingOutput(newOutput);
            for (Object obj : bindingOp.getBindingFaults().values()) {
                BindingFault fault = (BindingFault) obj;
                BindingFault newFault = def.createBindingFault();

                newFault.setName(fault.getName());
                newBindingOp.addBindingFault(newFault);
            }

            binding.addBindingOperation(newBindingOp);
        }
        def.removeBinding(oldBindingName);
    }

    /**
     * Creates a new <code>Jbi4CicsBinding</code> element from the specified
     * values.
     *
     * @param   copyCobol        the copy Cobol, used for the input and also for
     *                           the output step if <code>outputCopyCobol</code>
     *                           is <code>null</code> or blank.
     * @param   outputCopyCobol  the optional output copy Cobol to generates the
     *                           WSDL. If <code>null</code> or blank, is like to
     *                           not specify this parameter, so only
     *                           <code>copyCobol</code> will be considered and
     *                           will be used for input and for output during
     *                           CICS call.
     * @param   desc             the service descriptor.
     * @return  the newly created Cics binding element, containing the values
     *          received as parameters.
     */
    private static Jbi4CicsBinding createJbi4CicsBindingElement(
            final String copyCobol, final String outputCopyCobol,
            final ServiceDescriptor desc) {
        Jbi4CicsBinding binding = new Jbi4CicsBinding();

        binding.setElementType(Jbi4CicsExtension.Q_ELEM_JBI4CICS_BINDING);
        binding.setServicePackageName(desc.getServiceInterfacePackageName());
        binding.setCodePage(desc.getCodePage());
        binding.setCopyCobol(copyCobol);
        if ((outputCopyCobol != null) && !isBlank(outputCopyCobol)) {
            binding.setSameCopyCobol(Boolean.FALSE);
            binding.setOutputCopyCobol(outputCopyCobol);
        }

        return binding;
    }

    /**
     * Tests if the specified string is empty or contains only blank characters.
     * <p>
     * A <i>blank</i> character has code &lt;= <code>'&#92;u0020'</code> (the
     * space character).
     *
     * @param   str  the string to test.
     * @return  <code>true</code> if and only if <code>str</code> has length
     *          zero or is made only by blank characters.
     */
    private static boolean isBlank(String str) {
        for (int i = str.length() - 1; i >= 0; --i) {
            if (str.charAt(i) > ' ') {
                return false;
            }
        }
        return true;
    }
}
