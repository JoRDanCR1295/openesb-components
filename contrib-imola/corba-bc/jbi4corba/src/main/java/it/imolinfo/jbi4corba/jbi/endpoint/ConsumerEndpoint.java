/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.endpoint;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaDeployException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.exception.ServiceActivationException;
import it.imolinfo.jbi4corba.exception.ServiceCreationException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.jbi.processor.ConsumerExchangeProcessor;
import it.imolinfo.jbi4corba.jbi.processor.ExchangeProcessor;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtensionUtils;
import it.imolinfo.jbi4corba.webservice.descriptor.ConsumerServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.ConsumerServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.runtime.ConsumerServiceCreator;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
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
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;

import org.omg.CORBA.ORB;

import com.ibm.wsdl.extensions.PopulatedExtensionRegistry;
import com.ibm.wsdl.extensions.soap.SOAPBodyImpl;
import com.ibm.wsdl.extensions.soap.SOAPConstants;

/**
 * The Consumer Endpoint implementation class.
 */
@SuppressWarnings("unchecked")
public class ConsumerEndpoint extends Jbi4CorbaEndpoint {

	/** serialVersionUID. */
	private static final long serialVersionUID = -8507941260052862451L;

	/**
	 * The standard SOAP namespace URI used in WSDL files.
	 */
	private static final String SOAP_NAMESPACE_PREFIX = "soap";

	/**
	 * The logger for this class and its instances.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ConsumerEndpoint.class);

	/**
	 * The responsible to translate localized messages.
	 */
	private static final Messages MESSAGES = Messages
			.getMessages(ConsumerEndpoint.class);

	private ConsumerExchangeProcessor consumerExchangeProcessor;

	private ConsumerServiceDescriptor consumerServiceDescriptor;

	/**
	 * @param consumerServiceDescriptor
	 *            The consumer service descriptor
	 */
	public ConsumerEndpoint(QName serviceName, String endpointName,
			ConsumerServiceDescriptor consumerServiceDescriptor) {
		super(serviceName, endpointName);
		this.consumerServiceDescriptor = consumerServiceDescriptor;
		consumerServiceDescriptor.setEndpoint(this);
		consumerExchangeProcessor = new ConsumerExchangeProcessor(this);
	}

	/**
	 * Registers the service and starts the ORB.
	 * 
	 * @throws Exception
	 *             The exception
	 */
	public void activate() throws Jbi4CorbaException {

		registerService2();
		startServiceAndOrb();

		LOG.debug("consumer endpoint: " + this + " activated.");

	}

	/**
	 * This method is used to send asynchronously a MessageExchange.
	 * 
	 * @param me
	 *            The MessageExchange.
	 * @param channel
	 *            Used to send directly the ME if the Endpoint is an Active
	 *            Consumer.
	 * 
	 * @throws MessagingException
	 *             Sending operation failure.
	 */
	public void sendAsynch(MessageExchange me, DeliveryChannel channel)
			throws MessagingException {

		LOG.debug("Sending directly (JBI API)");
		channel.send(me);
	}

	/**
	 * Starts the service and the ORB.
	 * 
	 * @throws ServiceActivationException
	 *             The service activationa exception
	 */
	private void startServiceAndOrb() throws ServiceActivationException {
		ConsumerServiceCreator consumerServiceCreator = new ConsumerServiceCreator();

		consumerServiceCreator
				.registerAndActivateService(consumerServiceDescriptor);
		final ORB orb = consumerServiceDescriptor.getOrb();

		Thread thread = new Thread(new Runnable() {
			public void run() {
				LOG.debug("Activating orb.Thread: "
						+ Thread.currentThread().getName());

				orb.run();

				LOG.debug("Exiting orb.Thread: "
						+ Thread.currentThread().getName());
			}
		}, "ORB Runner for service: " + this);

		thread.setContextClassLoader(consumerServiceDescriptor
				.getServerCorbaClassesHolder().getOriginalClassLoader());
		thread.start();
	}

	/**
	 * Override.
	 * 
	 * @throws Exception
	 *             The exception
	 */
	public void deactivate() {
		stopServiceAndOrb();
	}

	/**
	 * Simplified implementation.
	 * 
	 */
	private void stopServiceAndOrb() {
		// simplified implementation
		consumerServiceDescriptor.getOrb().shutdown(false);
	}

	/**
	 * Override.
	 * 
	 * @return The processor
	 * 
	 * @see ConsumerExchangeProcessor.
	 */
	public ExchangeProcessor getProcessor() {
		return consumerExchangeProcessor;
	}

	/**
	 * If no targetService/targetEndpoint is configured, loads it from the WSDL
	 * service definition.
	 * 
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	public void registerService() throws Jbi4CorbaDeployException {
		// Do nothing...
	}

	/**
	 * this method has the following flow. First the wsdl is recovered. The wsdl
	 * at the moment is got from an existing jbi internal endpoint. In the
	 * future it could be given as a URL.
	 * 
	 * second the service creator with the wsdl is called
	 * 
	 * third the service invocation handler is created
	 * 
	 * fourth the endpoint metadata is published. (How to publish an external
	 * endpoint non SOAP???)
	 * 
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	public void registerService2() throws Jbi4CorbaDeployException {

		Definition wsdl;

		consumerServiceDescriptor.setComponentRootPath(RuntimeContext
				.getInstance().getComponentContext().getInstallRoot());

		ServiceEndpoint proxiedService = retrieveProxiedEndpointDefinition();
		consumerServiceDescriptor.setProxiedService(proxiedService);

		LOG.debug("Component root:"
				+ consumerServiceDescriptor.getComponentRootPath());

		// Removes all the Jbi4Corba extensions from the definition (if present)
		try {
			Jbi4CorbaExtensionUtils.removeCorbaElements(this.getDefinition());
		} catch (WSDLException e) {
			String msg = MESSAGES.getString(
					"CRB000139_Error_removing_JBI4CORBA_elements", e
							.getMessage());
			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}



		// Adds the SOAP extensions, if not present
		try {
			wsdl = addSoapElements(consumerServiceDescriptor);
			// consumerServiceDescriptor.getServiceWSDLDefinition());
		} catch (WSDLException e) {
			String msg = MESSAGES.getString(
					"CRB000138_Error_adding_SOAP_elements", e.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}
		writeWSDLDocument(wsdl);

		ConsumerServiceClassesGenerator consumerServiceClassesGenerator = new ConsumerServiceClassesGenerator();
		String wsdlFileName = "file://"
				+ consumerServiceDescriptor.getWsdlFileName();

		ServerCorbaClassesHolder serverCorbaClassesHolder;
		LOG.debug("consumerServiceDescriptor.getRootPath():"
				+ consumerServiceDescriptor.getRootPath()
				+ "; consumerServiceDescriptor.getComponentRootPath():"
				+ consumerServiceDescriptor.getComponentRootPath());
		try {

			serverCorbaClassesHolder = consumerServiceClassesGenerator
					.generateConsumerServiceClasses(wsdlFileName,
							consumerServiceDescriptor.getRootPath(),
							consumerServiceDescriptor.getComponentRootPath(),
							consumerServiceDescriptor.getJbiServiceDescriptor());

		} catch (ClassGenerationException e) {
			String msg = MESSAGES.getString(
					"CRB000103_Unable_to_create_service_classes", wsdlFileName,
					e.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}

		consumerServiceDescriptor
				.setServerCorbaClassesHolder(serverCorbaClassesHolder);

		ConsumerServiceCreator consumerServiceCreator = new ConsumerServiceCreator();
		try {
			consumerServiceCreator.createJbiService(consumerServiceDescriptor);
		} catch (ServiceCreationException e) {
			String msg = MESSAGES.getString(
					"CRB000104_Unable_to_create_jbi_service_consumer",
					consumerServiceDescriptor, e.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}

	}

	/**
	 * Adds SOAP elements to the specified WSDL if they are not present.
	 * 
	 * @param def
	 *            the WSDL to add SOAP elements. Must be not <code>null</code>.
	 * @return the WSDL document originated from <code>def</code> after the
	 *         addition of required SOAP elements, if not present inside
	 *         <code>def</code> itself.
	 * @throws WSDLException
	 *             in case of error manipulating the <code>Definition</code>.
	 */
	private static Definition addSoapElements(

	ConsumerServiceDescriptor consumerServiceDescriptor) throws WSDLException {

		Definition def = consumerServiceDescriptor.getServiceWSDLDefinition();

		ExtensionRegistry extReg = def.getExtensionRegistry();

		LOG.debug("Definition=" + def);
		boolean soapFound = false;
		LOG.debug("finding soap address ...");
		Port port = null;

		WSDLFactory factory = WSDLFactory.newInstance();

		LOG.debug("Factory class:" + factory.getClass().getName());

		ExtensionSerializer bindingSerializer = extReg.querySerializer(
				Binding.class, SOAPConstants.Q_ELEM_SOAP_BINDING);

		def.setExtensionRegistry(new PopulatedExtensionRegistry());
		LOG.debug("ExtensionRegistry:" + extReg.getClass().getName());
		LOG.debug("BINDING SERIALIZER: " + bindingSerializer);

		WSDLWriter writer = factory.newWSDLWriter();

		LOG.debug("addSoapElements[PRE]. soapFound=" + soapFound + "; Port="
				+ port + "; factory=" + factory + "; writer=" + writer);

		if (def.getBindings() == null || def.getBindings().size() == 0) {

			LOG.debug("The CONCRETE part of the wsdl is missing.");
			Definition newDef = addSOAPExtensionsToWSDL(def);
			consumerServiceDescriptor.setServiceWSDLDefinition(newDef);
			return newDef;

		} else {
			LOG.debug("The CONCRETE part of the wsdl is present.");
		}

		def.addNamespace(SOAP_NAMESPACE_PREFIX, SOAPConstants.NS_URI_SOAP);

		for (Object serviceAsObject : def.getServices().values()) {
			Service service = (Service) serviceAsObject;
			LOG.debug("Service=" + service + "; Service.QName="
					+ service.getQName());

			for (Object obj : service.getPorts().values()) {
				port = (Port) obj;
				for (Object element : port.getExtensibilityElements()) {
					if (element instanceof SOAPAddress) {
						soapFound = true;
						break;
					} else {
						LOG.debug("The elemente is NOT a SOAPAddress");
					}

				}
			}
		}
		LOG.debug("addSoapElements[POST]. soapFound=" + soapFound + "; Port="
				+ port + "; factory=" + factory + "; writer=" + writer);

		if (!soapFound) {
			LOG.debug("SORRY. soap NOT found.");

			LOG.debug("Port class:" + port.getClass().getName());

			SOAPAddress soapAddress = (SOAPAddress) extReg.createExtension(
					Port.class, SOAPConstants.Q_ELEM_SOAP_ADDRESS);

			LOG.debug("creating soapAddress=" + soapAddress);
			SOAPBinding soapBinding = (SOAPBinding) extReg.createExtension(
					Binding.class, SOAPConstants.Q_ELEM_SOAP_BINDING);
			LOG.debug("creating binding=" + soapBinding.getClass().getName());

			// Take first BindingOperation: what if they're more than one?
			BindingOperation bo = (BindingOperation) port.getBinding()
					.getBindingOperations().get(0);

			SOAPBody inputBody = (SOAPBody) extReg.createExtension(
					BindingInput.class, SOAPConstants.Q_ELEM_SOAP_BODY);

			LOG.debug("creating body=" + inputBody);

			SOAPOperation soapOperation = (SOAPOperation) extReg
					.createExtension(BindingOperation.class,
							SOAPConstants.Q_ELEM_SOAP_OPERATION);
			LOG.debug("creating SoapOperation=" + soapOperation);

			soapAddress.setLocationURI("http://localhost/fake_location");
			port.addExtensibilityElement(soapAddress);

			soapBinding.setTransportURI("http://schemas.xmlsoap.org/soap/http");
			soapBinding.setStyle("document");
			port.getBinding().addExtensibilityElement(soapBinding);

			soapOperation.setSoapActionURI("");
			bo.addExtensibilityElement(soapOperation);

			inputBody.setUse("literal");

			addSoapBodyIfNotPresent(bo.getBindingInput(), inputBody);
			addSoapBodyIfNotPresent(bo.getBindingOutput(), inputBody);
		}

		return def;
	}

	/**
	 * Used to add the soap:body to the binding input.
	 * 
	 * @param bindingInput
	 *            The binding input
	 * @param soapBody
	 *            The soap body
	 */
	private static void addSoapBodyIfNotPresent(
			final BindingInput bindingInput, final SOAPBody soapBody) {

		for (Object obj : bindingInput.getExtensibilityElements()) {
			if (obj instanceof SOAPBody) {
				return;
			}
		}
		bindingInput.addExtensibilityElement(soapBody);
	}

	/**
	 * Used to add the soap:body to the binding output.
	 * 
	 * @param bindingOutput
	 *            The binding output
	 * @param soapBody
	 *            The soap body
	 */
	private static void addSoapBodyIfNotPresent(
			final BindingOutput bindingOutput, final SOAPBody soapBody) {

		if (bindingOutput == null) {
			LOG.debug(">>>> bindingOutput is NULL!!!");
			return;
		}

		for (Object obj : bindingOutput.getExtensibilityElements()) {
			if (obj instanceof SOAPBody) {
				return;
			}
		}
		bindingOutput.addExtensibilityElement(soapBody);
	}

	/**
	 * This method find the first PortType in the definition.
	 * 
	 * @param def
	 *            The WSDL definition.
	 * 
	 * @return The PortType found.
	 */
	protected static PortType findFirstPortType(final Definition def) {

		PortType portType = null;

		for (Object portTypeAsObject : def.getPortTypes().keySet()) {
			LOG.debug("PortTypeKEY=" + portTypeAsObject + "; CLASS="
					+ portTypeAsObject.getClass());

			if (portType == null) {
				portType = (PortType) def.getPortType((QName) portTypeAsObject);
			}
		}

		return portType;
	}

	/**
	 * If there isn't the concrete part of the wsdl we must construct it using
	 * the abstract part and this method adds the 'operations' in the PortType
	 * to the binding.
	 * 
	 * @param wsdl
	 *            The WSDL definition.
	 * @param portType
	 *            A PortType.
	 * @param wsdlBinding
	 *            A Binding.
	 */
	protected static void addOperationToWsdlBinding(Definition wsdl,
			PortType portType, Binding wsdlBinding) {

		// Add the bindingOperations (with input,output and faults)
		List<Operation> operations = portType.getOperations();

		for (Operation operation : operations) {
			BindingOperation bop = wsdl.createBindingOperation();

			bop.setOperation(operation);

			bop.setName(operation.getName());

			Input input = operation.getInput();
			if (input != null) {

				SOAPBody soapBody = new SOAPBodyImpl();
				soapBody.setUse("literal");

				BindingInput bindingInput = wsdl.createBindingInput();
				bindingInput.setName(input.getName());
				bindingInput.addExtensibilityElement(soapBody);

				bop.setBindingInput(bindingInput);
			}

			Output output = operation.getOutput();
			if (output != null) {
				SOAPBody soapBody = new SOAPBodyImpl();
				soapBody.setUse("literal");

				BindingOutput bindingOutput = wsdl.createBindingOutput();
				bindingOutput.setName(output.getName());
				bindingOutput.addExtensibilityElement(soapBody);

				bop.setBindingOutput(bindingOutput);
			}

			Map faults = operation.getFaults();

			Iterator faultIt = faults.entrySet().iterator();
			while (faultIt.hasNext()) {
				Fault fault = (Fault) ((Map.Entry) faultIt.next()).getValue();
				BindingFault bindingFault = wsdl.createBindingFault();
				bindingFault.setName(fault.getName());
				bop.addBindingFault(bindingFault);
			}

			wsdlBinding.addBindingOperation(bop);
		}
	}

	/**
	 * Adds SOAP extensions to wsdl. This code expects that the PortTypeName is
	 * the real interface name.
	 * 
	 * @param def
	 *            The WSDL definition.
	 * 
	 * @return The new WSDL definition.
	 * 
	 * @throws WSDLException
	 */
	protected static Definition addSOAPExtensionsToWSDL(Definition def)
			throws WSDLException {

		ExtensionRegistry extRegistry = def.getExtensionRegistry();

		String defaultLocationURI = "http://localhost/services/defaultSoapAddress";

		PortType portType = findFirstPortType(def);
		String portTypeLocalPart = portType.getQName().getLocalPart();
		String portTypeNamespace = portType.getQName().getNamespaceURI();

		String portTypeNameCut = null;
		if (portTypeLocalPart.endsWith("PortType")) {
			int totLen = portTypeLocalPart.length();
			int suffixLen = "PortType".length();
			portTypeNameCut = portTypeLocalPart
					.substring(0, totLen - suffixLen);
		} else {
			portTypeNameCut = portTypeLocalPart;
		}

		// creating the default names
		String serviceName = portTypeNameCut + "Service";
		String bindingName = portTypeNameCut + "HttpBinding";
		String portName = portTypeNameCut + "HttpPort";
		LOG.debug("servicename=" + serviceName + "; bindingName=" + bindingName
				+ "; portName=" + portName);

		// creating the wsdl:binding element
		Binding wsdlBinding = def.createBinding();
		def.addBinding(wsdlBinding);
		wsdlBinding.setQName(new QName(portTypeNamespace, bindingName));
		wsdlBinding.setPortType(portType);
		wsdlBinding.setUndefined(false);

		// Creates the binding extensibility elements
		Port wsdlPort = def.createPort();

		// Creates the address extensibility elements
		SOAPAddress soapAddress = (SOAPAddress) extRegistry.createExtension(
				wsdlPort.getClass(), SOAPConstants.Q_ELEM_SOAP_ADDRESS);

		soapAddress.setLocationURI(defaultLocationURI);

		wsdlPort.setName(portName);
		wsdlPort.setBinding(wsdlBinding);
		wsdlPort.addExtensibilityElement(soapAddress);

		addOperationToWsdlBinding(def, portType, wsdlBinding);

		// creating soap:binding
		SOAPBinding soapBinding = (SOAPBinding) extRegistry.createExtension(
				wsdlBinding.getClass(), SOAPConstants.Q_ELEM_SOAP_BINDING);
		soapBinding.setStyle("document");
		soapBinding.setTransportURI("http://schemas.xmlsoap.org/soap/http");

		wsdlBinding.addExtensibilityElement(soapBinding);

		// Adding the service
		Service service = def.createService();
		service.setQName(new QName(portTypeNamespace, serviceName));
		service.addPort(wsdlPort);
		def.addService(service);

		// return the WSDL with abstract and concrete part
		LOG.debug("NEW DEFINITION=" + def);

		// LOG.debug("*********************");
		// debug(def);
		// LOG.debug("*********************");
		return def;
	}

	// /**
	// * Print all the WSDL definition.
	// */
	// protected static void debug(Definition def) {
	// if (! LOG.isDebugEnabled()) return;
	// // else
	// for (Object s : def.getServices().keySet()) {
	// Service se = (Service) def.getServices().get(s);
	// LOG.debug("___ SERVICE. key=" + s + "; object=" + se + "; class="
	// + se.getClass());
	//
	// for (Object p : se.getPorts().keySet()) {
	// Port port = (Port) se.getPorts().get(p);
	// LOG.debug("___ PORT. key=" + p + "; object=" + port + "; class="
	// + port.getClass());
	//
	// Binding binding = port.getBinding();
	// List bindingOperations = binding.getBindingOperations();
	// for (int i = 0; i < bindingOperations.size(); i++) {
	// LOG.debug("__ I=" + i);
	// BindingOperation bindingOperation
	// = (BindingOperation) bindingOperations.get(i);
	// LOG.debug("__ BindingOperation=" + bindingOperation);
	// //String opName = bindingOperation.getOperation().getName();
	// }
	// }
	// }
	//
	// }

	/**
	 * Writing the WSDL.
	 * 
	 * @param wsdl
	 *            The wsdl document
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	private void writeWSDLDocument(Definition def)
			throws Jbi4CorbaDeployException {
		FileWriter fr = null;
		WSDLFactory factory = null;
		WSDLWriter writer = null;
                
                
		try {
                    File root=new File(consumerServiceDescriptor.getRootPath());
                    //allow multiple consumer
                    if(!root.exists()){
                        root.mkdir();
                    }
			if (consumerServiceDescriptor.getWsdlFileName() == null) {
				consumerServiceDescriptor
						.setWsdlFileName(consumerServiceDescriptor
								.getRootPath()
								+ File.separator
								+ consumerServiceDescriptor.getServiceName()
								+ ".wsdl");
			}

			fr = new FileWriter(consumerServiceDescriptor.getWsdlFileName());

		} catch (IOException e) {
			String msg = MESSAGES
					.getString("CRB000105_Unable_to_open_filename",
							consumerServiceDescriptor.getWsdlFileName(), e
									.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}
		try {
			factory = WSDLFactory.newInstance();
			writer = factory.newWSDLWriter();
			writer.writeWSDL(def, fr);

		} catch (WSDLException e) {
			String msg = MESSAGES.getString(
					"CRB000106_Unable_to_create_default_transformer", writer, e
							.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}

		try {
			fr.close();
		} catch (IOException e) {
			String msg = MESSAGES.getString(
					"CRB000108_Unable_to_close_fileWriter", fr, e.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}

	}

	/**
	 * Getter.
	 * 
	 * @return The consumer service descriptor.
	 */
	public ConsumerServiceDescriptor getConsumerServiceDescriptor() {
		return consumerServiceDescriptor;
	}

	/**
	 * Setter.
	 * 
	 * @param consumerServiceDescriptor
	 *            The consumer service descriptor
	 */
	public void setConsumerServiceDescriptor(
			ConsumerServiceDescriptor consumerServiceDescriptor) {
		this.consumerServiceDescriptor = consumerServiceDescriptor;
	}

	/**
	 * Create a wsdl definition for a consumer endpoint. Loads the target
	 * endpoint definition and add http binding informations to it.
	 * 
	 * @return The return
	 * 
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	protected ServiceEndpoint retrieveProxiedEndpointDefinition()
			throws Jbi4CorbaDeployException {

		String targetEndpoint = consumerServiceDescriptor.getTargetEndpoint();
		QName targetService = consumerServiceDescriptor.getTargetService();
		QName targetInterfaceName = consumerServiceDescriptor
				.getTargetInterfaceName();

		if (LOG.isDebugEnabled()) {
			LOG.debug("Retrieving proxied endpoint definition");
			LOG.debug("Target service: " + targetService);
			LOG.debug("Target endpoint: " + targetEndpoint);
			LOG.debug("Target Interface Name: " + targetInterfaceName);
		}

		ComponentContext ctx = RuntimeContext.getInstance()
				.getComponentContext();
		ServiceEndpoint ep = null;

		LOG.info("CRB000151_Endpoint_found", new Object[] { ctx.getEndpoint(
				targetService, targetEndpoint) });
		ServiceEndpoint[] eps1 = ctx.getEndpointsForService(targetService);
		for (int i = 0; i < eps1.length; i++) {
			LOG.debug("Endpoint from  getEndpointsForService found:" + eps1[i]);
		}

		if (targetService != null || targetEndpoint != null
				|| targetInterfaceName != null) {

			// targetXXX is defined on SU configuration (see CRB-64)
			if (targetService != null && targetEndpoint != null) {
				ep = ctx.getEndpoint(targetService, targetEndpoint);
				if (ep == null) {
					String msg = MESSAGES.getString(
							"CRB000109_Could_not_retrieve_endpoint",
							targetService, targetEndpoint, ctx);

					LOG.error(msg);
					throw new Jbi4CorbaDeployException(msg);
				}
				LOG
						.debug("Endpoint retrived using targetService/targetEndpoint:"
								+ targetService + "/" + targetEndpoint);
			}

			if (ep == null && targetService != null) {
				LOG.debug("Returns EP from service");
				ServiceEndpoint[] eps = ctx
						.getEndpointsForService(targetService);
				if (eps != null && eps.length > 0) {
					ep = eps[0];
				}
				if (ep == null) {
					LOG.error("CRB000110_Could_not_retrieve_endpoint",
							targetService, ctx);
					throw new Jbi4CorbaDeployException(MESSAGES.getString(
							"CRB000110_Could_not_retrieve_endpoint",
							targetService, ctx));
				}
				LOG.debug("Endpoint retrived using targetService:"
						+ targetService);
			}

			if (ep == null && targetInterfaceName != null) {
				LOG.debug("Returns EP from targetInterfaceName");
				ServiceEndpoint[] eps = ctx.getEndpoints(targetInterfaceName);
				if (eps != null && eps.length > 0) {
					ep = eps[0];
				}
				if (ep == null) {
					LOG.error("CRB000111_Could_not_retrieve_endpoint",
							targetInterfaceName, ctx);
					throw new Jbi4CorbaDeployException(MESSAGES.getString(
							"CRB000111_Could_not_retrieve_endpoint",
							targetInterfaceName, ctx));
				}
				LOG.debug("Endpoint retrived using targetInterfaceName:"
						+ targetInterfaceName);
			}
		}

		if (ep != null) {
			LOG.debug("Enpoind retrived");
			LOG.debug("     Service Name:" + ep.getServiceName());
			LOG.debug("     Endpoint Name:" + ep.getEndpointName());
		} else {
			LOG.info("CRB000112_No_endpoint_retrived");
		}

		// If the endpoint is null, loads it using my name
		if (ep == null) {
			LOG.debug("Endpoint is null: pointing to myself");
			ep = ctx.getEndpoint(this.getServiceName(), this.getEndpointName());
		}

		return ep;
	}

	/**
	 * Validate the endpoint. (now do noting)
	 * 
	 * @throws Jbi4EjbException
	 *             if some problem occurs
	 */
	public void validate() throws Jbi4CorbaException {
		// Do nothing
	}

	/**
	 * Unregisters the service.
	 * 
	 * @see it.imolinfo.jbi4ejb.jbi.endpoint.Jbi4EjbEndpoint#unregisterService()
	 * @throws Jbi4EjbException
	 *             if some problem occurs
	 */
	public void unregisterService() throws Jbi4CorbaException {
		// DO nothing
		LOG.info("CRB000150_Service_unregistered", new Object[] {
				this.getServiceName(), this.getEndpointName() });

	}

	/**
	 * Gets the provider role.
	 */
	public Role getRole() {
		return Role.CONSUMER;
	}

}
