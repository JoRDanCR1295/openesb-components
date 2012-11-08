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
import it.imolinfo.jbi4corba.exception.Jbi4CorbaDeployException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.processor.ProviderExchangeProcessor;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.rmi.RMISecurityManager;
import java.util.Map;
import java.util.Properties;

import java.util.logging.Level;
import javax.jbi.messaging.MessageExchange.Role;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.cxf.endpoint.Endpoint;
import org.apache.cxf.service.Service;
import org.apache.cxf.service.model.EndpointInfo;
import org.omg.CORBA.Any;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Request;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.portable.ValueFactory;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextPackage.CannotProceed;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * The Provider Endpoint.
 */
@SuppressWarnings("unchecked")
public class ProviderEndpoint extends Jbi4CorbaEndpoint {

	/** serialVersionUID. */
	private static final long serialVersionUID = -7829120677780408268L;

	/**
	 * The logger for this class and its instances.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ProviderEndpoint.class);

	/**
	 * The responsible of message localization.
	 */
	private static final Messages MESSAGES = Messages
			.getMessages(ProviderEndpoint.class);

	private ProviderServiceDescriptor serviceDescriptor;

	// The cxf service model
	private Service cxfService;

	// The cxf endpoint
	private Endpoint cxfEndpoint;

	private ORB orb;

	/**
	 * The Corba helper class
	 */
	private Class helperClass;

	/**
	 * The Narrow Method
	 */
	private Method narrowMethod;

	private String iorCorbaObject;

	/**
	 * Instantiates a new jbi4 ejb provider endpoint.
	 * 
	 * @param serviceName
	 *            the service name
	 * @param endpointName
	 *            the endpoint name
	 * 
	 * @throws Jbi4EjbException
	 *             if some problem occurs
	 */
	public ProviderEndpoint(QName serviceName, String endpointName,
			ProviderServiceDescriptor serviceDescriptor)
			throws Jbi4CorbaException {
		super(serviceName, endpointName);
		this.serviceDescriptor = serviceDescriptor;
		this.setExchangeProcessor(new ProviderExchangeProcessor(this));
		this.serviceDescriptor.setEndpoint(this);
	}

	/**
	 * @return Returns the CXF Service.
	 */
	public Service getCXFService() {
		return cxfService;
	}

	/**
	 * @return Returns the CXF Service.
	 */
	public Endpoint getCXFEndpoint() {
		return cxfEndpoint;
	}

	/**
	 * Return the Ior for the corba object
	 */
	public String getIorCorbaObject() {
		return iorCorbaObject;
	}

	/**
	 * set the Ior for reference corba Object generate by statefull interface
	 */
	public void setIorCorbaObject(String iorCorbaObject) {
		this.iorCorbaObject = iorCorbaObject;
	}

	/**
	 * This method returns the role of the component.
	 * 
	 * @return The role of the component.
	 * 
	 * @see org.servicemix.common.Endpoint#getRole() @org.apache.xbean.XBean
	 *      hide="true"
	 */
	public Role getRole() {
		return Role.PROVIDER;
	}

	/**
	 * 
	 * @throws Exception
	 *             The exception
	 */
	public void activate() throws Jbi4CorbaException {
		LOG.debug(">>>>> activate - begin");

		initOrb();
		try {
			locateCorbaService();
		} catch (Jbi4CorbaException e) {
			// LOG.warn("Unable to retrived object using locateCorbaService: " +
			// e);
			String msg = MESSAGES
					.getString(
							"CRB000145_Unable_to_retrive_object_using_locateCorbaService",
							new java.lang.Object[] { e });
			LOG.warn(msg);
		}
		LOG.debug("<<<<< activate - end");
	}

	/**
	 * Deactivate the endpoint.
	 * 
	 * @throws Exception
	 *             The exception
	 */
	public void deactivate() throws Jbi4CorbaException {
		stopCorbaOrb();
	}
	
	/**
	 * Gets the service data from the WSDL and creates the CXF service.
	 * 
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	public void registerService() throws Jbi4CorbaDeployException {
		LOG.debug("registering service for service descriptor: "
				+ serviceDescriptor);

		serviceDescriptor.setComponentRootPath(RuntimeContext.getInstance()
				.getComponentContext().getInstallRoot());

		// CXF service generation
		ProviderServiceCreator serviceCreator = new ProviderServiceCreator();
		try {
			cxfService = serviceCreator.createService(serviceDescriptor, this
					.getServiceName());
			LOG.debug("CXF Service Gnerator >>>> " + this.getServiceName());
			EndpointInfo cxfei = CXFUtils.getEndpointInfo(cxfService);
			cxfEndpoint = CXFUtils.getEndpoint(cxfService, cxfei);
		} catch (Jbi4CorbaException ex) {
			String msg = MESSAGES.getString(
					"CRB000146_Error_in_service_creation",
					new java.lang.Object[] { ex.getMessage() });
			LOG.error(msg, ex);
			throw new Jbi4CorbaDeployException(msg, ex);
		} catch (IOException ex) {
			String msg = MESSAGES.getString(
					"CRB000146_Error_in_service_creation",
					new java.lang.Object[] { ex.getMessage() });
			LOG.error(msg, ex);
			throw new Jbi4CorbaDeployException(msg, ex);
		} catch (WSDLException ex) {
			String msg = MESSAGES.getString(
					"CRB000146_Error_in_service_creation",
					new java.lang.Object[] { ex.getMessage() });
			LOG.error(msg, ex);
			throw new Jbi4CorbaDeployException(msg, ex);
		}

		DOMSource domSource = new DOMSource(generateWsdl(cxfService));

		String wsdlFilename = null;
		wsdlFilename = serviceDescriptor.getWsdlRootDirectory() + "/"
				+ cxfService.getName().getLocalPart() + ".wsdl.debug.cxf";

		LOG.debug("CRB000127_Producing_service_wsdl_to",
				new java.lang.Object[] { wsdlFilename });
		FileWriter fr;
		try {
			fr = new FileWriter(wsdlFilename);
		} catch (IOException e) {
			String msg = MESSAGES.getString(
					"CRB000105_Unable_to_open_filename", wsdlFilename, e
							.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}
		StreamResult streamResult = new StreamResult(fr);
		TransformerFactory transformerFactory = TransformerFactory
				.newInstance();
		Transformer transformer;
		try {
			transformer = transformerFactory.newTransformer();
		} catch (TransformerConfigurationException e) {
			String msg = MESSAGES.getString(
					"CRB000106_Unable_to_create_default_transformer",
					transformerFactory, e.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}
		try {
			transformer.transform(domSource, streamResult);
		} catch (TransformerException e) {
			String msg = MESSAGES.getString(
					"CRB000107_Unable_to_create_transform", domSource,
					streamResult, transformer, e.getMessage());

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
	 * @param serviceCreator
	 *            The service creator
	 * @param sd
	 *            The provider service descriptor
	 * @param interfaceName
	 *            The interface name
	 * 
	 * @return The WSDL generated
	 * 
	 * @throws DeploymentException
	 *             The deployment exception
	 */
	protected Document generateWsdl(Service cxfService)
			throws Jbi4CorbaDeployException {

		ByteArrayOutputStream baos = new ByteArrayOutputStream();

		try {
			CXFUtils.writeDefinitionOnOutputStream(cxfService, baos);
		} catch (WSDLException ex) {
			String msg = MESSAGES.getString("CRB000147_Error_in_WSDL_creation",
					new java.lang.Object[] { ex.getMessage() });
			LOG.warn(msg, ex);
		}

		LOG.debug(">>>>> generateWsdl - begin");

		if (LOG.isDebugEnabled()) {

			LOG.debug("cxfService:" + cxfService);

			LOG.debug("WSDL:\n------------------\n" + baos.toString()
					+ "\n------------------");
		}
		// =========================================================
		// WSDL From CXF -> WSDL as DOM
		// =========================================================

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);

		DocumentBuilder documentBuilder;
		try {
			documentBuilder = factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			String msg = MESSAGES.getString(
					"CRB000128_Unable_to_create_documentBuilder", factory, e
							.getMessage());

			LOG.error(msg, e);
			throw new Jbi4CorbaDeployException(msg, e);
		}

		ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
		Document doc;
		try {

			doc = documentBuilder.parse(bais);

		} catch (SAXException e) {
			String m = MESSAGES.getString("CRB000129_Unable_to_parse_document",
					bais, documentBuilder, e.getMessage());

			LOG.error(m, e);
			throw new Jbi4CorbaDeployException(m, e);
		} catch (IOException e) {
			String m = MESSAGES.getString("CRB000129_Unable_to_parse_document",
					bais, documentBuilder, e.getMessage());

			LOG.error(m, e);
			throw new Jbi4CorbaDeployException(m, e);
		}

		LOG.debug("<<<<< generateWsdl - end");
		return doc;
	}

	/**
	 * @return Returns the serviceDescriptor
	 */
	public ProviderServiceDescriptor getServiceDescriptor() {
		return serviceDescriptor;
	}

	/**
	 * @param serviceDescriptor
	 *            The serviceDescriptor to set.
	 */
	public void setServiceDescriptor(ProviderServiceDescriptor serviceDescriptor) {
		this.serviceDescriptor = serviceDescriptor;
	}

	/**
	 * Stops the ORB.
	 */
	private void stopCorbaOrb() {
		orb.shutdown(true);
	}

	/**
	 * 
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	private void initOrb() throws Jbi4CorbaException {

		LOG.debug(">>>>> initOrb - begin");
		ClassLoader oldCL = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(
		// serviceDescriptor.getUrlClassLoader());
				serviceDescriptor.getOriginalClassLoader());
		if (serviceDescriptor != null) {
			debugProperties(serviceDescriptor.getOrbProperties());
		}

		try {
			LOG.debug("locating service for serviceDescriptor: "
					+ serviceDescriptor);

			orb = ORB.init((String[]) null, serviceDescriptor
					.getOrbProperties());

			LOG.debug("orb: " + orb + " created for service: "
					+ serviceDescriptor);

			/* Registering the factory class. */
			Map<String, java.lang.Object> map = serviceDescriptor
					.getValueTypeIdAndInstance();

			if (map == null || map.size() == 0) {
				LOG
						.debug("CRB000130_No_value_type_factory_to_register_using_the_ORB");
			} else {
				LOG.debug("ValueTypeFactoryMap.size=" + map.size());

				// registering all valuetypes factories.
				for (String id : map.keySet()) {
					ValueFactory vf = (ValueFactory) map.get(id);

					LOG.debug("Provider. Registering a ValueType Factory. id="
							+ id + "; instance=" + vf);

					((org.omg.CORBA_2_3.ORB) orb)
							.register_value_factory(id, vf);

				}
			}
			// get the helper class and the methods
            //LOG.debug("RAAAF: in deploy lookup value factory per MySequence: "+((org.omg.CORBA_2_3.ORB) orb).lookup_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0"));
            //LOG.debug("RAAAF: in deploy lookup value factory per MySequence: "+((org.omg.CORBA_2_3.ORB) orb).lookup_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0").getClass());
            //LOG.debug("RAAAF: in deploy lookup value factory per MySequence: "+((ValueFactory)((org.omg.CORBA_2_3.ORB) orb).lookup_value_factory("IDL:3hh4.123/it/imolinfo/jbi4corba/test/testprovidercomplex/MySequence:1.0")).);

			//
			helperClass = serviceDescriptor.getCorbaHelperClass();

			// Reload using original classloader.

			String helperClassName = helperClass.getName();
			helperClass = serviceDescriptor.getOriginalClassLoader().loadClass(
					helperClassName);
			LOG.debug("Helper class loader: "
					+ helperClass.getClass().getName());
			narrowMethod = getNarrowMethod(helperClass);

		} catch (ClassNotFoundException ex) {
			java.util.logging.Logger
					.getLogger(ProviderEndpoint.class.getName()).log(
							Level.SEVERE, null, ex);
			String msg = MESSAGES.getString("CRB000152_Error_in_initializing_the_ORB");
			LOG.error(msg, ex.getMessage());

		} finally {
			Thread.currentThread().setContextClassLoader(oldCL);
			LOG.debug("<<<<< initOrb - end");
		}
	}

	/**
	 * 
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	public void locateCorbaService() throws Jbi4CorbaException {

		ClassLoader oldCL = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(
		// serviceDescriptor.getUrlClassLoader());
				serviceDescriptor.getOriginalClassLoader());
		try {
			LOG.debug(">>>>> locateCorbaService - begin");

			// Class helperClass = serviceDescriptor.getCorbaHelperClass();
			// Method narrowMethod = getNarrowMethod(helperClass);
			// Method typeMethod = getTypeMethod(helperClass);
			// Method extractMethod = getExtractMethod(helperClass);

			java.lang.Object retrievedObject = null;
			Object corbaObjectReference = null;

			// ---- Localization ...
			LOG.debug("CRB000140_LocalizationType",
					new java.lang.Object[] { serviceDescriptor
							.getLocalizationType() });

			if (ProviderServiceDescriptor.NAMESERVICE
					.equalsIgnoreCase(serviceDescriptor.getLocalizationType())) {

				retrievedObject = localizationViaNameService();

				corbaObjectReference = getCorbaObjectReference(helperClass,
						narrowMethod, retrievedObject);

			} else if (ProviderServiceDescriptor.CORBALOC
					.equalsIgnoreCase(serviceDescriptor.getLocalizationType())) {

				retrievedObject = localizationViaCorbaloc();

				corbaObjectReference = getCorbaObjectReference(helperClass,
						narrowMethod, retrievedObject);
			} else if (ProviderServiceDescriptor.CORBANAME
					.equalsIgnoreCase(serviceDescriptor.getLocalizationType())) {

				retrievedObject = localizationViaCorbaname();

				corbaObjectReference = getCorbaObjectReference(helperClass,
						narrowMethod, retrievedObject);
			} else if (ProviderServiceDescriptor.IOR
					.equalsIgnoreCase(serviceDescriptor.getLocalizationType())) {

				retrievedObject = localizationViaIOR();

				corbaObjectReference = getCorbaObjectReference(helperClass,
						narrowMethod, retrievedObject);
			}

			// else if (EJB.equals(serviceDescriptor.getLocalizationType())) {
			//
			// retrievedObject = ejbLocalization(helperClass,
			// extractMethod,
			// narrowMethod,
			// typeMethod);
			//
			// corbaObjectReference = getCorbaObjectReference(
			// helperClass, narrowMethod, retrievedObject);
			//
			// }
			// Added for Test
			// else {
			// throw new IllegalArgumentException(MESSAGES.getString(
			// "CRB000137_Localization_type_not_supported",
			// serviceDescriptor.getLocalizationType()));
			// }

			serviceDescriptor.setCorbaObjectReference(corbaObjectReference);
			LOG.debug("service reference found: " + corbaObjectReference);
			if (corbaObjectReference != null) {
				LOG.debug("service reference classloader: "
						+ corbaObjectReference.getClass().getClassLoader());
			}

		} finally {
			Thread.currentThread().setContextClassLoader(oldCL);
		}
		LOG.debug("<<<<< locateCorbaService - end");
	}

	/**
	 * This method is used to obtain a reference to the corba object.
	 * 
	 * @param helperClass
	 *            The class that provides the method 'narrow'.
	 * @param narrowMethod
	 *            The method to invoke.
	 * @param retrievedObject
	 *            The object used as paramameter for the narrow.
	 * 
	 * @return The corba object reference.
	 * 
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected Object getCorbaObjectReference(Class helperClass,
			Method narrowMethod, java.lang.Object retrievedObject)
			throws Jbi4CorbaException {

		LOG.debug("helperClass=" + helperClass + "; narrowMethod="
				+ narrowMethod + "; retrievedObject=" + retrievedObject);

		Object corbaObjectReference = null;
		try {

			Object retrievedObjectAsCorbaObject = (Object) retrievedObject;

			corbaObjectReference = (Object) narrowMethod.invoke(helperClass,
					retrievedObjectAsCorbaObject);

		} catch (IllegalArgumentException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"IllegalArgumentException", retrievedObject, e.getMessage() };

			LOG.error("CRB000135_Exception_calling_narrow_method", args, e);
			throw new Jbi4CorbaException(
					"CRB000135_Exception_calling_narrow_method", args, e);
		} catch (IllegalAccessException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"IllegalAccessException", retrievedObject, e.getMessage() };

			LOG.error("CRB000135_Exception_calling_narrow_method", args, e);
			throw new Jbi4CorbaException(
					"CRB000135_Exception_calling_narrow_method", args, e);
		} catch (InvocationTargetException e) {
			Throwable target = e.getTargetException();

			LOG.error("CRB000135_Exception_calling_narrow_method",
					new java.lang.Object[] { "InvocationTargetException",
							retrievedObject, e.getMessage() }, e);
			LOG.error("CRB000136_Real_error_message_is",
					new java.lang.Object[] { target.getMessage() }, target);
			throw new Jbi4CorbaException(
					"CRB000135_Exception_calling_narrow_method",
					new java.lang.Object[] { "Exception", retrievedObject,
							target.getMessage() }, target);
		}

		LOG.debug("corbaObjectReference=" + corbaObjectReference);
		return corbaObjectReference;
	}

	/**
	 * 
	 * @param helperClass
	 *            The helper class
	 * @param extractMethod
	 *            The extract method
	 * @param narrowMethod
	 *            The narrow method
	 * @param typeMethod
	 *            The type method
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected java.lang.Object ejbLocalization(Class helperClass,
			Method extractMethod, Method narrowMethod, Method typeMethod)
			throws Jbi4CorbaException {

		java.lang.Object retrievedObject = null;
		SecurityManager oldSecurityManager = System.getSecurityManager();
		try {

			System.setSecurityManager(new RMISecurityManager());
			// obtain home interface via corba loc
			Object ejbHome = orb.string_to_object(serviceDescriptor
					.getCorbaServiceName());
			// call create method and obtain a pointer to real stateless bean
			Any result = orb.create_any();
			NamedValue resultVal = orb.create_named_value("result", result,
					org.omg.CORBA.ARG_OUT.value);
			Request createRequest = ejbHome._create_request(null, "create", orb
					.create_list(0), resultVal);
			TypeCode resultTypeCode;

			try {
				resultTypeCode = (TypeCode) typeMethod.invoke(helperClass,
						new java.lang.Object[] {});

			} catch (IllegalArgumentException e) {
				java.lang.Object[] args = new java.lang.Object[] {
						"IllegalArgumentException", retrievedObject,
						e.getMessage() };
				// @TODO
				LOG.error("CRB000135_Exception_calling_typeMethod", args, e);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_typeMethod", args, e);
			} catch (IllegalAccessException e) {
				java.lang.Object[] args = new java.lang.Object[] {
						"IllegalAccessException", retrievedObject,
						e.getMessage() };
				// @TODO
				LOG.error("CRB000135_Exception_calling_typeMethod", args, e);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_extractMethod", args, e);
			} catch (InvocationTargetException e) {
				Throwable target = e.getTargetException();
				// @TODO
				LOG.error("CRB000135_Exception_calling_typeMethod",
						new java.lang.Object[] { "InvocationTargetException",
								retrievedObject, e.getMessage() }, e);
				LOG.error("CRB000136_Real_error_message_is",
						new java.lang.Object[] { target.getMessage() }, target);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_typeMethod",
						new java.lang.Object[] { "Exception", retrievedObject,
								target.getMessage() }, target);
			}

			createRequest.set_return_type(resultTypeCode);
			createRequest.invoke();
			result = createRequest.return_value();

			// we assume this is a refence to the ejb
			try {

				retrievedObject = extractMethod.invoke(helperClass,
						new java.lang.Object[] { result });

			} catch (IllegalArgumentException e) {
				java.lang.Object[] args = new java.lang.Object[] {
						"IllegalArgumentException", retrievedObject,
						e.getMessage() };
				// @TODO
				LOG.error("CRB000135_Exception_calling_extractMethod", args, e);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_extractMethod", args, e);
			} catch (IllegalAccessException e) {
				java.lang.Object[] args = new java.lang.Object[] {
						"IllegalAccessException", retrievedObject,
						e.getMessage() };
				// @TODO
				LOG.error("CRB000135_Exception_calling_extractMethod", args, e);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_extractMethod", args, e);
			} catch (InvocationTargetException e) {
				Throwable target = e.getTargetException();
				// @TODO
				LOG.error("CRB000135_Exception_calling_extractMethod",
						new java.lang.Object[] { "InvocationTargetException",
								retrievedObject, e.getMessage() }, e);
				LOG.error("CRB000136_Real_error_message_is",
						new java.lang.Object[] { target.getMessage() }, target);
				throw new Jbi4CorbaException(
						"CRB000135_Exception_calling_extractMethod",
						new java.lang.Object[] { "Exception", retrievedObject,
								target.getMessage() }, target);
			}
		} finally {
			System.setSecurityManager(oldSecurityManager);
		}

		return retrievedObject;
	}

	/**
	 * This method is used to retrieve an object using corbaloc url. The
	 * corbaloc URL is defined using the corbaServiceName property of the
	 * service descriptor.
	 * 
	 * [...]
	 * 
	 * Bootstrap Options for the ORB
	 * 
	 * The ORB can be configured to return the handle of a customized CORBA
	 * service from resolve_initial_references() using either ORBInitRef and/or
	 * ORBDefaultInitRef.
	 * 
	 * For example, Use -ORBInitRef to resolve to specific CORBA services, for
	 * example,
	 * 
	 * -ORBInitRef TraderService=corbaloc::myBank.com:2050/TraderService
	 * 
	 * If no -ORBInitRef is given, -ORBDefaultInitRef is used to resolve. In the
	 * TraderService example,
	 * 
	 * -ORBDefaultInitRef corbaloc:iiop:1.2:myBank.com:2050
	 * 
	 * The order of resolution when these options are used is as follows: 1.
	 * Objects registered with register_initial_references 2. -ORBInitRef 3.
	 * -ORBDefaultInitRef
	 * 
	 * @see http://java.sun.com/j2se/1.5.0/docs/guide/idl/INStutorial.html
	 * @see it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor
	 * 
	 * @return The object used to retrieve the corba reference.
	 * 
	 * @throws Jbi4CorbaException
	 *             When the properties ORBInitRef and ORBDefaultInitRef are both
	 *             null or empty.
	 * 
	 */
	protected java.lang.Object localizationViaCorbaloc()
			throws Jbi4CorbaException {

		String corbalocUrl = serviceDescriptor.getCorbaServiceName();
		LOG.debug("corbalocUrl=" + corbalocUrl);

		if (corbalocUrl == null || "".equals(corbalocUrl)) {
			LOG.error("CRB000141_CorbalocURL_NotFound");
			throw new Jbi4CorbaException("CRB000141_CorbalocURL_NotFound");
		}

		java.lang.Object retrievedObject = orb.string_to_object(corbalocUrl);
		LOG.debug("retrievedObject=" + retrievedObject);

		return retrievedObject;
	}

	/**
	 * 
	 * @return The retrieved object
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected java.lang.Object localizationViaCorbaname()
			throws Jbi4CorbaException {

		String corbanameUrl = serviceDescriptor.getCorbaServiceName();
		LOG.debug("corbanameUrl=" + corbanameUrl);

		if (corbanameUrl == null || "".equals(corbanameUrl)) {
			LOG.error("CRB000142_CorbanameURL_NotFound");
			throw new Jbi4CorbaException("CRB000142_CorbanameURL_NotFound");
		}

		java.lang.Object retrievedObject = orb.string_to_object(corbanameUrl);
		LOG.debug("retrievedObject=" + retrievedObject);

		if (retrievedObject == null) {
			String msg = MESSAGES.getString(
					"CRB000148_Unable_to_retrived_object_using_corbaname",
					new java.lang.Object[] { corbanameUrl });
			LOG.error(msg);
			throw new Jbi4CorbaDeployException(msg);

		}

		return retrievedObject;
	}

	/**
	 * This Method return narrowed Corba Object starting from the IOR
	 * 
	 * @param String
	 *            ior
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 * 
	 */
	public java.lang.Object getCorbaObjectReference(String ior)
			throws Jbi4CorbaException {

		LOG.debug("File.IOR=" + ior);

		if (ior == null || "".equals(ior)) {
			LOG.error("CRB000143_IOR_NotFound");
			throw new Jbi4CorbaException("CRB000143_IOR_NotFound");
		}

		java.lang.Object retrievedObject = null;

		try {
			retrievedObject = getNarrowMethod(helperClass).invoke(helperClass,
					orb.string_to_object(ior));
			// retrievedObject=helperClass.getDeclaredMethod("narrow",
			// org.omg.CORBA.Object.class).invoke(helperClass,
			// orb.string_to_object(ior));

		} catch (IllegalAccessException e) {
			String msg = MESSAGES
					.getString("CRB000153_Error_in_getting_corba_object_reference");
			LOG.error(msg, e);
			throw new Jbi4CorbaException(e);
		} catch (InvocationTargetException e) {
			String msg = MESSAGES
					.getString("CRB000153_Error_in_getting_corba_object_reference");
			LOG.error(msg, e);
			throw new Jbi4CorbaException(e);
		}

		return retrievedObject;
	}

	/**
	 * Localize the CORBA servant using the IOR.
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected java.lang.Object localizationViaIOR() throws Jbi4CorbaException {
		String filename = serviceDescriptor.getCorbaServiceName();
		LOG.debug("filename=" + filename);

		if (filename == null || "".equals(filename)) {
			LOG.error("CRB000143_IOR_NotFound");
			throw new Jbi4CorbaException("CRB000143_IOR_NotFound");
		}

		File iorFile = new File(filename);
		if (!iorFile.exists()) {
			LOG.error("CRB000143_IOR_NotFound");
			throw new Jbi4CorbaException("CRB000143_IOR_NotFound");
		}

		String ior = readIorFromFile(filename);
		LOG.debug("File.IOR=" + ior);

		if (ior == null || "".equals(ior)) {
			LOG.error("CRB000143_IOR_NotFound");
			throw new Jbi4CorbaException("CRB000143_IOR_NotFound");
		}

		java.lang.Object retrievedObject = orb.string_to_object(ior);
		LOG.debug("retrievedObject=" + retrievedObject);

		return retrievedObject;
	}

	/**
	 * 
	 * @param filename
	 *            The file name
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba Exception
	 */
	protected String readIorFromFile(String filename) throws Jbi4CorbaException {
		String ior = null;
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(filename));

			ior = br.readLine();
		} catch (IOException e) {
			java.lang.Object[] args = { filename };
			LOG.error("CRB000144_ReadingFileError", args, e);
			throw new Jbi4CorbaException("CRB000144_ReadingFileError", args, e);
		}

		return ior;
	}

	/**
	 * This method retrieves the corba object reference using the 'NameService'.
	 * 
	 * @return The retrieved Object.
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected java.lang.Object localizationViaNameService()
			throws Jbi4CorbaException {

		// get the root naming context
		org.omg.CORBA.Object objRef;

		// resolve initial references
		try {

			objRef = orb
					.resolve_initial_references(ProviderServiceDescriptor.NAMESERVICE);

		} catch (InvalidName e) {
			java.lang.Object[] args = new java.lang.Object[] {
					ProviderServiceDescriptor.NAMESERVICE, orb, e.getMessage() };

			LOG.error("CRB000131_Invalid_initial_reference_name", args, e);
			throw new Jbi4CorbaException(
					"CRB000131_Invalid_initial_reference_name", args, e);
		} catch (SystemException se) {
			// DA VERIFICARE
			java.lang.Object[] args = new java.lang.Object[] {
					ProviderServiceDescriptor.NAMESERVICE, orb, se.getMessage() };
			LOG.error("CRB000131_Invalid_initial_reference_name", args, se);
			throw new Jbi4CorbaException(
					"CRB000131_Invalid_initial_reference_name", args, se);
		}

		// Use NamingContextExt instead of NamingContext. This is
		// part of the Interoperable naming Service.
		NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		LOG.debug("name service: " + ncRef);
		if (ncRef == null) {
			throw new Jbi4CorbaException("CRB000132_Name_service_context_null");
		}

		// resolve the Object Reference in Naming
		java.lang.Object retrievedObject = null;
		try {

			retrievedObject = ncRef.resolve_str(serviceDescriptor
					.getCorbaServiceName());

		} catch (NotFound e) {
			java.lang.Object[] args = new java.lang.Object[] { "NotFound",
					serviceDescriptor.getCorbaServiceName(), ncRef,
					e.getMessage() };

			LOG.error("CRB000134_Exception_retrieving_corba_object_reference",
					args, e);
			throw new Jbi4CorbaException(
					"CRB000134_Exception_retrieving_corba_object_reference",
					args, e);

		} catch (CannotProceed e) {
			java.lang.Object[] args = new java.lang.Object[] { "CannotProceed",
					serviceDescriptor.getCorbaServiceName(), ncRef,
					e.getMessage() };

			LOG.error("CRB000134_Exception_retrieving_corba_object_reference",
					args, e);
			throw new Jbi4CorbaException(
					"CRB000134_Exception_retrieving_corba_object_reference",
					args, e);

		} catch (org.omg.CosNaming.NamingContextPackage.InvalidName e) {
			java.lang.Object[] args = new java.lang.Object[] { "InvalidName",
					serviceDescriptor.getCorbaServiceName(), ncRef,
					e.getMessage() };

			LOG.error("CRB000134_Exception_retrieving_corba_object_reference",
					args, e);
			throw new Jbi4CorbaException(
					"CRB000134_Exception_retrieving_corba_object_reference",
					args, e);
		}

		LOG.debug("retrievedObject=" + retrievedObject);
		return retrievedObject;
	}

	/**
	 * Gets the object that represent the method 'narrow' of the helper class.
	 * 
	 * @param helperClass
	 *            The helper class.
	 * 
	 * @return The narrow method
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba Exception
	 */
	protected Method getNarrowMethod(Class helperClass)
			throws Jbi4CorbaException {

		Method narrowMethod;
		try {

			narrowMethod = helperClass
					.getDeclaredMethod("narrow", Object.class);

		} catch (SecurityException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"SecurityException", helperClass, e.getMessage() };

			LOG.error("CRB000133_Exception_retrieving_narrow_method", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_narrow_method", args, e);
		} catch (NoSuchMethodException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"NoSuchMethodException", helperClass, e.getMessage() };

			LOG.error("CRB000133_Exception_retrieving_narrow_method", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_narrow_method", args, e);
		}

		return narrowMethod;
	}

	/**
	 * 
	 * @param helperClass
	 *            The helper class
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected Method getExtractMethod(Class helperClass)
			throws Jbi4CorbaException {

		Method extractMethod = null;

		try {

			extractMethod = helperClass.getDeclaredMethod("extract", Any.class);

		} catch (SecurityException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"SecurityException", helperClass, e.getMessage() };
			LOG.error("CRB000133_Exception_retrieving_extractMethod", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_extractMethod", args, e);
		} catch (NoSuchMethodException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"NoSuchMethodException", helperClass, e.getMessage() };
			LOG.error("CRB000133_Exception_retrieving_extractMethod", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_extractMethod", args, e);
		}

		return extractMethod;
	}

	/**
	 * 
	 * @param helperClass
	 *            The helper class
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	protected Method getTypeMethod(Class helperClass) throws Jbi4CorbaException {
		Method typeMethod = null;

		try {

			typeMethod = helperClass.getDeclaredMethod("type");

		} catch (SecurityException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"SecurityException", helperClass, e.getMessage() };
			LOG.error("CRB000133_Exception_retrieving_typeMethod", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_typeMethod", args, e);
		} catch (NoSuchMethodException e) {
			java.lang.Object[] args = new java.lang.Object[] {
					"NoSuchMethodException", helperClass, e.getMessage() };
			LOG.error("CRB000133_Exception_retrieving_typeMethod", args, e);
			throw new Jbi4CorbaException(
					"CRB000133_Exception_retrieving_typeMethod", args, e);
		}

		return typeMethod;
	}

	/**
	 * 
	 * @param prop
	 *            The prop
	 */
	private void debugProperties(Properties prop) {
		if (!LOG.isDebugEnabled()) {
			// no log
			return;
		}
		// else
		if (prop == null || prop.size() == 0) {
			LOG.debug("No properties found.");
			return;
		}
		// else
		for (java.lang.Object k : prop.keySet()) {
			LOG.debug("Properties[" + k + "]=" + prop.getProperty((String) k));
		}

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
		LOG.info("CRB000150_Service_unregistered", new java.lang.Object[] {
				this.getServiceName(), this.getEndpointName() });
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
	 * Return the orb
	 * 
	 */
	public ORB getOrb() {
		return orb;
	}

}
