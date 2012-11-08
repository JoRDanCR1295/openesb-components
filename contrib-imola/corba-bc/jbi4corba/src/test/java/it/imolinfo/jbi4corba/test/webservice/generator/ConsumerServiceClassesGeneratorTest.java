 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.webservice.generator.ConsumerServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.ServerCorbaClassesHolder;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;


import org.apache.cxf.jaxws.JaxWsClientFactoryBean;
import org.apache.cxf.service.Service;

/**
 * This class is used to test the code generation for the component in consumer
 * mode.
 * 
 */
public class ConsumerServiceClassesGeneratorTest extends TestCase {

	/**
	 * Logger.
	 */
	private static Logger log = LoggerFactory
			.getLogger(ConsumerServiceClassesGeneratorTest.class);

	/**
	 * The directory where the WSDLs are located.
	 */
	public static final File WSDLDIR = new File("src/test/etc/wsdl/");

	/**
	 * The list of jars for the classpath.
	 */
	private List<String> jarFilesName = new ArrayList<String>();

	public void setUp() {

		String repodir = System.getProperty("localRepository");
		log.debug("repodir=" + repodir);

		assertNotNull(repodir);
		assertFalse("".equals(repodir));

		jarFilesName = JarListHelper.extractJarList(repodir);

	}


	/**
	 * TestCase Construtor.
	 * 
	 * @param arg0
	 */
	public ConsumerServiceClassesGeneratorTest(String arg0) {
		super(arg0);
	}

	public void testEchoClassesGeneration() {
		try {
			JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

			jbiServiceDescriptor.setWsdlURL(WSDLDIR.getAbsolutePath()
					+ "/EchoService.wsdl");

			ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();

			ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
					.generateConsumerServiceClassesDirect(jbiServiceDescriptor
							.getWsdlURL(),
							"target/consumer/testEchoClassesGeneration",
							jarFilesName,null);

			log.debug("classes generated: " + serverCorbaClassesHolder);
		} catch (Exception e) {
			log.error("Error in testEchoClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	// TODO unsupported type
	public void _testXsdBase64BinaryClassesGeneration() {
		try {
			testBody("xsdBase64Binary.wsdl",
					"testXsdBase64BinaryClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testXsdBase64BinaryClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	// TODO unsupported type
	public void _testXsdHexBinaryClassesGeneration() {
		try {
			testBody("xsdHexBinary.wsdl", "testXsdHexBinaryClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testXsdHexBinaryClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	public void testStrangePortTypeNameClassesGeneration() {
		try {
			testBody("strangePortTypeName.wsdl",
					"testStrangePortTypeNameClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testStrangePortTypeNameClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	public void testXsdPrimitivesClassesGeneration() {
		try {
			testBody("xsdPrimitives.wsdl", "testXsdPrimitivesClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testCollectionClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	public void testXsdSimpleTypeAtomicClassesGeneration() {
		try {

			testBody("xsdSimpleType.wsdl",
					"testXsdSimpleTypeAtomicClassesGeneration");

		} catch (Exception e) {
			log.error("Error in testXsdSimpleTypeAtomicClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	/**
	 * FIXME not supported ... yet
	 */
	public void _testXsdSimpleTypeListClassesGeneration() {
		try {
			testBody("xsdSimpleType-list.wsdl",
					"testXsdSimpleTypeListClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testXsdSimpleTypeListClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	public void testXsdComplexTypeSequenceClassesGeneration() {
		try {
			testBody("xsdComplexType-sequence.wsdl",
					"testXsdComplexTypeSequenceClassesGeneration");
		} catch (Exception e) {
			log
					.error(
							"Error in testXsdComplexTypeSequenceClassesGeneration",
							e);
			fail(e.getMessage());
		}
	}

	public void testXsdComplexTypeAllClassesGeneration() {
		try {
			testBody("xsdComplexType-all.wsdl",
					"testXsdComplexTypeAllClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testXsdComplexTypeAllClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	public void testXsdComplexTypeGroupClassesGeneration() {
		try {
			testBody("xsdComplexType-group.wsdl",
					"testXsdComplexTypeGroupClassesGeneration");
		} catch (Exception e) {
			log.error("Error in testXsdComplexTypeGroupClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	// /**
	// * NOTE: the code is not ready for this test!
	// */
	// public void testXsdComplexTypeFaultClassesGeneration() {
	// try {
	//
	// testBody("xsdComplexType-fault.wsdl",
	// "testXsdComplexTypeFaultClassesGeneration");
	//
	// // TODO Add assert
	// } catch (Exception e) {
	// log.error("Error in testXsdComplexTypeFaultClassesGeneration", e);
	// e.printStackTrace();
	// fail(e.getMessage());
	// }
	// }

	public void testXsdSimpleTypeInComplexTypeClassesGeneration() {
		try {
			testBody("xsd-SimpleTypeInComplexType.wsdl",
					"testXsdSimpleTypeInComplexTypeClassesGeneration");
		} catch (Exception e) {
			log.error(
					"Error in testXsdSimpleTypeInComplexTypeClassesGeneration",
					e);
			fail(e.getMessage());
		}
	}

	public void testXsdComplexTypeInComplexTypeClassesGeneration() {
		try {
			testBody("xsd-ComplexTypeInComplexType.wsdl",
					"testXsdComplexTypeInComplexTypeClassesGeneration");
		} catch (Exception e) {
			log
					.error(
							"Error in testXsdComplexTypeInComplexTypeClassesGeneration",
							e);
			fail(e.getMessage());
		}
	}

	public void testCrb106Consumer() {
		try {
			testBody("EchoCrb106Consumer.wsdl", "testCrb106Consumer");
		} catch (Exception e) {
			log.error("Error in testCrb106Consumer", e);
			fail(e.getMessage());
		}
	}

	public void testXsdFaultClassesGeneration() {

		try {
			ServerCorbaClassesHolder holder = testBody("xsdEchoFault.wsdl",
					"testxsdEchoFaultClassesGeneration");
			holder.getUrlClassLoader().loadClass(
					"jbi4corba.test_consumer_fault.EchoExceptionComplex");
			holder.getUrlClassLoader().loadClass(
					"jbi4corba.test_consumer_fault.EchoExceptionSimple");

		} catch (Exception e) {
			log.error("Error in testXsdFaultClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	// public void testEchoEnumWsdl() {
	// try {
	// testBody("EchoEnum.wsdl", "testEchoEnumWsdl");
	// } catch (Exception e) {
	// log.error("Error in testEchoEnumWsdl", e);
	// fail(e.getMessage());
	// }
	// }

	/**
	 * The test for the complx type.
	 */
	public void testXsdSimpleAndComplexClassesGeneration() {
		try {
			ServerCorbaClassesHolder servant = testBodyDirect(
					"xsdSimpleAndComplex.wsdl",
					"testXsdSimpleAndComplexClassesGeneration");

			log.debug("========================== Code Generation ... done.");

			JaxWsClientFactoryBean endpointFactory = CXFUtils
					.getJaxWsClientFactoryBean();

			endpointFactory.setServiceClass(servant.getWebServiceInterface());
			endpointFactory.create();

			// Gets the service model
			Service service = endpointFactory.getServiceFactory().getService();

			if (log.isDebugEnabled()) {
				log.debug("service created: " + service);
			}

			// ByteArrayOutputStream baos = new ByteArrayOutputStream();
			// xfireService.getWSDLWriter().write(baos);
			// log.debug("WSDL:");
			// log.debug(baos.toString() + "\n----");

			log.debug("========================== xfireService ... done");

			// TODO Assertions

		} catch (Throwable e) {
			e.printStackTrace();
			log.error("Error in testXsdSimpleAndComplexClassesGeneration:"
					+ e.getMessage(), e);
			fail(e.getMessage());
		}
	}

	/**
	 * This test is used to verify the code generation one there is a oneway
	 * operation.
	 */
	public void testEchoConsInOnlyServiceConsumer() {
		try {
			testBody("EchoConsInOnlyServiceConsumer.wsdl",
					"testEchoConsInOnlyServiceConsumer");
		} catch (Exception e) {
			log.error("Error in testEchoConsInOnlyServiceConsumer", e);
			fail(e.getMessage());
		}
	}

	/**
	 * @param wsdlFileName
	 *            The WSDL for the code generation.
	 * @param testName
	 *            The name of the test.
	 * 
	 */
	private ServerCorbaClassesHolder testBody(String wsdlFileName,
			String testName) throws Exception {

		JbiServiceDescriptor jbisd = new JbiServiceDescriptor();

		jbisd.setWsdlURL(WSDLDIR.getAbsolutePath() + "/" + wsdlFileName);

		ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();

		ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
				.generateConsumerServiceClassesDirect(jbisd.getWsdlURL(),
						"target/consumer/" + testName, jarFilesName, null);

		log.debug("classes generated: " + serverCorbaClassesHolder);
		return serverCorbaClassesHolder;
	}

	/**
	 * @param wsdlFileName
	 *            The WSDL for the code generation.
	 * @param testName
	 *            The name of the test.
	 * 
	 */
	private ServerCorbaClassesHolder testBodyDirect(String wsdlFileName,
			String testName) throws Exception {

		JbiServiceDescriptor jbisd = new JbiServiceDescriptor();

		jbisd.setWsdlURL(WSDLDIR.getAbsolutePath() + "/" + wsdlFileName);

		ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();

		ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
				.generateConsumerServiceClassesDirect(jbisd.getWsdlURL(),
						"target/consumer/" + testName, jarFilesName,null);

		log.debug("classes generated: " + serverCorbaClassesHolder);
		return serverCorbaClassesHolder;
	}

	public void testEchoFaultClassesGeneration() {
		try {
			JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

			jbiServiceDescriptor.setWsdlURL(WSDLDIR.getAbsolutePath()
					+ "/EchoFault.wsdl");

			ConsumerServiceClassesGenerator serviceGenerator = new ConsumerServiceClassesGenerator();

			ServerCorbaClassesHolder serverCorbaClassesHolder = serviceGenerator
					.generateConsumerServiceClassesDirect(jbiServiceDescriptor
							.getWsdlURL(),
							"target/consumer/testEchoClassesGeneration",
							jarFilesName,null);

			log.debug("classes generated: " + serverCorbaClassesHolder);
		} catch (Exception e) {
			log.error("Error in testEchoClassesGeneration", e);
			fail(e.getMessage());
		}
	}

}
