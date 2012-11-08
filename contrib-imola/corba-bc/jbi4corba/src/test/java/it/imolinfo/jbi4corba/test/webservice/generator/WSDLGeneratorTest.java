 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.exception.WSDLGenerationException;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtension;
import it.imolinfo.jbi4corba.schema.DefinitionAndSchema;
import it.imolinfo.jbi4corba.schema.SchemaUtil;
import it.imolinfo.jbi4corba.utils.HelperStringUtils;
import it.imolinfo.jbi4corba.utils.IdlFileDataHolder;
import it.imolinfo.jbi4corba.webservice.generator.WSDLDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.WSDLGenerator;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.TestCase;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.Difference;
import org.custommonkey.xmlunit.DifferenceConstants;
import org.custommonkey.xmlunit.DifferenceListener;
import org.custommonkey.xmlunit.ElementNameAndAttributeQualifier;
import org.custommonkey.xmlunit.NodeDetail;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

public class WSDLGeneratorTest extends TestCase {

	public WSDLFactory factory = null;

	public WSDLReader reader = null;

	public WSDLWriter writer = null;

    @Override
	public void setUp() {

		try {
			factory = WSDLFactory.newInstance();
			reader = factory.newWSDLReader();
			writer = factory.newWSDLWriter();

		} catch (WSDLException e) {
			e.printStackTrace();
		}
	}

	public void testWSDLGenerator() throws ClassGenerationException,
			IOException, WSDLException {

		File idl = new File("src/test/etc/idl/Echo.idl");
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		String[] wsdlExpected = {"src/test/etc/wsdl/Echo_expected_without_schema.wsdl"};
		String[] schemaExpected = {};
		Properties p = new Properties();
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);

		descList.add(desc);
		
		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}
	
	public void compareWSDL(File idl, ArrayList<WSDLDescriptor> descList, String[] wsdlExpected,String[] schemaExpected, int numInterface){
		
		File wsdl = null;
		File schemaFile = null;
		
		try {

			WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
			List<Definition> defList = generator.generateWSDLListfromIDL(idl, descList);
			assertEquals(defList.size(), numInterface);
			DefinitionAndSchema defSchema = generator.createRemoveXSD(defList);
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder mBuilder = factory.newDocumentBuilder();
		
			int index = 0;
			for(Definition def : defSchema.getDefinitions()){
				assertNotNull(def);
				wsdl = File.createTempFile("wsdl_created_during_test_"+index+"_", null);
				writeDefinition(def, wsdl);
				Document actualDoc = mBuilder.parse(wsdl);
				boolean equals = false;
				for(int i=0; i < wsdlExpected.length; i++){
					Document expectedDoc = mBuilder.parse(new File(wsdlExpected[i]));
					Diff diff = new Diff(expectedDoc, actualDoc);
					diff.overrideElementQualifier(new ElementNameAndAttributeQualifier());
					if(diff.similar()){
						equals = true;
					} else {
						// System.out.println(diff);
					}
				}
				assertTrue(equals);
				index++;
				wsdl.delete();
			}
			
			index = 0;
			
            //Write XSD
			String tempDir = System.getProperty("java.io.tmpdir");
			List<String> schemaFileNames = SchemaUtil.createXSD(defSchema.getSchemas(), tempDir , "TypeDef");
            if(defSchema.isContainsW3c()){
                   try {
					SchemaUtil.createW3CSchema(tempDir);
					schemaFileNames.add(tempDir + File.separator + "ws-addr.xsd");
				} catch (URISyntaxException e) {
					e.printStackTrace();
				}
            }
                                                            
			// assertTrue("schemaExpected : "+schemaExpected.length +" - schema : "+defSchema.getSchemas().size(),schemaExpected.length == defSchema.getSchemas().size());
			// for(String schema : schemaFileName..getSchemas()){
			for(int k = 0; k < schemaFileNames.size(); k++) {
				String schemaFileName = schemaFileNames.get(k);
				assertNotNull(schemaFileName);
				schemaFile =  new File(schemaFileName);
								
				Document actualDoc = mBuilder.parse(schemaFile);
				boolean equals = false;
				for(int i=0; i < schemaExpected.length; i++){
					Document expectedDoc = mBuilder.parse(new File(schemaExpected[i]));
					XMLUnit.setIgnoreWhitespace(true); 
					XMLUnit.setIgnoreComments(true);  
					Diff diff = new Diff(expectedDoc, actualDoc);
					diff.overrideDifferenceListener(new MyDifferenceLister());
					diff.overrideElementQualifier(new ElementNameAndAttributeQualifier());					
					if(diff.similar()){						
						equals = true;
					} else {				
						// System.out.println("Differences:" + diff);
					}
				}
				assertTrue(equals);
				index++;
				schemaFile.delete();
			}
			
		} catch (IOException ioex) {
			ioex.printStackTrace();
			fail(ioex.getMessage());
		} catch (WSDLException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (TransformerException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (SAXException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (ClassGenerationException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (WSDLGenerationException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} catch (Jbi4CorbaException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} finally {
			if(wsdl != null){
				//wsdl.delete();
			}
			if(schemaFile != null){
				//schemaFile.delete();
			}
		}
	}

	// Testing WSDL generation from IDL with multiple interfaces
	public void testSimpleMultipleWSDLGenerator()
			throws ClassGenerationException, IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoMultiple.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService",
				"it.imolinfo.jbi4corba.test.webservice.generator.echoComplex",
				"Echo");
		WSDLDescriptor desc1 = new WSDLDescriptor("CORBA_SERVICE_NAME2",
				"NameService2",
				"it.imolinfo.jbi4corba.test.webservice.generator.echoComplex",
				"Echo2");

		String[] wsdlExpected = {"src/test/etc/wsdl/EchoMultInterface_expected0_without_schema.wsdl","src/test/etc/wsdl/EchoMultInterface_expected1_without_schema.wsdl"};
		String[] schemaExpected = {};
		
		Properties p = new Properties();
		Properties p1 = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");

		p1.put("qui", "1");
		p1.put("quo", "2");
		p1.put("qua", "");

		desc.setOrbProperties(p);
		desc1.setOrbProperties(p1);
		descList.add(desc);
		descList.add(desc1);

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 2);
		
	}

	// Testing WSDL generation from IDL with multiple interfaces
	public void testMultipleWSDLGenerator() throws ClassGenerationException,
			IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoMultipleComplex.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService",
				"it.imolinfo.jbi4corba.test.webservice.generator.echoComplex",
				"EchoComplex");
		WSDLDescriptor desc1 = new WSDLDescriptor("CORBA_SERVICE_NAME2",
				"NameService2",
				"it.imolinfo.jbi4corba.test.webservice.generator.echoComplex",
				"EchoComplex2");

		Properties p = new Properties();
		Properties p1 = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");

		p1.put("qui", "1");
		p1.put("quo", "2");
		p1.put("qua", "");

		desc.setOrbProperties(p);
		desc1.setOrbProperties(p1);
		descList.add(desc);
		descList.add(desc1);

		// String[] wsdlExpected={"src/test/etc/wsdl/EchoMultipleComplex_expected_0.wsdl","src/test/etc/wsdl/EchoMultipleComplex_expected_1.wsdl"};
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoMultipleComplex_expected_0_without_schema.wsdl","src/test/etc/wsdl/EchoMultipleComplex_expected_1_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoMultipleComplex_expected_schema.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 2);
	}

	// Testing WSDL generation from IDL with multiple interfaces
	// with interface that returns an interface
	public void testSimpleMultDynWSDLGenerator()
			throws ClassGenerationException, IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoMultipleWithDynInt.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("Echo", "NameService",
				"http://it.imolinfo.jbi4corba.test.testechoMultdyn", "Echo");
		WSDLDescriptor desc1 = new WSDLDescriptor("Echo2", "NameService",
				"http://it.imolinfo.jbi4corba.test.testechoMultdyn", "Echo2");

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		desc.setOrbProperties(p);
		desc1.setOrbProperties(p);
		descList.add(desc);
		descList.add(desc1);

		String[] wsdlExpected = {"src/test/etc/wsdl/EchoMultDyn_expected0_without_schema.wsdl","src/test/etc/wsdl/EchoMultDyn_expected1_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoMultDyn_schema_expected0.xml","src/test/etc/wsdl/xsd/ws-addr.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 2);
		
	}

	public void testWSDLGeneratorWithFaults() throws ClassGenerationException,
			IOException, WSDLException {
		File idl = new File("src/test/etc/idl/EchoFault.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("EchoFault", "NameService",
				"it.imolinfo.jbi4corba.test.webservice.generator.fault",
				"EchoFaultJBIPort");

		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/Echo_fault_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/Echo_fault_expected_schema0.xml","src/test/etc/wsdl/xsd/Echo_fault_expected_schema1.xml"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);		
	}

	public void testWSDLGeneratorSimpleSequenceINOUT()
			throws ClassGenerationException, IOException, WSDLException {
		File idl = new File("src/test/etc/idl/SimpleSequenceINOUT.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/SimpleSequenceINOUT_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/TypeDef_foo.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}

//	public void _testWSDLGeneratorSimpleSequenceOfSequenceINOUT()
//			throws ClassGenerationException, IOException, WSDLException {
//		File idl = new File(
//				"src/test/etc/idl/SimpleSequenceOfSequenceINOUT.idl");
//		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
//		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
//				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
//		Properties p = new Properties();
//
//		p.put("qui", "1");
//		p.put("quo", "2");
//		p.put("qua", "");
//		desc.setOrbProperties(p);
//		descList.add(desc);
//
//		// String[] wsdlExpected={"src/test/etc/wsdl/Echo_expected.wsdl"};
//		String[] wsdlExpected = {"",""};
//		String[] schemaExpected = {""};
//
//		compareWSDL(idl, descList, wsdlExpectedWithoutSchema, schemaExpected, 1);
//	}

	public void testWSDLGeneratorSequence() throws ClassGenerationException,
			IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoSequence.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);

		String[] wsdlExpected = {"src/test/etc/wsdl/EchoSequence_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoSequence_expected.xsd", "src/test/etc/wsdl/xsd/TypeDef_array_sequence.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);

	}

	public void testWSDLGeneratorUnions() throws ClassGenerationException,
			IOException, WSDLException {

		File idl = new File("src/test/etc/idl/UnionTypes.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("Echo", "NameService",
				"it.imolinfo.jbi4corba.test.Echo", "Echo");
		WSDLDescriptor desc1 = new WSDLDescriptor("Echo1", "NameService",
				"it.imolinfo.jbi4corba.test.Echo1", "Echo1");

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		desc.setOrbProperties(p);
		desc1.setOrbProperties(p);

		descList.add(desc);
		descList.add(desc1);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/UnionTypes_wsdl_0_expected_without_scheam.wsdl","src/test/etc/wsdl/UnionTypes_wsdl_1_expected_without_scheam.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/UnionTypes_wsdl_expected_scheam.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 2);

	}

	public void testWSDLGeneratorAnys() throws ClassGenerationException,
			IOException, WSDLException {

		File idl = new File("src/test/etc/idl/AnyTypes.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("EchoAny", "NameService",
				"it.imolinfo.jbi4corba.test", "EchoAny");

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		desc.setOrbProperties(p);

		descList.add(desc);

		String[] wsdlExpected = {"src/test/etc/wsdl/AnyTypes_wsdl_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/TypeDef_array_anytype.xsd","src/test/etc/wsdl/xsd/AnyTypes_wsdl_expected_schema.xml"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);

	}

	public void testWSDLGeneratorFaultImportError()
			throws ClassGenerationException, IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoFaultImportError.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);

		descList.add(desc);

		String[] wsdlExpected = {"src/test/etc/wsdl/EchoFaultImportError_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoFaultImportError_expected_schema0.xml","src/test/etc/wsdl/xsd/EchoFaultImportError_expected_schema1.xml"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);

	}

	public void testWSDLGeneratorSimpleIncludes() {

		String idlFile = "src/test/etc/idl/EchoWithIncludes.idl";
		
		File idl = new File(idlFile);

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("EchoAny", "NameService",
				"it.imolinfo.jbi4corba.test", "EchoAny");
		Properties p = new Properties();
		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		desc.setOrbProperties(p);

		descList.add(desc);
			
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoWithIncludes_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoWithIncludes_expected_schema0.xml","src/test/etc/wsdl/xsd/EchoWithIncludes_expected_schema1.xml"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}

	public void testWSDLGeneratorMultiLevelIncludes() {
		String idlFile = "src/test/etc/idl/EchoWithMultiLevelIncludes.idl";
		
		File idl = new File(idlFile);

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("EchoAny", "NameService",
				"it.imolinfo.jbi4corba.test", "EchoAny");
		Properties p = new Properties();
		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoWithMultiLevelIncludes_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/TypeDef_array.xsd","src/test/etc/wsdl/xsd/EchoWithMultiLevelIncludes_expected_schema0.xml","src/test/etc/wsdl/xsd/EchoWithMultiLevelIncludes_expected_schema1.xml"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}

	
	/**
	 * Test Complex-InOUT
	 * @throws Jbi4CorbaException
	 **/
	
//	public void testWSDLGeneratorMultiModule() throws IOException, WSDLException,
//	Jbi4CorbaException {
//		
//		File idl = new File("src/test/etc/idl/EchoComplexInOut.idl");
//
//		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
//		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
//		List<Definition> defList = null;
//
//		List<IdlFileDataHolder> idldataholder = null;
//		
//		idldataholder = generator.getIdlFileData(idl);
//
//		
//
//		Properties p = new Properties();
//
//		p.put("org.omg.CORBA.ORBInitialPort", "1050");
//		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
//		p.put("org.omg.CORBA.ORBInitialHost", "localhost");
//
//		for (IdlFileDataHolder holder : idldataholder) {
//			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
//					"NameService", holder.getInterfaceNameSpace(), holder
//							.getInterfaceName());
//			desc.setOrbProperties(p);
//			descList.add(desc);
//		}
//
//		
//
//			defList = generator.generateWSDLListfromIDL(idl, descList);
//		
//
//		File wsdl = null;
//		int index = 0;
//		for (Definition def : defList) {
//
//			try {
//				wsdl = File.createTempFile(def.getQName().getLocalPart()
//						+ ".wsdl", null);
//
//				// Writes out the definition
//				writeDefinition(def, wsdl);
//
//				boolean sameXML = compareFilesWithDOM(new File(
//						"src/test/etc/wsdl/InOut"
//								+ def.getQName().getLocalPart()
//								+ "_expected.wsdl"), wsdl);
//				assertTrue(sameXML);
//				wsdl.delete();
//				index++;
//			} catch (Exception ex) {
//				ex.printStackTrace();
//				fail(ex.getMessage());
//			} finally {
//				wsdl.delete();
//			}
//		}
//
//	}
	
//	/**
//	 *	Test WSDL generation starting from idl contains Object interface
//	 */
//	public void testWSDLGeneratorIDLWithObject() throws IOException, WSDLException,
//	Jbi4CorbaException {
//
//		File idl = new File("src/test/etc/idl/IDLWithObject.idl");
//
//		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
//		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
//		List<Definition> defList = null;
//
//		List<IdlFileDataHolder> idldataholder = null;
//
//		idldataholder = generator.getIdlFileData(idl);
//
//		Properties p = new Properties();
//
//		p.put("org.omg.CORBA.ORBInitialPort", "1050");
//		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
//		p.put("org.omg.CORBA.ORBInitialHost", "localhost");
//
//		for (IdlFileDataHolder holder : idldataholder) {
//			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
//					"NameService", holder.getInterfaceNameSpace(), holder
//							.getInterfaceName());
//			desc.setOrbProperties(p);
//			descList.add(desc);
//		}
//
//		defList = generator.generateWSDLListfromIDL(idl, descList);
//
//		File wsdl = null;
//		int index = 0;
//		for (Definition def : defList) {
//
//			try {
//				wsdl = File.createTempFile(def.getQName().getLocalPart()
//						+ ".wsdl", null);
//
//				// Writes out the definition
//				writeDefinition(def, wsdl);
//
//				boolean sameXML = compareFilesWithDOM(new File(
//						"src/test/etc/wsdl/"
//								+ def.getQName().getLocalPart()
//								+ "_expected.wsdl"), wsdl);
//				assertTrue(sameXML);
//				wsdl.delete();
//				index++;
//			} catch (Exception ex) {
//				ex.printStackTrace();
//				fail(ex.getMessage());
//			} finally {
//				wsdl.delete();
//			}
//		}
//
//	}
//	
	/**
	 * Test Complex-InOUT
	 * @throws Jbi4CorbaException
	 **/
	
	public void testWSDLGeneratorComplexInOut() throws IOException, WSDLException,
	Jbi4CorbaException {
		
		File idl = new File("src/test/etc/idl/EchoComplexInOut.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
		List<Definition> defList = null;
		List<IdlFileDataHolder> idldataholder = null;
		idldataholder = generator.getIdlFileData(idl);
		Properties p = new Properties();
		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		for (IdlFileDataHolder holder : idldataholder) {
			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
					"NameService", holder.getInterfaceNameSpace(), holder
							.getInterfaceName());
			desc.setOrbProperties(p);
			descList.add(desc);
		}

		defList = generator.generateWSDLListfromIDL(idl, descList);

		String[] wsdlExpected = {"src/test/etc/wsdl/InOut_expected_without_schema.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/InOut_expected_schema.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, defList.size());
		
	}

	/**
	 * Nokia IDL- WSDL generation Test
	 * 
	 * @throws Jbi4CorbaException
	 */

	public void testWSDLGeneratorNokiaIdl() throws IOException, WSDLException,
			Jbi4CorbaException {

		File idl = new File("src/test/etc/idl3gpp/AlarmIRPSystem.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
		List<Definition> defList = null;

		List<IdlFileDataHolder> idldataholder = null;

		idldataholder = generator.getIdlFileData(idl);

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		for (IdlFileDataHolder holder : idldataholder) {
			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
					"NameService", holder.getInterfaceNameSpace(), holder
							.getInterfaceName());
			desc.setOrbProperties(p);
			descList.add(desc);
		}

		defList = generator.generateWSDLListfromIDL(idl, descList);

		String[] wsdlExpected = new String[defList.size()];
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/ws-addr.xsd",
								   "src/test/etc/wsdl/xsd/TypeDef_array_anytype.xsd",
									"src/test/etc/wsdl3gpp/xsd/TypeDef_TimeBase.xsd",
									"src/test/etc/wsdl3gpp/xsd/TypeDef_ManagedGenericIRPConstDefs.xsd",
									"src/test/etc/wsdl3gpp/xsd/TypeDef_CosNotification.xsd",
									"src/test/etc/wsdl3gpp/xsd/TypeDef_AlarmIRPConstDefs.xsd"};
		for(int i=0; i<defList.size();i++){
			wsdlExpected[i]="src/test/etc/wsdl3gpp/"+ defList.get(i).getQName().getLocalPart()+ "_expected.wsdl";
		}
		compareWSDL(idl, descList, wsdlExpected, schemaExpected, defList.size());

	}
        
        /**
	 * Nokia IDL- WSDL generation Test
	 * 
	 * @throws Jbi4CorbaException
	 */

	public void _testWSDLGeneratorIncludeComplexIdl() throws IOException, WSDLException,
			Jbi4CorbaException {

		File idl = new File("src/test/etc/IncludeComplex/TestInclude.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
		List<Definition> defList = null;

		List<IdlFileDataHolder> idldataholder = null;

		idldataholder = generator.getIdlFileData(idl);

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		for (IdlFileDataHolder holder : idldataholder) {
			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
					"NameService", holder.getInterfaceNameSpace(), holder
							.getInterfaceName());
			desc.setOrbProperties(p);
			descList.add(desc);
		}

		defList = generator.generateWSDLListfromIDL(idl, descList);
		String[] wsdlExpected = {"src/test/etc/wsdl/foo_0_expected_without_schema.wsdl","src/test/etc/wsdl/foo_1_expected_without_schema.wsdl"};
		String[] schemaExpected = {};
		//for(int i=0; i<defList.size();i++){
		//	wsdlExpected[i]= "src/test/etc/wsdl/"+ defList.get(i).getQName().getLocalPart()+ "_expected.wsdl";
		//}
		compareWSDL(idl, descList, wsdlExpected, schemaExpected, defList.size());

	}
	
	/**
	 * Nokia IDL- WSDL generation Test
	 * 
	 * @throws Jbi4CorbaException
	 */

	public void testWSDLGeneratorWhitoutModule() throws IOException, WSDLException,
			Jbi4CorbaException {

		File idl = new File("src/test/etc/idl/EchoMultipleWithoutModule.idl");

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
		List<Definition> defList = null;

		List<IdlFileDataHolder> idldataholder = null;

		idldataholder = generator.getIdlFileData(idl);

		Properties p = new Properties();

		p.put("org.omg.CORBA.ORBInitialPort", "1050");
		p.put("org.omg.CORBA.ORBClass", "com.sun.corba.ee.impl.orb.ORBImpl");
		p.put("org.omg.CORBA.ORBInitialHost", "localhost");

		for (IdlFileDataHolder holder : idldataholder) {
			WSDLDescriptor desc = new WSDLDescriptor(holder.getInterfaceName(),
					"NameService", holder.getInterfaceNameSpace(), holder
							.getInterfaceName());
			desc.setOrbProperties(p);
			descList.add(desc);
		}

		defList = generator.generateWSDLListfromIDL(idl, descList);

		String[] wsdlExpected = {"src/test/etc/wsdl/EchoWModule_expected_without_schema0.wsdl","src/test/etc/wsdl/EchoWModule_expected_without_schema1.wsdl","src/test/etc/wsdl/EchoWModule_expected_without_schema2.wsdl","src/test/etc/wsdl/EchoWModule_expected_without_schema3.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/ws-addr.xsd"};
		//for(int i=0; i<defList.size();i++){
		//	wsdlExpected[i]= "src/test/etc/wsdl/"+ defList.get(i).getQName().getLocalPart()+ "WModule_expected.wsdl";
		//}
		compareWSDL(idl, descList, wsdlExpected, schemaExpected, defList.size());

	}



	// Uncomment for INOUT test
	// public void testWSDLGeneratorWithInOut()
	// throws ClassGenerationException, IOException, WSDLException {
	// File idl = new File("src/test/etc/idl/EchoInOut.idl");
	// File wsdl = File.createTempFile("Echo_wsdl_created_during_test", null);
	// WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
	// "NameService", "NAME_SPACE", "ENDPOINT_NAME");
	// Properties p = new Properties();
	// WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
	//
	// p.put("qui", "1");
	// p.put("quo", "2");
	// p.put("qua", "");
	// desc.setOrbProperties(p);
	//
	// try {
	//
	// Definition def = generator.generateWSDLfromIDL(idl, desc);
	// assertNotNull(def);
	//    
	// // Writes out the definition
	// writeDefinition(def, wsdl);
	//    
	// boolean sameXML = compareFilesWithDOM(new
	// File("src/test/etc/wsdl/Echo_expected.wsdl"), wsdl);
	// assertTrue(sameXML);
	//    
	// } catch (Exception ex) {
	// ex.printStackTrace();
	// fail(ex.getMessage());
	// } finally {
	// wsdl.delete();
	// }
	// }
	//    
	// public void testComplexWSDLGeneratorWithInOut()
	// throws ClassGenerationException, IOException, WSDLException {
	// File idl = new File("src/test/etc/idl/EchoComplexInOut.idl");
	// File wsdl = File.createTempFile("Echo_wsdl_created_during_test", null);
	// WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
	// "NameService", "NAME_SPACE", "ENDPOINT_NAME");
	// Properties p = new Properties();
	// WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
	//
	// p.put("qui", "1");
	// p.put("quo", "2");
	// p.put("qua", "");
	// desc.setOrbProperties(p);
	//
	// try {
	//
	// Definition def = generator.generateWSDLfromIDL(idl, desc);
	// assertNotNull(def);
	//
	// // Writes out the definition
	// writeDefinition(def, wsdl);
	//
	// boolean sameXML = compareFilesWithDOM(new
	// File("src/test/etc/wsdl/Echo_expected.wsdl"), wsdl);
	// assertTrue(sameXML);
	//
	// } catch (Exception ex) {
	// ex.printStackTrace();
	// fail(ex.getMessage());
	// } finally {
	// wsdl.delete();
	// }
	// }

	// private static int compareFiles(
	// final File expected, final File actual) throws IOException {
	// BufferedReader expectedR = new BufferedReader(new FileReader(expected));
	// BufferedReader actualR = new BufferedReader(new FileReader(actual));
	// int rowCompared = 0;
	// String exp = expectedR.readLine();
	// String act = actualR.readLine();
	//
	// while ((exp != null) && (act != null)) {
	// exp = exp.trim();
	// act = act.trim();
	// assertEquals("File content mismatch", exp, act);
	//
	// rowCompared++;
	//
	// exp = expectedR.readLine();
	// act = actualR.readLine();
	// }
	//
	// // Catch errors where only one of 'exp' and 'act' is null
	// assertEquals("File content mismatch", exp, act);
	//
	// expectedR.close();
	// actualR.close();
	//
	// return rowCompared;
	// }

	/**
	 * Compares two XML using DOM.
	 * 
	 * @param expected
	 * @param actual
	 * @return
	 * @throws Exception
	 */
	public static boolean compareFilesWithDOM(final File expected,
			final File actual) throws Exception {

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder mBuilder;

		boolean domEquals = false;

		try {
			mBuilder = factory.newDocumentBuilder();
			Document expectedDoc = mBuilder.parse(expected);
			Document actualDoc = mBuilder.parse(actual);
			expectedDoc.normalize();
			expectedDoc.normalizeDocument();
			actualDoc.normalize();
			actualDoc.normalizeDocument();

			String actualDocStr = documentToString(actualDoc, "UTF-8", false);
			String expectedDocStr = documentToString(expectedDoc, "UTF-8",false);

			/*
			 * System.err.println("*****************************************************************************"
			 * ); System.err.println("ActualDoc: "+actualDocStr);
			 * System.err.println(
			 * "*****************************************************************************"
			 * ); System.err.println("ExpectedDoc: "+expectedDocStr);
			 * System.err.println(
			 * "*****************************************************************************"
			 * ); System.err.println("CompareTo: " +
			 * expectedDocStr.compareTo(actualDocStr));System.err.println(
			 * "*****************************************************************************"
			 * );
			 */

			// domEquals = actualDocStr.equals(expectedDocStr);
			domEquals = HelperStringUtils.compareStringNoOrder(actualDocStr,
					expectedDocStr);
			
		} catch (ParserConfigurationException ex) {
			ex.printStackTrace();
			throw new Exception(ex);
		}

		return domEquals;

	}
	
	public void testWSDLGeneratorSimpleTypeDef() throws ClassGenerationException,	IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoSimpleTypeDef.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		
		
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoSimpleTypeDef_expected.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoSimpleTypeDef.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);

	}	
	
	public void testWSDLGeneratorSimpleArraySequenceTypeDef() throws ClassGenerationException,	IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoSimpleArraySequenceTypeDef.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
				
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoSimpleArraySequenceTypeDef_expected.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoSimpleArraySequenceTypeDef.xsd", "src/test/etc/wsdl/xsd/TypeDef_array_simple_typedef.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);

	}		
	
	public void testWSDLGeneratorComplexTypeDef() throws ClassGenerationException,	IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoComplexTypeDef.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoComplexTypeDef_expected.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoComplexTypeDef.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}
	
	public void testWSDLGeneratorComplexArraySequenceTypeDef() throws ClassGenerationException,	IOException, WSDLException {

		File idl = new File("src/test/etc/idl/EchoComplexArraySequenceTypeDef.idl");
		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
		Properties p = new Properties();

		p.put("qui", "1");
		p.put("quo", "2");
		p.put("qua", "");
		desc.setOrbProperties(p);
		descList.add(desc);
		
		String[] wsdlExpected = {"src/test/etc/wsdl/EchoComplexArraySequenceTypeDef_expected.wsdl"};
		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoComplexArraySequenceTypeDef.xsd",
				"src/test/etc/wsdl/xsd/TypeDef_array_complex_typedef_array.xsd"};

		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
	}	
	
//	public void testWSDLGeneratorTypeDefAny() throws ClassGenerationException,	IOException, WSDLException {
//
//		File idl = new File("src/test/etc/idl/EchoTypeDefAny.idl");
//		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
//		WSDLDescriptor desc = new WSDLDescriptor("CORBA_SERVICE_NAME",
//				"NameService", "NAME_SPACE", "ENDPOINT_NAME");
//		Properties p = new Properties();
//
//		p.put("qui", "1");
//		p.put("quo", "2");
//		p.put("qua", "");
//		desc.setOrbProperties(p);
//		descList.add(desc);
//		
//		String[] wsdlExpected = {"src/test/etc/wsdl/EchoComplexArraySequenceTypeDef_expected.wsdl"};
//		String[] schemaExpected = {"src/test/etc/wsdl/xsd/EchoComplexArraySequenceTypeDef.xsd"};
//
//		compareWSDL(idl, descList, wsdlExpected, schemaExpected, 1);
//	}	
		
	

	
	private void writeDefinition(Definition def, File file)
			throws WSDLException, IOException {

		ExtensionRegistry registry = def.getExtensionRegistry();
		Jbi4CorbaExtension.register(registry);

		writer.writeWSDL(def, new FileWriter(file));
	}
	
	

	/**
	 * Converts the xml documet to a String. Useful for logging.
	 * 
	 * @param node
	 *            The node
	 * @param encoding
	 *            The encoding
	 * @param omitXMLDeclaration
	 *            The omit XML declaration
	 * @return The return
	 * @throws WrapperProcessingException
	 *             The wrapper processing exception
	 */
	public static String documentToString(Document doc, String encoding,
			boolean omitXMLDeclaration) throws Exception {
		String ret = null;
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		Transformer trans;
		try {
			trans = TransformerFactory.newInstance().newTransformer();

			trans.setOutputProperty(OutputKeys.ENCODING, encoding);
			trans.setOutputProperty(OutputKeys.INDENT, "yes");
			trans.setOutputProperty(
					"{http://xml.apache.org/xslt}indent-amount", "4");
			trans.setOutputProperty(OutputKeys.METHOD, "xml");
			trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
					omitXMLDeclaration ? "yes" : "no");
			trans.transform(new DOMSource(doc), new StreamResult(baos));
			ret = baos.toString(encoding);
		} catch (TransformerException ex) {
			throw new Exception(ex);
		} catch (UnsupportedEncodingException ex) {
			throw new Exception(ex);
		} catch (TransformerFactoryConfigurationError ex) {
			throw new Exception(ex);
		}
		return ret;
	}
	
    private static class MyDifferenceLister implements DifferenceListener {
        private List<Difference> ignore;

        MyDifferenceLister() {
            ignore = new LinkedList<Difference>();
            ignore.add(DifferenceConstants.HAS_DOCTYPE_DECLARATION);
            ignore.add(DifferenceConstants.NAMESPACE_PREFIX);
            ignore.add(DifferenceConstants.CHILD_NODELIST_SEQUENCE);            
            ignore.add(DifferenceConstants.ATTR_SEQUENCE);
            ignore.add(DifferenceConstants.COMMENT_VALUE);
            ignore.add(DifferenceConstants.NAMESPACE_URI);
        }

        public int differenceFound(Difference difference) {
        	// System.out.println("Difference found: " + difference + ":" + difference.getId());
       
    		// Since sometime the ns changes dinamically, we can have sometimes
    		// type="ns1:utcT"...and sometimes type="ns3:utcT". We have to avoid that.
        	if (difference.getId() == DifferenceConstants.ATTR_VALUE_ID) {        		
        		NodeDetail detail1 = difference.getControlNodeDetail();
        		NodeDetail detail2 = difference.getTestNodeDetail();
        		// For example: ns2:ShortTypeOpt
        		String attr1 = detail1.getValue();
        		// For example: ns1:ShortTypeOpt
        		String attr2 = detail2.getValue();
        		if ((attr1 != null) && (attr2 != null) && (attr1.length() == attr1.length())) {
        			int pos = attr1.indexOf(":");
        			if (pos != -1) {
        				String ns1 = attr1.substring(0, pos-1);
        				String val11 = attr1.substring(pos, attr1.length());
        				String ns2 = attr2.substring(0, pos-1);
        				String val12 = attr2.substring(pos, attr2.length());
        				String toCompare1 = ns1 + val11;
        				String toCompare2 = ns2 + val12;
        				if (toCompare1.equals(toCompare2)) {
        					return RETURN_IGNORE_DIFFERENCE_NODES_IDENTICAL;
        				}
        			}
        		}        		
        	}
            return ignore.contains(difference) ? RETURN_IGNORE_DIFFERENCE_NODES_IDENTICAL : RETURN_ACCEPT_DIFFERENCE;
        }

        /**
         * @see org.custommonkey.xmlunit.DifferenceListener#skippedComparison(org.w3c.dom.Node,
         *      org.w3c.dom.Node)
         */
        public void skippedComparison(Node n1, Node n2) {
        }
    }


}
