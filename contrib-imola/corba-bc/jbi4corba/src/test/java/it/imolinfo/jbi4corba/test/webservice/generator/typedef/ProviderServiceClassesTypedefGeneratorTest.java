 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.test.config.FileHelper;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.webservice.generator.ClientCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.ProviderServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.io.File;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

/**
 * This test case is used to verify the 'idl to java' code generation.
 */
public class ProviderServiceClassesTypedefGeneratorTest extends TestCase {

	/**
	 * The directory where we place the IDLs used to generate the java code.
	 */
	private static final String IDLDIR = "src/test/etc/idl";

	/**
	 * Logger
	 */
	private static Logger log = LoggerFactory
			.getLogger(ProviderServiceClassesTypedefGeneratorTest.class);

	/**
	 * The list of jars for the classpath.
	 */
	private List<String> jarFilesName = new ArrayList<String>();

	/**
	 * Constructor.
	 */
	public ProviderServiceClassesTypedefGeneratorTest(String arg0) {
		super(arg0);

		String repodir = System.getProperty("localRepository");
		log.debug("repodir=" + repodir);

		assertNotNull(repodir);
		assertFalse("".equals(repodir));
		
		jarFilesName = JarListHelper.extractJarList(repodir);
		
	}

	
	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * simple 'typedef'.
	 */
	public void testEchoClassesGenerationSimpleTypesTypeDef() {
		final String idlToTest = "EchoSimpleTypeDef.idl";
		final String targetDir = "target/testEchoClassesGenerationSimpleTypesTypedef";

		log.debug(">>>>> testEchoClassesGenerationTypedef - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// Tests string
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mystring", "java.lang.String");
			
			// Tests int
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myint", "int");
			
			// Tests boolean
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myboolean", "boolean");
			
			// Tests char
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mychar", "char");	

			// Tests wchar
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mywchar", "char");
			
			// Tests double
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mydouble", "double");		
			
			// Tests float
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myfloat", "float");	
			
			// Tests long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mylong", "int");	
			
			// Tests longlong
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mylonglong", "long");		
			
			// Tests short
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myshort", "short");	
			
			// Tests unsigned long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedlong", "int");	
			
			// Tests unsigned long long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedlonglong", "long");	
			
			// Tests unsigned short
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedshort", "short");
			
			// Tests unsigned short
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myany", "java.lang.Object");			

			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationTypedef()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		} finally {
			FileHelper.deleteDir(targetDir);
		}
		log.debug("<<<<< testEchoClassesGenerationTypedef - end");
	}
	
	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * simple 'typedef'.
	 */
	public void testEchoClassesGenerationSimpleArraySequenceTypeDef() {
		final String idlToTest = "EchoSimpleArraySequenceTypeDef.idl";
		final String targetDir = "target/testEchoClassesGenerationSimpleArraySequenceTypesTypedef";

		log.debug(">>>>> testEchoClassesGenerationTypedef - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// Tests string
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mystringArr", "java.lang.String[]");
			
			// Tests int
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myintArr", "int[]");
			
			// Tests boolean
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mybooleanArr", "boolean[]");
			
			// Tests char
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mycharArr", "char[]");	

			// Tests wchar
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mywcharArr", "char[]");
			
			// Tests double
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mydoubleArr", "double[]");		
			
			// Tests float
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myfloatArr", "float[]");	
			
			// Tests long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mylongArr", "int[]");	
			
			// Tests longlong
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.mylonglongArr", "long[]");		
			
			// Tests short
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myshortArr", "short[]");	
			
			// Tests unsigned long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedlongArr", "int[]");	
			
			// Tests unsigned long long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedlonglongArr", "long[]");	
			
			// Tests unsigned short
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.myunsignedshortArr", "short[]");
			
			// Tests matrix string
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.MatrixString", "java.lang.String[][]");
			
			// Tests matrix long
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.MatrixLong", "int[][]");			

			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationTypedef()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		} finally {
			FileHelper.deleteDir(targetDir);
		}
		log.debug("<<<<< testEchoClassesGenerationTypedef - end");
	}
	
	
	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * complex 'typedef'.
	 */
	public void testEchoClassesGenerationComplexTypeDef() {
		final String idlToTest = "EchoComplexTypeDef.idl";
		final String targetDir = "target/testEchoClassesGenerationComplexTypedef";

		log.debug(">>>>> testEchoClassesGenerationTypedef - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myEchoVT",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoVT");
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myMyLong",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.MyLong");		
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myEchoStruct",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStruct");		
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myStructOfStruct",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.StructOfStruct");
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myVTPrimi",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.VTPrimi");	
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myVTPrimiSeq",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.VTPrimiSeq");	
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myValueTypeOfValueType",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.ValueTypeOfValueType");
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myValueTypeOfStruct",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.ValueTypeOfStruct");	
			
			testTypeDefGeneratedClassComplex(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.myStructOfValuetype",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.StructOfValuetype");			

		} catch (Exception e) {
			e.printStackTrace();
			String m = "Error in ... testEchoClassesGenerationTypedef()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		} finally {
			FileHelper.deleteDir(targetDir);
		}
		log.debug("<<<<< testEchoClassesGenerationTypedef - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * complex 'typedef'.
	 */
	public void testEchoClassesGenerationComplexArraySequenceTypeDef() {
		final String idlToTest = "EchoComplexArraySequenceTypeDef.idl";
		final String targetDir = "target/testEchoClassesGenerationComplexArraySequenceTypedef";

		log.debug(">>>>> testEchoClassesGenerationTypedef - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStructSeq",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStruct[]");
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoVTSeq",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoVT[]");	
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.MySequenceVTSeq",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.MySequenceVT[]");		
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStructArr",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStruct[]");	
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStructMatrix",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoStruct[][]");	
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoVTArr",
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.EchoVT[]");	
			
			testTypeDefGeneratedClass(targetDir,
					"it.imolinfo.jbi4corba.test.webservice.generator.echocomplex.MySequenceArr",
					"java.lang.String[][][]");		
			
		} catch (Exception e) {
			e.printStackTrace();
			String m = "Error in ... testEchoClassesGenerationTypedef()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		} finally {
			FileHelper.deleteDir(targetDir);
		}
		log.debug("<<<<< testEchoClassesGenerationTypedef - end");
	}


	// ================================
	// Utility Methods
	// ================================
	/**
	 * The code used to generate the classes.
	 */
	private List<ClientCorbaClassesHolder> testBody(String idlToTest,
			String targetDir) throws ClassGenerationException {
		log.debug(">>>>> testBody - begin");

		JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

		jbiServiceDescriptor.setIdlFileName(idlToTest);
		jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);
		jbiServiceDescriptor.setServiceNameSpace("pippo");

		log.debug("JbiServiceDescriptor created" + "; IdlFileName="
				+ jbiServiceDescriptor.getIdlFileName()
				+ "; IdlFileNameDirectory="
				+ jbiServiceDescriptor.getIdlFileNameDirectory());

		// serviceDescriptor.setIdl_compiler("jacorb");
		ProviderServiceClassesGenerator serviceGenerator = new ProviderServiceClassesGenerator();

		log.debug("ProviderServiceClassesGenerator created.");

		List<ClientCorbaClassesHolder> classes = serviceGenerator
				.generateProviderServiceClasses(jbiServiceDescriptor,
						targetDir, jarFilesName, // new params
						null);

		log.debug("ProviderServiceClassesGenerator"
				+ ".generateProviderServiceClasses(JbiServiceDescriptor"
				+ "serviceDescriptor, String workdir) ... done");

		log.debug("classes: " + classes);
		log.debug("<<<<< testBody - end");
		return classes;
	}
	

	@SuppressWarnings("unchecked")
	private void testTypeDefGeneratedClass(String targetDir, String classToBeGeneratedName, 
			String expectedValueClass) throws ClassGenerationException, SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		
		// TYPEDEF GENERATION
		// Loads the typedefs created classes
		Class myGeneratedClass = Util.classLoad(targetDir + File.separator + "classes", classToBeGeneratedName);
		
		// Tests if the class is related with the correct Helper
		// Field field = myGeneratedClass.getField("helperClass");
		// gets the static value (null);
		//Object fieldValue = field.get(null);		
		//System.out.println("HelperClass:" + ((Class)fieldValue).getName());
		//String createdHelper = ((Class)fieldValue).getName();
		//assertEquals(classToBeGeneratedName + "Helper", createdHelper);
		
		// Tests if the class is related with the correct type
		Field fieldVal = myGeneratedClass.getField("value");
		// gets the static value (null);
		Class fieldType = fieldVal.getType();		
		if (fieldType.isArray()) {			
			System.out.println("AliasedType (as value) name:" + fieldType.getCanonicalName());
			
			assertEquals(expectedValueClass, fieldType.getCanonicalName());
		} else {
			// not an array
			System.out.println("AliasedType (as value) name:" + fieldType.getName());
			assertEquals(expectedValueClass, fieldType.getName());
		}			
	}
	
	@SuppressWarnings("unchecked")
	/**
	 * In this case, we have to test that the extended class is the expected.
	 */
	private void testTypeDefGeneratedClassComplex(String targetDir, String classToBeGeneratedName, 
			String expectedValueClass) throws ClassGenerationException, SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		
		// TYPEDEF GENERATION
		// Loads the typedefs created classes
		Class myGeneratedClass = Util.classLoad(targetDir + File.separator + "classes", classToBeGeneratedName);
		
		// Tests if the class is related with the correct Helper
		// Field field = myGeneratedClass.getField("helperClass");
		// gets the static value (null);
		// Object fieldValue = field.get(null);
		// System.out.println("HelperClass:" + ((Class)fieldValue).getName());
		// String createdHelper = ((Class)fieldValue).getName();
		// assertEquals(classToBeGeneratedName + "Helper", createdHelper);
		
		
		// Tests if the class is related with the correct type
		Class superClass = myGeneratedClass.getSuperclass();
		// gets the static value (null);					
		System.out.println("AliasedType (as superclass) name:" + superClass);
		assertEquals(expectedValueClass, superClass.getName());
	}	

}
