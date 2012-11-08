 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.webservice.generator.ClientCorbaClassesHolder;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.ProviderServiceClassesGenerator;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.builder.ToStringBuilder;

import junit.framework.TestCase;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

/**
 * This test case is used to verify the 'idl to java' code generation.
 */
public class ProviderServiceClassesGeneratorTest extends TestCase {

	/**
	 * The directory where we place the IDLs used to generate the java code.
	 */
	private static final String IDLDIR = "src/test/etc/idl";

	/**
	 * Logger
	 */
	private static Logger log = LoggerFactory
			.getLogger(ProviderServiceClassesGeneratorTest.class);

	/**
	 * The list of jars for the classpath.
	 */
	private List<String> jarFilesName = new ArrayList<String>();

	/**
	 * Constructor.
	 */
	public ProviderServiceClassesGeneratorTest(String arg0) {
		super(arg0);

		String repodir = System.getProperty("localRepository");
		log.debug("repodir=" + repodir);

		assertNotNull(repodir);
		assertFalse("".equals(repodir));
		
		jarFilesName = JarListHelper.extractJarList(repodir);
		
	}

	private void assertTweakGetterSetter(String classesDir) throws Exception {

		Class c = classLoad(classesDir,
				"it.imolinfo.jbi4corba.test.webservice.generator.EchoStruct");

		assertNotNull(c);
		assertNotNull(c.getMethod("getField1", new Class[] {}));
		assertNotNull(c.getMethod("setField1", new Class[] { int.class }));
	}

	private Class classLoad(String dir, String className)
			throws ClassGenerationException {

		URLClassLoader urlClassLoader = null;
		try {
			File fcd = new File(dir);

			if (log.isDebugEnabled()) {
				log
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL("file://" + fcd.getAbsolutePath() + "/");
			urlClassLoader = new URLClassLoader(new URL[] { u }, Thread
					.currentThread().getContextClassLoader());

			log.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			log.debug("class name: " + className);
			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			String m = "could not instantiate the url class loader on url: "
					+ "file://" + new File(dir).getAbsolutePath() + "/";
			log.error(m, e);
			throw new ClassGenerationException(m, e);
		} catch (ClassNotFoundException e) {
			String m = "could not instantiate the url class loader on url: "
					+ "file://" + new File(dir).getAbsolutePath() + "/";
			log.error(m, e);
			throw new ClassGenerationException(m, e);
		}
	}

	/**
	 * Testing code generation for ... Echo.idl
	 */
	public void testEchoClassesGeneration() {
		try {
			JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

			jbiServiceDescriptor.setIdlFileName("Echo.idl");
			jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);

			// serviceDescriptor.setIdl_compiler("jacorb");
			ProviderServiceClassesGenerator serviceGenerator = new ProviderServiceClassesGenerator();

			List<ClientCorbaClassesHolder> classes = serviceGenerator
					.generateProviderServiceClasses(jbiServiceDescriptor,
							"target/testEchoClassesGeneration", jarFilesName, // new
							// params
							null);

			log.debug("classes: " + classes);

			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			log.error("Error in testEchoClassesGeneration", e);
			fail(e.getMessage());
		}
	}

	/**
	 * Testing code generation for ... TwoServices.idl
	 */
	public void testEchoClassesGeneration2() {
		try {
			JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

			jbiServiceDescriptor.setIdlFileName("TwoServices.idl");
			jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);

			// serviceDescriptor.setIdl_compiler("jacorb");
			ProviderServiceClassesGenerator serviceGenerator = new ProviderServiceClassesGenerator();

			List<ClientCorbaClassesHolder> classes = serviceGenerator
					.generateProviderServiceClasses(jbiServiceDescriptor,
							"target/testEchoClassesGeneration2", jarFilesName, // new
							// params
							null);

			log.debug("classes: " + classes);

			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
			assertEquals(2, classes.size());
		} catch (Exception e) {
			log.error("Error in testEchoClassesGeneration2", e);
			fail(e.getMessage());
		}
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'valuetype'.
	 */
	public void testEchoClassesGenerationValueType() {
		final String idlToTest = "EchoValueType.idl";
		final String targetDir = "target/testEchoClassesGenerationValueType";

		log.debug(">>>>> testEchoClassesGenerationValueType - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationValueType()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationValueType - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'enum'.
	 */
	public void _testEchoClassesGenerationEnum() {
		final String idlToTest = "EchoEnum.idl";
		final String targetDir = "target/testEchoClassesGenerationEnum";

		log.debug(">>>>> testEchoClassesGenerationEnum - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());

		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationEnum()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationEnum - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'struct'.
	 */
	public void testEchoClassesGenerationStruct() {
		final String idlToTest = "EchoStruct.idl";
		final String targetDir = "target/testEchoClassesGenerationStruct";

		log.debug(">>>>> testEchoClassesGenerationStruct - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());

			// check setter and getter
			assertTweakGetterSetter(new File(targetDir + File.separator
					+ "classes").getAbsolutePath());

		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationStruct()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationStruct - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'sequence'.
	 */
	public void testEchoClassesGenerationSequence() {
		final String idlToTest = "EchoSequence.idl";
		final String targetDir = "target/testEchoClassesGenerationSequence";

		log.debug(">>>>> testEchoClassesGenerationSequence - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationSequence()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationSequence - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'array'.
	 */
	public void testEchoClassesGenerationArray() {
		final String idlToTest = "EchoArray.idl";
		final String targetDir = "target/testEchoClassesGenerationArray";

		log.debug(">>>>> testEchoClassesGenerationArray - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			e.printStackTrace();
			String m = "Error in ... testEchoClassesGenerationArray()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationArray - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'value box types'.
	 * 
	 * Notes:<br/> ------<br/> There are 2 cases to consider:<br/>
	 * <ol>
	 * <li>Value Boxes that are mapped to java primitive types.</li>
	 * <li>Value Boxes that are mapped to java classes.</li>
	 * </ol>
	 * 
	 * Boxed Primitive Types:<br/> if the value box IDL type maps to a lava
	 * primitive (e.g.: float, long, char, wchar, boolean, octet), then the
	 * value box type is mapped to a java class whose name is the same as the
	 * IDL value type. The class has a public data member named 'value' and has
	 * the appropriated java type.
	 * 
	 * Complex Type:<br/> If the value box IDL type maps to a java class (e.g.:
	 * string, wstring, enum, array, any, interface) then the value box type is
	 * mapped to a java class that is appropriate for the IDL type. Holder and
	 * Helper classes are also generated.
	 */
	public void testEchoClassesGenerationValueBoxTypes() {
		final String idlToTest = "EchoValueBoxTypes.idl";
		final String targetDir = "target/testEchoClassesGenerationValueBoxTypes";

		log.debug(">>>>> testEchoClassesGenerationValueBoxTypes - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationValueBoxTypes()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationValueBoxTypes - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains a
	 * 'exception'.
	 * 
	 * Notes:<br/> ------<br/> IDL exceptions are mapped very similar to
	 * structs. They are mapped to a java class that provides instance variables
	 * for the fields of the exception and constructors.
	 * 
	 * Corba system exceptions are UNCHECKED exceptions and they inherit
	 * (indirectly) from java.lang.RuntimeException.
	 * 
	 * User defined exceptions are CHECKED exceptions and they inherit
	 * (indirectly) from java.lang.Exception via org.omg.CORBA.UserException
	 * which itself extends IDLEntity.
	 * 
	 */
	public void testEchoClassesGenerationException() {
		final String idlToTest = "EchoException.idl";
		final String targetDir = "target/testEchoClassesGenerationException";

		log.debug(">>>>> testEchoClassesGenerationException - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationException()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationException - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains an
	 * 'abstract valuetype'.
	 */
	public void _testEchoClassesGenerationAbstractValueType() {
		final String idlToTest = "EchoAbstractValueType.idl";

		final String targetDir = "target/testEchoClassesGenerationAbstractValueType";

		log.debug(">>>>> testEchoClassesGenerationAbstractValueType - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationTypedef()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationAbstractValueType - end");
	}

	/**
	 * This test is used to verify the generation class when the IDL contains
	 * 'CGIL'.
	 */
	public void _testEchoClassesGenerationCGIL() {
		final String idlToTest = "cgil.idl";
		final String targetDir = "target/testEchoClassesGenerationCGIL";

		log.debug(">>>>> testEchoClassesGenerationCGIL - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationCGIL()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationCGIL - end");
	}


	public void testEchoClassesGenerationCOMPLEX() {
		final String idlToTest = "EchoComplex.idl";
		final String targetDir = "target/testEchoClassesGenerationCOMPLEX";

		log.debug(">>>>> testEchoClassesGenerationCOMPLEX - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationCOMPLEX()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationCOMPLEX - end");
	}

	/**
	 * @see http://195.223.140.228:9090/browse/CRB-106
	 */
	public void testEchoCrb106() {
		final String idlToTest = "EchoCrb106.idl";
		final String targetDir = "target/testEchoCrb106";

		log.debug(">>>>> testEchoCrb106 - begin");
		try {
			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());

			log.debug("List<ClientCorbaClassesHolder>.size=" + classes.size());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationCOMPLEX()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoCrb106 - end");
	}

	/**
	 * This test is used to verify the generation class whit Holder.
	 */
	public void testEchoClassesGenerationHolder() {
		final String idlToTest = "EchoHolder.idl";
		final String targetDir = "target/testEchoClassesGenerationHolder";

		log.debug(">>>>> testEchoClassesGenerationHolder - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationHolder()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationHolder - end");
	}

	// /**
	// * This test is used to verify the generation class when the IDL contains
	// * a 'fixed' data types.
	// */
	// public void testEchoFixed() {
	// final String idlToTest = "EchoFixed.idl";
	// final String targetDir = "target/testEchoFixed";
	//
	// log.debug(">>>>> testEchoFixed - begin");
	// try {
	// List<ClientCorbaClassesHolder> classes
	// = testBody(idlToTest, targetDir);
	//
	// // assertSOMETHING(message, condition)
	// assertNotNull("classes is null", classes);
	// assertFalse("Expected one or more classes.", classes.isEmpty());
	// } catch (Exception e) {
	// String m = "Error in ... testEchoFixed()";
	// log.error(m, e);
	// fail(m + ". cause:" + e.getMessage());
	// }
	// log.debug("<<<<< testEchoFixed - end");
	// }

	// ================================
	// Utility Methods
	// ================================

	/**
	 * This test is used to verify the generation class with UnionTypes.
	 */
	public void testEchoClassesGenerationUnionTypes() {
		final String idlToTest = "UnionTypes.idl";
		final String targetDir = "target/testEchoClassesGenerationUnionTypes";

		final String classesDir = new File(targetDir + File.separator
				+ "classes").getAbsolutePath();
		log.debug(">>>>> testEchoClassesGenerationUnionTypes - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
			assertTrue("does not contain two class", classes.size() == 2);

			for (ClientCorbaClassesHolder clientCorbaClassesHolder : classes) {

				List<MethodSignature> metSignaturs = clientCorbaClassesHolder
						.getMethodSignatures();


				
					Set<String> allUnionTs = clientCorbaClassesHolder.getAllUnionTypesMap().keySet();
					assertTrue(
							"number of union types in echo2 signature is not 4",
							allUnionTs.size() == 5);

					assertTrue(
							"all unions does not contain ThirdUnion",
							allUnionTs
									.contains("it.imolinfo.jbi4corba.test.ThirdUnion"));
					assertTrue(
							"all unions does not contain FirstUnion",
							allUnionTs
									.contains("it.imolinfo.jbi4corba.test.FirstUnion"));
					assertTrue(
							"all unions does not contain SecondUnion",
							allUnionTs
									.contains("it.imolinfo.jbi4corba.test.SecondUnion"));
					assertTrue(
							"all unions does not contain ForthUnion",
							allUnionTs
									.contains("it.imolinfo.jbi4corba.test.ForthUnion"));
					assertTrue(
							"all unions deos not contain TempUnion",
							allUnionTs
									.contains("it.imolinfo.jbi4corba.test.TempUnion"));

					UnionType ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.ThirdUnion");
					assertNotNull("ThirdUnion is null", ut);
					assertTrue(
							"ThirdUnion: type is not it.imolinfo.jbi4corba.test.ThirdUnion",
							ut
									.getTypeName()
									.equals(
											"it.imolinfo.jbi4corba.test.ThirdUnion"));
					assertTrue(
							"ThirdUnion: discriminator type is short",
							ut.getDiscriminatorType().equals(
									short.class));
					assertTrue("ThirdUnion: number of fields is not 4",
							ut.getNumberOfFields() == 4);
					Class thirdUnionfield1Type = ut
							.getFieldType("primo");
					assertNotNull("ThirdUnion: field 'primo' is null",
							thirdUnionfield1Type);
					assertTrue(
							"ThirdUnion: field 'primo' type is not int",
							thirdUnionfield1Type.equals(int.class));
					Class thirdUnionfield2Type = ut
							.getFieldType("secondo");
					assertNotNull(
							"ThirdUnion: field 'secondo' is null",
							thirdUnionfield2Type);
					assertTrue(
							"ThirdUnion: field 'secondo' type is not short",
							thirdUnionfield2Type.equals(short.class));
					Class thirdUnionfield3Type = ut
							.getFieldType("third");
					assertNotNull("ThirdUnion: field 'third' is null",
							thirdUnionfield3Type);
					assertTrue(
							"ThirdUnion: field 'third' type is it.imolinfo.jbi4corba.test.FirstUnion[]",
							thirdUnionfield3Type
									.getName()
									.equals(
											"[Lit.imolinfo.jbi4corba.test.FirstUnion;"));
					Class thirdUnionfield4Type = ut
							.getFieldType("altro");
					assertNotNull("ThirdUnion: field 'altro' is null",
							thirdUnionfield4Type);
					assertTrue(
							"ThirdUnion: field 'altro' type is boolean",
							thirdUnionfield4Type.equals(boolean.class));

					ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.SecondUnion");
					assertNotNull("SecondUnion is null", ut);
					assertTrue(
							"SecondUnion: type is not it.imolinfo.jbi4corba.test.SecondUnion",
							ut
									.getTypeName()
									.equals(
											"it.imolinfo.jbi4corba.test.SecondUnion"));
					assertTrue(
							"SecondUnion: discriminator type is short",
							ut.getDiscriminatorType().equals(
									short.class));
					assertTrue(
							"SecondUnion: number of fields is not 3",
							ut.getNumberOfFields() == 3);
					Class secondUnionfield1Type = ut
							.getFieldType("numeric");
					assertNotNull(
							"SecondUnion: field 'numeric' is null",
							secondUnionfield1Type);
					assertTrue(
							"SecondUnion: field 'numeric' type is not int",
							secondUnionfield1Type.equals(int.class));
					Class secondUnionfield2Type = ut
							.getFieldType("alfanumeric");
					assertNotNull(
							"SecondUnion: field 'alfanumeric' is null",
							secondUnionfield2Type);
					assertTrue(
							"SecondUnion: field 'alfanumeric' type is not string",
							secondUnionfield2Type.equals(String.class));
					Class secondUnionfield3Type = ut
							.getFieldType("two_format");
					assertNotNull(
							"SecondUnion: field 'two_format' is null",
							secondUnionfield3Type);
					assertTrue(
							"SecondUnion: field 'two_format' type is not org.omg.CORBA.Any",
							secondUnionfield3Type
									.equals(org.omg.CORBA.Any.class));

					ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.FirstUnion");
					assertNotNull("FirstUnion is null", ut);
					assertTrue(
							"FirstUnion: type is not it.imolinfo.jbi4corba.test.FirstUnion",
							ut
									.getTypeName()
									.equals(
											"it.imolinfo.jbi4corba.test.FirstUnion"));
					assertTrue(
							"FirstUnion: discriminator type is short",
							ut.getDiscriminatorType().equals(
									short.class));
					assertTrue("FirstUnion: number of fields is not 5",
							ut.getNumberOfFields() == 5);
					Class firstUnionfield1Type = ut
							.getFieldType("numeric");
					assertNotNull(
							"FirstUnion: field 'numeric' is null",
							firstUnionfield1Type);
					assertTrue(
							"FirstUnion: field 'numeric' type is not int",
							firstUnionfield1Type.equals(int.class));
					Class firstUnionfield2Type = ut
							.getFieldType("alfanumeric");
					assertNotNull(
							"FirstUnion: field 'alfanumeric' is null",
							firstUnionfield2Type);
					assertTrue(
							"FirstUnion: field 'numeric' type is not string",
							firstUnionfield2Type.equals(String.class));
					Class firstUnionfield3Type = ut
							.getFieldType("uni1");
					assertNotNull(
							"FirstUnion: field 'uni1' is null",
							firstUnionfield3Type);
					assertTrue(
							"FirstUnion: field uni1 is not it.imolinfo.jbi4corba.test.TempUnion",
							firstUnionfield3Type
									.getName()
									.equals(
											"it.imolinfo.jbi4corba.test.TempUnion"));
					Class firstUnionfield4Type = ut
							.getFieldType("uni2");
					assertNotNull(
							"FirstUnion: field 'uni2' is null",
							firstUnionfield4Type);
					assertTrue(
							"FirstUnion: field uni2 is not array of it.imolinfo.jbi4corba.test.TempUnion",
							firstUnionfield4Type
									.getName()
									.equals(
											"[Lit.imolinfo.jbi4corba.test.TempUnion;"));
				
					Class firstUnionfield5Type = ut
						.getFieldType("two_format");
					assertNotNull(
						"FirstUnion: field 'two_format' is null",
						firstUnionfield5Type);
					assertTrue(
							"FirstUnion: field 'two_format' type is not org.omg.CORBA.Any",
							firstUnionfield5Type
									.equals(org.omg.CORBA.Any.class));
			
				
				
				
				// verify that union types verifyXXX methods are public
				Collection<UnionType> uniTyps = clientCorbaClassesHolder.getAllUnionTypes();
				
				for (UnionType unTyp : uniTyps)
				{
					checkVerifyUTMethods(classesDir,unTyp);
				}
			}
			
			
			

		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationUnionTypes()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationUnionTypes - end");
	}
	
	/**
	 * This test is used to verify the generation class with UnionTypes discriminator.
	 */
	public void testEchoClassesGenerationUnionTypesDiscriminators() {
		final String idlToTest = "UnionTypesDiscriminators.idl";
		final String targetDir = "target/testEchoClassesGenerationUnionTypesDiscriminators";

		final String classesDir = new File(targetDir + File.separator
				+ "classes").getAbsolutePath();
		log.debug(">>>>> testEchoClassesGenerationUnionTypesDiscriminators - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			// assertSOMETHING(message, condition)
			assertNotNull("classes is null", classes);
			assertFalse("Expected one or more classes.", classes.isEmpty());
			assertTrue("contains one class", classes.size() == 1);
			
			UnionType ut;

			for (ClientCorbaClassesHolder clientCorbaClassesHolder : classes) {

				List<MethodSignature> metSignaturs = clientCorbaClassesHolder
						.getMethodSignatures();
				if (clientCorbaClassesHolder.getOperationsClass().getName()
						.endsWith("EchoDiscriminatorsOperations")) {
					assertTrue("number of methods is 1",
							metSignaturs.size() == 1);
					
							Set<String> unionTs = clientCorbaClassesHolder.getAllUnionTypesMap().keySet();
							assertTrue(
									"number of union types in echoDiscriminator signature is not 4",
									unionTs.size() == 4);
							ut = clientCorbaClassesHolder
									.getUnionType("it.imolinfo.jbi4corba.test.CharDUnion");
							assertNotNull("CharDUnion is null", ut);
							assertTrue(
									"CharDUnion: type is not it.imolinfo.jbi4corba.test.CharDUnion",
									ut
											.getTypeName()
											.equals(
													"it.imolinfo.jbi4corba.test.CharDUnion"));
							assertTrue(
									"CharDUnion: discriminator type is char",
									ut.getDiscriminatorType().equals(
											char.class));
							ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.BooleanDUnion");

							assertTrue(
									"method echoDiscriminator does not contain BooleanDUnion",
									unionTs
											.contains("it.imolinfo.jbi4corba.test.BooleanDUnion"));
							assertTrue(
									"BooleanDUnion: discriminator type is boolean",
									ut.getDiscriminatorType().equals(
											boolean.class));
							ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.IntDUnion");
							assertTrue(
									"method echoDiscriminator does not contain IntDUnion",
									unionTs
											.contains("it.imolinfo.jbi4corba.test.IntDUnion"));
							assertTrue(
									"IntDUnion: discriminator type is int",
									ut.getDiscriminatorType().equals(
											int.class));
							ut = clientCorbaClassesHolder
							.getUnionType("it.imolinfo.jbi4corba.test.EnumDUnion");
							assertTrue(
									"method echoDiscriminator does not contain EnumDUnion",
									unionTs
											.contains("it.imolinfo.jbi4corba.test.EnumDUnion"));
							assertTrue(
									"IntDUnion: discriminator type is long",
									ut.getDiscriminatorType().getName().equals(
											"it.imolinfo.jbi4corba.test.MyEnum"));

				
				}
				
				// verify that union types verifyXXX methods are public
				
			}
			
			
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationUnionTypesDiscriminators()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationUnionTypesDiscriminators - end");
	}
	
	/**
	 * This test is used to verify the generation of union type wrappers for union types as parameter/return type.
	 */
	public void testEchoClassesGenerationUnionTypesWrappers() {
		final String idlToTest = "UnionTypesWrappers.idl";
		final String targetDir = "target/testEchoClassesGenerationUnionTypesWrappers";

		final String classesDir = new File(targetDir + File.separator
				+ "classes").getAbsolutePath();
		log.debug(">>>>> testEchoClassesGenerationUnionTypesDiscriminators - begin");
		try {

			List<ClientCorbaClassesHolder> classes = testBody(idlToTest,
					targetDir);

			Class c = classLoad(classesDir,
			"it.imolinfo.jbi4corba.test.EchoOperations");
			
			assertNotNull(c);
			
			Method[] methods = c.getDeclaredMethods();
			
			Class frsWr = classLoad(classesDir,
			"it.imolinfo.jbi4corba.test.FirstUnionWrapper");
			assertNotNull( frsWr);
			Class secWr = classLoad(classesDir,
			"it.imolinfo.jbi4corba.test.SecondUnionWrapper");
			assertNotNull( secWr);
			Class thrWr = classLoad(classesDir,
			"it.imolinfo.jbi4corba.test.ThirdUnionWrapper");
			assertNotNull( thrWr);
			
			Class tempWr = classLoad(classesDir,
			"it.imolinfo.jbi4corba.test.TempUnionWrapper");
			assertNotNull( tempWr);
			
			Field unionField = frsWr.getDeclaredField("choiceValue");
			javax.xml.bind.annotation.XmlElements annXMLs = unionField.getAnnotation(javax.xml.bind.annotation.XmlElements.class);
			assertNotNull(annXMLs);
			javax.xml.bind.annotation.XmlElement[] annXML = annXMLs.value();
			assertNotNull(annXML);
			assertTrue(
					"number of XMLElement annotations is not 5",
					annXML.length == 5
							);
			int numOfXMLElement = 0;
			
			for (javax.xml.bind.annotation.XmlElement ann : annXML)
			{
			
				assertNotNull(ann);
				if (ann.name().equals("uni1"))
				{
					assertTrue(
							"uni1 type is not TempUnionWrapper",
							ann.type().getName().equals(tempWr.getName())
									);
					numOfXMLElement++;
				}
				
				if (ann.name().equals("alfanumeric"))
				{
					assertTrue(
							"alfanumeric type is not String",
							ann.type().equals(String.class)
									);
					numOfXMLElement++;
				}
				if (ann.name().equals("numeric"))
				{
					assertTrue(
							"numeric type is not int",
							ann.type().equals(int.class)
									);
					numOfXMLElement++;
				}
				if (ann.name().equals("two_format"))
				{
					assertTrue(
							"two_format type is not java.lang.Object",
							ann.type().equals(Object.class)
									);
					numOfXMLElement++;
				}
				if (ann.name().equals("uni2"))
				{
					assertTrue(
							"uni2 type is not Object[]",
							ann.type().getName().equals("[Lit.imolinfo.jbi4corba.test.TempUnionWrapper;")
									);
					numOfXMLElement++;
				}
				
			}
			assertTrue(
					"number of XMLElement annotations is not 5",
					numOfXMLElement == 5
					);
			
			
			for (Method method : methods)
			{
				Class[] params = method.getParameterTypes();
				Class returnType = method.getReturnType(); 
				
				if (method.getName().equals("echo1"))
				{
					
					assertTrue(
							"echo1: first parameter type is it.imolinfo.jbi4corba.test.ThirdUnionWrapper",
							params[0].getName().equals(
									thrWr.getName()));
					assertTrue(
							"echo1: second parameter type is short[][]",
							params[1].equals(
									short[][].class));
					
					assertTrue(
							"echo1: third parameter type is it.imolinfo.jbi4corba.test.SecondUnionWrapper",
							params[2].getName().equals(
									secWr.getName()));
					
					assertTrue(
							"echo1: return type is string",
							returnType.equals(
									String.class));
					
				}
				if (method.getName().equals("echo2"))
				{
					assertTrue(
							"echo2: first parameter type is boolean[]",
							params[0].equals(
									boolean[].class));
					assertTrue(
							"echo2: second parameter type is it.imolinfo.jbi4corba.test.SecondUnionWrapper",
							params[1].getName().equals(
									secWr.getName()));
					
					assertTrue(
							"echo2: third parameter type is boolean[]",
							params[2].equals(
									boolean[].class));
					
					assertTrue(
							"echo2: forth parameter type is short[][]",
							params[3].equals(
									short[][].class));
					
					assertTrue(
							"echo2: return type is it.imolinfo.jbi4corba.test.ThirdUnion",
							returnType.getName().equals(
									thrWr.getName()));
				}
				if (method.getName().equals("echo3"))
				{
					assertTrue(
							"echo3: first parameter type is boolean[]",
							params[0].equals(
									boolean[].class));
					assertTrue(
							"echo3: second parameter type is boolean",
							params[1].equals(
									boolean.class));
					
					assertTrue(
							"echo3: third parameter type is char",
							params[2].equals(
									char.class));
					
					assertTrue(
							"echo3: forth parameter type is it.imolinfo.jbi4corba.test.ThirdUnion",
							params[3].getName().equals(
									thrWr.getName()));
					
					assertTrue(
							"echo3: fith parameter type is boolean",
							params[4].equals(
									boolean.class));
					
					assertTrue(
							"echo3: sixth parameter type is char",
							params[5].equals(
									char.class));
					
					assertTrue(
							"echo3: return type is it.imolinfo.jbi4corba.test.FirstUnion",
							returnType.getName().equals(
									frsWr.getName()));
				}
			}
		
			
		} catch (Exception e) {
			String m = "Error in ... testEchoClassesGenerationUnionTypesDiscriminators()";
			log.error(m, e);
			fail(m + ". cause:" + e.getMessage());
		}
		log.debug("<<<<< testEchoClassesGenerationUnionTypesDiscriminators - end");
	}

	/**
	 * checkVerifyUTMethods
	 * verify methods that are public
	 * @param classesDir
	 * @param unTyp
	 * @throws ClassGenerationException
	 * @throws SecurityException
	 * @throws NoSuchMethodException
	 */
	private void checkVerifyUTMethods(String classesDir, UnionType unTyp) throws ClassGenerationException, SecurityException, NoSuchMethodException {

		Class c = classLoad(classesDir,
				unTyp.getTypeName());

		assertNotNull(c);
		
		for (String field : unTyp.getTypeFieldNameList()) {
			Method met = c.getDeclaredMethod("verify" + field, new Class[] { short.class });
			assertTrue("verify" + field + " is not public", Modifier.isPublic(met.getModifiers()));
		}
	}

	
	
	/**
	 * The code used to generate the classes.
	 */
	private List<ClientCorbaClassesHolder> testBody(String idlToTest,
			String targetDir) throws ClassGenerationException {
		log.debug(">>>>> testBody - begin");

		JbiServiceDescriptor jbiServiceDescriptor = new JbiServiceDescriptor();

		jbiServiceDescriptor.setIdlFileName(idlToTest);
		jbiServiceDescriptor.setIdlFileNameDirectory(IDLDIR);

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
	
}
