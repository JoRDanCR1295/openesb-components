 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.webservice.generator.bcm.CorbaEnumAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.CorbaOnewayAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.SerializableDecorationAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.SerializableInspectorAdapter;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * A collection of utility methods.
 */
public class Util {

	private static final String CLASS_FILE_EXTENSION = ".class";

	/**
	 * The platform dependent protocol used to load file and classes.
	 */
	public static final String PROTOCOL;

	/**
	 * Buffer size to copy from an InputStream to the output.
	 */
	private static final int DEFAULT_BUFFER_SIZE = 4096;

	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory.getLogger(Util.class);

	/**
	 * Default constructor.
	 */
	public Util() {

	}

	static {
		if (System.getProperty("os.name").indexOf("Win") >= 0) {
			PROTOCOL = "file:///";
		} else {
			PROTOCOL = "file://";
		}
	}

	/**
	 * @param classesDir
	 *            The classes dir
	 * @param className
	 *            The class name
	 * @param extraClasspath
	 *            The extra class path
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void compileRemoteClassesNoValueMethodsON(String classesDir,
			String className, List<String> extraClasspath)
			throws ClassGenerationException {

		LOG.debug(">>>>> compileRemoteClassesNoValueMethodsON - begin");

		String cp = classesDir;

		if (extraClasspath != null) {
			for (String i : extraClasspath) {
				cp += File.pathSeparator + i;
			}
		}

		List<String> params = new ArrayList<String>(Arrays.asList(new String[] {
				"-classpath", classesDir, "-d", classesDir, "-idl", "-always",
				"-factory", // FIXME test
				"-iiop", "-nolocalstubs", "-noValueMethods", // only when -idl
				"-poa", "-verbose", className }));

		compileRemoteClasses(classesDir, className, params);

		LOG.debug("<<<<< compileRemoteClassesNoValueMethodsON - end");
	}

	/**
	 * @param classesDir
	 *            The classes dir
	 * @param className
	 *            The class name
	 * @param extraClasspath
	 *            The extra class path
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void compileRemoteClassesNoValueMethodsOFF(String classesDir,
			String className, List<String> extraClasspath)
			throws ClassGenerationException {

		LOG.debug(">>>>> compileRemoteClassesNoValueMethodsOFF - begin");

		String cp = classesDir;

		if (extraClasspath != null) {
			for (String i : extraClasspath) {
				cp += File.pathSeparator + i;
			}
		}

		List<String> params = new ArrayList<String>(Arrays.asList(new String[] {
				"-classpath", cp, "-d", classesDir, "-idl", "-always", "-iiop",
				"-nolocalstubs", "-verbose", className }));

		compileRemoteClasses(classesDir, className, params);

		LOG.debug("<<<<< compileRemoteClassesNoValueMethodsOFF - end");
	}

	// XXX javadoc
	/**
	 * @param classesDir
	 *            The classes dir
	 * @param className
	 *            The class name
	 * @param params
	 *            The params
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	private static void compileRemoteClasses(String classesDir,
			String className, List<String> params)
			throws ClassGenerationException {

		LOG.debug(">>>>> compileRemoteClasses - begin");

		LOG.debug("compileRemoteClasses - classesDir=" + classesDir
				+ "; className=" + className);

		/*
		 * -idl Causes rmic to generate OMG IDL for the classes specified and
		 * any classes referenced. IDL provides a purely declarative,
		 * programming language-independent way of specifying an object's API.
		 * The IDL is used as a specification for methods and data that can be
		 * written in and invoked from any language that provides CORBA
		 * bindings.
		 * 
		 * When the -idl option is used, other options also include:
		 * 
		 * -always or -alwaysgenerate: Forces re-generation
		 * 
		 * -factory: Uses factory keyword in generated IDL.
		 * 
		 * -idlModule fromJavaPackage[.class] toIDLModule Specifies IDLEntity
		 * package mapping. For example: -idlModule foo.bar my::real::idlmod.
		 * 
		 * -idlFile fromJavaPackage[.class] toIDLFile Specifies IDLEntity file
		 * mapping. For example: -idlFile test.pkg.X TEST16.idl.
		 * 
		 * -iiop Causes rmic to generate IIOP stub and tie classes, rather than
		 * JRMP stub and skeleton classes. A stub class is a local proxy for a
		 * remote object and is used by clients to send calls to a server. Each
		 * remote interface requires a stub class, which implements that remote
		 * interface. A client's reference to a remote object is actually a
		 * reference to a stub. Tie classes are used on the server side to
		 * process incoming calls, and dispatch the calls to the proper
		 * implementation class. Each implementation class requires a tie class.
		 * 
		 * Invoking rmic with the -iiop generates stubs and ties that conform to
		 * this naming convention: _<implementationName>_stub.class
		 * _<interfaceName>_tie.class
		 * 
		 * When the -iiop option is used, other options also include:
		 * 
		 * -always or -alwaysgenerate Forces re-generation even when existing
		 * stubs/ties/IDL are newer than the input class. -nolocalstubs Do not
		 * create stubs optimized for same-process clients and servers.
		 * -noValueMethods Must be used with the -idl option. Prevents addition
		 * of valuetype methods and initializers to emitted IDL. These methods
		 * and initializers are optional for valuetypes, and are generated
		 * unless the -noValueMethods option is specified when using the -idl
		 * option. -poa Changes the inheritance from
		 * org.omg.CORBA_2_3.portable.ObjectImpl to
		 * org.omg.PortableServer.Servant. The PortableServer module for the
		 * Portable Object Adapter (POA) defines the native Servant type. In the
		 * Java programming language, the Servant type is mapped to the Java
		 * org.omg.PortableServer.Servant class. It serves as the base class for
		 * all POA servant implementations and provides a number of methods that
		 * may be invoked by the application programmer, as well as methods
		 * which are invoked by the POA itself and may be overridden by the user
		 * to control aspects of servant behavior.
		 */

		ByteArrayOutputStream BAOS = new ByteArrayOutputStream();
		PrintStream printstream = new PrintStream(BAOS);

		sun.rmi.rmic.Main main = new sun.rmi.rmic.Main(printstream, "rmic");

		boolean result = main.compile(params.toArray(new String[] {}));

		// com.sun.tools.javac.Main.main(params.toArray(new String[] {}));
		LOG.debug("compilation output: \n" + BAOS.toString());
		if (!result) {
			Object[] args = new Object[] { params, result, BAOS.toString() };

			LOG.error("CRB000528_Error_compiling_classes", args);
			throw new ClassGenerationException(
					"CRB000528_Error_compiling_classes", args, null);
		} else {
			LOG.debug("compilation ok");
		}

		LOG.debug("<<<<< compileRemoteClasses - end");
	}

	/**
	 * Finding the *.java files in the directory 'basedir'.
	 * 
	 * @param basedir
	 *            The directory where the method looks up the files.
	 * 
	 * @return The list of java source file (canonical path)
	 * 
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	public static List<String> findJavaSources(String basedir)
			throws Jbi4CorbaException {

		LOG.debug(">>>>> findJavaSources(String) - begin");

		List<String> javaSourcesNames = findJavaSources(basedir, null);

		LOG.debug("<<<<< findJavaSources(String) - end");
		return javaSourcesNames;
	}

	/**
	 * This method looking for 'x' inside the list. The type of the slash and
	 * the case will be ignored.
	 * 
	 * @param list
	 *            The list to inspect.
	 * @param x
	 *            The object to find.
	 * 
	 * @return true, if x is contained in the list.
	 */
	private static boolean containsIgnoreSlashes(List<String> list, String x) {
		if (x == null) {
			return false;
		}
		// else
		if (list == null) {
			return false;
		}
		// else

		String xrr = x.replace('\\', ' ').replace('/', ' ');

		for (String c : list) {
			String crr = c.replace('\\', ' ').replace('/', ' ');

			if (xrr.equalsIgnoreCase(crr)) {
				return true;
			}
		}

		// else
		return false;
	}

	/**
	 * 
	 * @param basedir
	 *            The basedir
	 * @param exclude
	 *            The exclude
	 * @return The return
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	public static List<String> findJavaSources(String basedir,
			List<String> exclude) throws Jbi4CorbaException {

		LOG.debug(">>>>> findJavaSources(String, List<String>) - begin");

		List<String> javaSourcesNames = new ArrayList<String>();
		List<File> sourceFiles = findFilesFromSourceDirectory(basedir, ".java");

		for (File source : sourceFiles) {

			String src = null;
			try {

				src = source.getCanonicalPath();

			} catch (IOException e) {
				Object[] args = new Object[] { basedir };

				LOG.error("CRB000529_Unexpected_error", args, e);
				throw new Jbi4CorbaException("CRB000529_Unexpected_error",
						args, e);
			}

			/*
			 * Some files is not compiled.
			 * 
			 * This feature is useful when we add to the classpath some class
			 * compiled before this task and bytecode manipulated.
			 */
			if (src.endsWith("src")) {

				LOG.debug("The file " + src + " won't be compile.");

			} else if (containsIgnoreSlashes(exclude, src)) {

				LOG.debug("The file " + src + " won't be compile.");

			} else {
				LOG.debug("The file " + src + " will be compiled.");
				javaSourcesNames.add(src);
			}
		}

		LOG.debug("<<<<< findJavaSources(String, List<String>) - end");
		return javaSourcesNames;
	}

	/**
	 * This method find all the java source file with the suffix
	 * 'DefaultFacoty.java'.
	 * 
	 * @param basedir
	 *            The directory inspected.
	 * 
	 * @return The list of files found.
	 * 
	 * @throws IOException
	 *             The IO exception
	 */
	public static List<String> findAllDefaultFactory(String basedir)
			throws IOException {

		final String suffix = "DefaultFactory.java";

		LOG.debug(">>>>> findAllDefaultFactory - begin");

		List<String> javaSourcesNames = new ArrayList<String>();
		List<File> sourceFiles = findFilesFromSourceDirectory(basedir, suffix);

		for (File source : sourceFiles) {
			javaSourcesNames.add(source.getAbsolutePath());

			LOG.debug("findAllDefaultFactory + " + source.getCanonicalPath());
		}

		LOG.debug("<<<<< findAllDefaultFactory - end");
		return javaSourcesNames;
	}

	/**
	 * Finding the *.idl files in the directory 'basedir'.
	 * 
	 * @param basedir
	 *            The directory where the method looks up the files.
	 * 
	 * @return The list of IDL file (canonical path)
	 * 
	 * @throws Jbi4CorbaException
	 *             The Jbi4Corba exception
	 */
	public static List<String> findIdlFiles(String basedir)
			throws Jbi4CorbaException {

		LOG.debug(">>>>> findIdlFiles - begin");

		List<String> javaSourcesNames = new ArrayList<String>();
		List<File> sourceFiles = findFilesFromSourceDirectory(basedir, ".idl");

		for (File source : sourceFiles) {

			String src = null;
			try {
				src = source.getCanonicalPath();
			} catch (IOException e) {
				Object[] args = new Object[] { basedir };

				LOG.error("CRB000530_Unexpected_error", args, e);
				throw new Jbi4CorbaException("CRB000530_Unexpected_error",
						args, e);
			}

			javaSourcesNames.add(src);
			LOG.debug("findIdlFiles + " + src);
		}

		LOG.debug("<<<<< findIdlFiles - end");
		return javaSourcesNames;
	}

	/**
	 * This method find all the files in a directory according to the filter.
	 * MARCO 23/09/2007: now searches also in the root
	 * <code>baseDirString</code> (not only in the subdirectories). <br/>
	 * 
	 * @param basedirString
	 *            The directory where the method works.
	 * @param extensionFilter
	 *            The filter (the suffix of the files).
	 * @return The return
	 */
	public static List<File> findFilesFromSourceDirectory(String basedirString,
			final String extensionFilter) {

		LOG.debug(">>>>> findFilesFromSourceDirectory - begin");

		File basedir = new File(basedirString);

		FileFilter filter = new FileFilter() {
			public boolean accept(File file) {

				boolean filterAcceptExtension = file.getName().endsWith(
						extensionFilter);
				// Excludes the package-info.class classes
				boolean filterAccept = filterAcceptExtension
						&& (!file.getName().equals("package-info.class"));

				// LOG.debug("findFilesFromSourceDirectory"
				// + "; extensionFilter=" + extensionFilter
				// + "; filterAccept=" + filterAccept
				// + "; file=" + file.getName());

				return filterAccept;
			}
		};

		LOG.debug("findFilesFromSourceDirectory" + "; basedir=" + basedir
				+ "; FileFilter=" + filter + "; extensionFilter="
				+ extensionFilter);

		List<File> directories = findDirectories(basedir);
		/*
		 * MARCO: Added (23/09/2008) to test also the base directory.
		 */
		if (basedir != null) {
			directories.add(basedir);
		}
		LOG.debug("directories: " + directories);

		List<File> filterdFiles = new ArrayList<File>();
		for (File dir : directories) {
			File[] innerFilteredFiles = dir.listFiles(filter);

			if (innerFilteredFiles != null)
			{
				LOG.debug("inner filtered files: "
						+ Arrays.asList(innerFilteredFiles));
	
				filterdFiles.addAll(Arrays.asList(innerFilteredFiles));
			}
		}

		LOG.debug("<<<<< findFilesFromSourceDirectory - end");
		return filterdFiles;
	}

	// XXX javadoc

	/**
	 * @param basedir
	 *            The basedir
	 * @return The return
	 */
	private static List<File> findDirectories(File basedir) {
		List<File> directories = new ArrayList<File>();

		if (basedir == null || "".equals(basedir)) {
			return directories;
		}

		FileFilter directoryFilter = new FileFilter() {
			public boolean accept(File file) {
				return file.isDirectory();
			}
		};

		File[] files = basedir.listFiles(directoryFilter);

		if (files == null) {
			return directories;
		}

		for (int i = 0; i < files.length; i++) {
			List<File> innerDirectories = findDirectories(files[i]);
			directories.addAll(innerDirectories);
			directories.add(files[i]);
		}

		return directories;
	}

	/**
	 * XXX javadoc.
	 * 
	 * @param workdirsrc
	 *            The work dir src
	 * @param workdirclasses
	 *            The workdir classes
	 * @param javaSources
	 *            The java sources
	 * @param jarFiles
	 *            The jar files
	 * @param extraClassPath
	 *            The extra class path
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void compileJavaClasses(String workdirsrc,
			String workdirclasses, List<String> javaSources,
			List<String> jarFiles, List<String> extraClassPath)
			throws ClassGenerationException {

		LOG.debug(">>>>> compileJavaClasses - begin");

		LOG.debug("compileJavaClasses" + ".\n workdirsrc=" + workdirsrc
				+ ";\n workdirclasses=" + workdirclasses + ";\n javaSources="
				+ javaSources + ";\n jarFiles=" + jarFiles
				+ ";\n extraClassPath=" + extraClassPath);

		List<String> params = new ArrayList<String>(Arrays.asList(new String[] {
				"-d", workdirclasses, "-sourcepath", workdirsrc }));

		LOG.debug("creating classpath - begin");

		String classpath = "";

		// jars ...

		if (jarFiles != null) {

			for (String jarFileName : jarFiles) {
				classpath += jarFileName + File.pathSeparator; // ":" or ";"

				LOG.debug("ClassPath + (jar) " + jarFileName);
			}

		}

		// ... and extra

		if (extraClassPath != null) {
			for (String extra : extraClassPath) {

				File f = new File(extra);
				if (!f.isDirectory()) {
					extra = f.getParent();
					LOG.debug("-----> EXTRA=[" + extra + "]");
				}

				classpath += extra + File.pathSeparator; // ":" or ";"

				LOG.debug("ClassPath + (extra) " + extra);
			}
		} else {
			LOG.debug("No extra classpath.");
		}

		// at the end I must set up the classpath

		if (!"".equals(classpath)) {
			LOG.debug("Final ClassPath=" + classpath);

			params.add("-cp");
			params.add(classpath);
		} else {
			LOG.debug("Final ClassPath=<<EMPTY>>");
		}

		LOG.debug("creating classpath - end.");

		// log.debug("javaSources: " + Arrays.toString(javaSources.toArray()));
		// log.debug("params: " + Arrays.toString(params.toArray()));

		params.addAll(javaSources);

		LOG.debug("command line: " + Arrays.toString(params.toArray()));

		File classesdir = new File(workdirclasses);
		if (!classesdir.exists()) {
			boolean result = classesdir.mkdirs();

			if (!result) {
				Object[] args = new Object[] { classesdir };

				LOG.error("CRB000531_Unable_to_create_compilation_directory",
						args);
				throw new ClassGenerationException(
						"CRB000531_Unable_to_create_compilation_directory",
						args, null);
			} else {
				LOG.debug("Classes Dir Created:" + classesdir);
			}
		} else {
			LOG.debug("Classes Dir Already Exists:" + classesdir);
		}

		StringWriter stringWriter = new StringWriter();
		PrintWriter printstream = new PrintWriter(stringWriter);

		// Main main = new Main(printstream, "javac");
		// Class mainclass = com.sun.tools.javac.Main.class;

		/*
		 * ClassLoader mainclassClassLoader=mainclass.getClassLoader();
		 * log.debug
		 * ("mainclass: "+mainclass+" classloader: "+mainclass.getClassLoader
		 * ()); Class
		 * xmlAccessTypeClass=javax.xml.bind.annotation.XmlAccessType.class;
		 * log.debug("XmlAccessClass: "+xmlAccessTypeClass+" classloader: "+
		 * xmlAccessTypeClass.getClassLoader()); try {
		 * mainclassClassLoader.loadClass
		 * ("javax.xml.bind.annotation.XmlAccessType"); } catch
		 * (ClassNotFoundException e) {
		 * log.error("could not instantiate class: " +
		 * "javax.xml.bind.annotation.XmlAccessType", e); throw new
		 * ClassGenerationException("could not instantiate class: " +
		 * "javax.xml.bind.annotation.XmlAccessType", e); }
		 */

		LOG.debug("Compiling - begin");

		int result = com.sun.tools.javac.Main.compile(params
				.toArray(new String[] {}), printstream);

		LOG.debug("Compiling - end. result=" + result);

		// com.sun.tools.javac.Main.main(params.toArray(new String[] {}));

		LOG.debug("compilation output: \n" + stringWriter.toString());

		if (!(result == 0)) {
			Object[] args = new Object[] { params, result,
					stringWriter.toString() };

			LOG.error("CRB000528_Error_compiling_classes", args);
			throw new ClassGenerationException(
					"CRB000528_Error_compiling_classes", args, null);
		} else {
			LOG.debug("Compilation OK.");
		}

		LOG.debug("<<<<< compileJavaClasses - end");
	}

	/**
	 * This method extracts all the MethodSignature in the 'Operations' source
	 * files.
	 * 
	 * @param sourceDir
	 *            The directory where we find the files.
	 * 
	 * @return The map of method signature associated to the qualified java name
	 *         of the class (the key of the map).
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Map<String, List<MethodSignature>> extractMethodSignatureOfTheCorbaOperations(
			String sourceDir) throws ClassGenerationException {

		// find
		List<File> corbaOperationsSourceFileList = Util
				.findFilesFromSourceDirectory(sourceDir, "Operations.java");

		Map<String, List<MethodSignature>> map = new HashMap<String, List<MethodSignature>>();

		// check
		if (corbaOperationsSourceFileList == null
				|| corbaOperationsSourceFileList.size() == 0) {

			LOG.debug("No files found with the suffix 'Operations.java'");
			return map;
		}

		// else

		UtilJavaSourceParsing parser = new UtilJavaSourceParsing();

		for (File operationFile : corbaOperationsSourceFileList) {

			String absPath = operationFile.getAbsolutePath();
			LOG.debug("parsing ... " + absPath);

			String fullQualifiedName = extractJavaQualifiedName(sourceDir,
					absPath, ".java");

			List<MethodSignature> methodSignatureList = parser
					.extractMethodSignature(absPath, fullQualifiedName);

			map.put(fullQualifiedName, methodSignatureList);
            LOG.debug("signatures name: "+fullQualifiedName+" signs: "+Arrays.toString(methodSignatureList.toArray()));
		}

		return map;
	}

	/**
	 * This method extracs all the class used as method's parameter or method's
	 * result.
	 * 
	 * @param dir
	 *            Where looking for the classes.
	 * @return The set of class found.
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Set<Class> findClassUsedInTheOperations(String dir)
			throws ClassGenerationException {
		LOG.debug(">>>>> findClassUsedInTheOperations - begin");

		List<File> operationsClass = Util.findFilesFromSourceDirectory(dir,
				"Operations.class");

		Set<Class> result = findClassUsed(dir, operationsClass);

		LOG.debug("<<<<< findClassUsedInTheOperations - end:" + result);
		return result;
	}
	/**
	 * This method extracts all the types used in idl
	 * file.
	 * 
	 * @param dir
	 *            Where looking for the classes.
	 * @return The set of class found.
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Set<Class> findAllTypesUsedInIDL(String dir)
			throws ClassGenerationException {
		LOG.debug(">>>>> findClassUsedInTheOperations - begin");

		List<File> operationsClass = Util.findFilesFromSourceDirectory(dir,
				"Helper.class");
		
		Set<Class> result = findAllTypes(dir, operationsClass);

		LOG.debug("<<<<< findClassUsedInTheOperations - end:" + result);
		return result;
	}

	
	/**
	 * findAllTypes - finds all types defined in IDL
	 * 
	 * @param dir
	 * @param typeClasses
	 * @return
	 * @throws ClassGenerationException
	 */
	public static HashMap<String,String> getTypesMap(String dir) throws ClassGenerationException {
		
		List<File> typeClasses = Util.findFilesFromSourceDirectory(dir,
		"Helper.class");

		if (typeClasses == null || typeClasses.size() == 0) {
			LOG.info("CRB000532_type_classes_not_found");
			return new HashMap<String,String>();
		}

		HashMap<String,String> result = new HashMap<String,String>();
		String id="";
		for (int i = 0; i < typeClasses.size(); i++) {
			
			Class helperclazz = loadHelperClass(dir, typeClasses.get(i));
			
			try {
				id = helperclazz.getMethod("id",null ).invoke(null,null).toString();
			} catch (Exception e) {
                                throw new ClassGenerationException("CRB000533_Could_not_invoke_id_in_the_helper_class",e);
			}
			LOG.debug("loadTypeClass:" + helperclazz);
			if (helperclazz != null && !helperclazz.getCanonicalName().startsWith("org.omg."))
				result.put(id,helperclazz.getCanonicalName());
		}
		return result;
	}

	/**
	 * findAllTypes - finds all types defined in IDL
	 * 
	 * @param dir
	 * @param typeClasses
	 * @return
	 * @throws ClassGenerationException
	 */
	private static Set<Class> findAllTypes(String dir,
			List<File> typeClasses) throws ClassGenerationException {
		LOG.debug(">>>>> findAllTypes - begin");

		LOG.debug("Type Classes =" + typeClasses);

		if (typeClasses == null || typeClasses.size() == 0) {
			LOG.info("CRB000532_type_classes_not_found");
			return new HashSet<Class>();
		}

		Set<Class> result = new HashSet<Class>();

		for (int i = 0; i < typeClasses.size(); i++) {
			Class clazz = loadTypeClass(dir, typeClasses.get(i));
			LOG.debug("loadTypeClass:" + clazz);
			if (clazz != null && !clazz.getCanonicalName().startsWith("org.omg."))
				result.add(clazz);
		}
		return result;
	}
	
	/**
	 * loadTypeClass - loads type
	 * 
	 * @param dir
	 * @param classAsFile
	 * @return
	 * @throws ClassGenerationException
	 */
	private static Class loadTypeClass(String dir, File classAsFile) throws ClassGenerationException {
		URLClassLoader urlClassLoader = null;
		String className = null;

		try {
			File fcd = new File(dir);

			if (LOG.isDebugEnabled()) {
				LOG
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
			urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
					.getClassLoader());

			LOG.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			className = getClassName(classAsFile, dir, CLASS_FILE_EXTENSION);
			// remove Helper
			className = className.substring(0, className.length() - 6);
			LOG.debug("class name: " + className);
			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.debug("Class " + className + "is not a type", args,
					e);
			return null;
		}
		
	}
	
	/**
	 * loadTypeClass - loads type
	 * 
	 * @param dir
	 * @param classAsFile
	 * @return
	 * @throws ClassGenerationException
	 */
	private static Class loadHelperClass(String dir, File classAsFile) throws ClassGenerationException {
		URLClassLoader urlClassLoader = null;
		String className = null;

		try {
			File fcd = new File(dir);

			if (LOG.isDebugEnabled()) {
				LOG
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
			urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
					.getClassLoader());

			LOG.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			className = getClassName(classAsFile, dir, CLASS_FILE_EXTENSION);
			
			LOG.debug("class name: " + className);
			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.debug("Class " + className + "is not a type", args,
					e);
			return null;
		}
		
	}

	/**
	 * This method extracs all the class used as method's parameter or method's
	 * result.
	 * 
	 * @param dir
	 *            The dir
	 * @param classList
	 *            The class list
	 * @return the return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Set<Class> findClassUsed(String dir, List<File> classList)
			throws ClassGenerationException {
		LOG.debug(">>>>> findClassUsedInTheOperations - begin");

		LOG.debug("operationsClass=" + classList);

		if (classList == null || classList.size() == 0) {
			LOG.info("CRB000532_Operations_class_not_found");
			return new HashSet<Class>();
		}

		Set<Class> result = new HashSet<Class>();

		for (int i = 0; i < classList.size(); i++) {
			Class clazz = classLoad(dir, classList.get(i));
			LOG.debug("classLoad:" + clazz);

			// I don't want to include the operations class,
			// so I extract the data types used inside
			List<Class> types = UtilClassCollector.extractTypes(clazz);

			for (Class currType : types) {
				result = UtilClassCollector.visitClassCollector(result,
						currType);
			}
		}

		// check Holder
		List<Class> holder = findHolderUsed(dir);
		for (Class currType : holder) {
			result = UtilClassCollector.visitClassCollector(result, currType);
		}

		LOG.debug("<<<<< findClassUsedInTheOperations - end:" + result);
		return result;
	}

	/**
	 * 
	 * @param dir
	 *            The dir
	 * @param classAsFile
	 *            The class AsFile
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Class classLoad(String dir, File classAsFile)
			throws ClassGenerationException {
		URLClassLoader urlClassLoader = null;

		try {
			File fcd = new File(dir);

			if (LOG.isDebugEnabled()) {
				LOG
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
			urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
					.getClassLoader());

			LOG.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			String className = getClassName(classAsFile, dir, CLASS_FILE_EXTENSION);
			LOG.debug("class name: " + className);
			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		}
	}

	/**
	 * Load ths class using the class name in the target directory.
	 * 
	 * @param dir
	 *            The dir
	 * @param classAsFile
	 *            The class AsFile
	 * @param addParent
	 *            if true, link the classLoader with the Util ClassLoader.
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Class classLoad(String dir, String className)
			throws ClassGenerationException {
		return classLoad(dir, className, true);
	}

	/**
	 * Load ths class using the class name in the target directory.
	 * 
	 * @param dir
	 *            The dir
	 * @param classAsFile
	 *            The class AsFile
	 * @param addParent
	 *            if true, link the classLoader with the Util ClassLoader.
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Class classLoad(String dir, String className,
			boolean addParent) throws ClassGenerationException {
		URLClassLoader urlClassLoader = null;
		// ChildFirstClassLoader urlClassLoader = null;

		try {
			File fcd = new File(dir);

			if (LOG.isDebugEnabled()) {
				LOG
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
			if (addParent) {
				urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
						.getClassLoader());
			} else {
				urlClassLoader = new URLClassLoader(new URL[] { u });
			}

			LOG.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			LOG.debug("class name: " + className);
			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { PROTOCOL
					+ new File(dir).getAbsolutePath() + "/" };

			LOG.error("CRB000533_Could_not_instantiate_url_class_loader", args,
					e);
			throw new ClassGenerationException(
					"CRB000533_Could_not_instantiate_url_class_loader", args, e);
		}
	}

	/**
	 * This method find all the '.class' files in classesDir.
	 * 
	 * @param classesDir
	 *            The directory to inspect.
	 * @param classLoader
	 *            The class loader used to istantiate the classes.
	 * 
	 * @return The list of files found.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static List<Class> findGeneratedClasses(String classesDir,
			ClassLoader classLoader) throws ClassGenerationException {

		LOG.debug(">>>>> findGeneratedClasses - begin");

		LOG.debug("INPUT. classesDir=" + classesDir + "; classLoader"
				+ classLoader);

		List<Class> classes = new ArrayList<Class>();
		List<File> classFiles = Util.findFilesFromSourceDirectory(classesDir,
				CLASS_FILE_EXTENSION);
		LOG.debug("class files: " + classFiles);
		for (File classFile : classFiles) {
			String className = getClassName(classFile, classesDir, CLASS_FILE_EXTENSION);
			LOG.debug("class name: " + className);
			try {
				Class clazz = classLoader.loadClass(className);
				LOG.debug("clazz=" + clazz + " ... ");

				classes.add(clazz);
				LOG.debug("... clazz=" + clazz + " added.");
			} catch (ClassNotFoundException e) {
				Object[] args = new Object[] { className };

				LOG.error("CRB000534_Could_not_instantiate_class", args, e);
				throw new ClassGenerationException(
						"CRB000534_Could_not_instantiate_class", args, e);
			} catch (ClassFormatError e) {
				Object[] args = new Object[] { className };

				LOG.error("CRB000534_Could_not_instantiate_class", args, e);
				throw new ClassGenerationException(
						"CRB000534_Could_not_instantiate_class", args, e);
			}
		}

		LOG.debug("<<<<< findGeneratedClasses - end");
		return classes;
	}
	
	/**
	 * Gets the class name from the file absolute path, representing both 
	 * "java" and "class" file names
	 * @param absoluteFilePath
	 *            The file absolute path
	 * @param basedir
	 *            The base dir path
	 * @param extension ".class" or ".java"           
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static String getClassName(String absoluteFilePath, String basedir, String extension)
			throws ClassGenerationException {
		LOG.debug(">>>>> getClassName - begin");
		
		String absoulteBaseDir = new File(basedir).getAbsolutePath();

		LOG.debug("absoluteFileName: " + absoluteFilePath
				+ "; absoulteBaseDir: " + absoulteBaseDir);

		if (!absoluteFilePath.startsWith(absoulteBaseDir)) {
			Object[] args = new Object[] { absoluteFilePath,
					absoulteBaseDir };

			LOG.error("CRB000535_Classfile_is_not_under_dir", args);
			throw new ClassGenerationException(
					"CRB000535_Classfile_is_not_under_dir", args, null);
		}

		// +1 excludes the trailing slash
		String relativeFileName = absoluteFilePath.substring(absoulteBaseDir
				.length() + 1);
		LOG.debug("relativeFileName.class=" + relativeFileName);

		// eliminates .class
		if (relativeFileName.endsWith(extension)) {
			relativeFileName = relativeFileName.substring(0, relativeFileName
					.length() - extension.length());
			LOG.debug("relativeFileName=" + relativeFileName);
		}

		String className = relativeFileName.replace(File.separator, ".");
		LOG.debug("className=" + className);

		LOG.debug("<<<<< getClassName - end");
		return className;
	}	

	/**
	 * Gets the class name from the file absolute path, representing both 
	 * "java" and "class" file names
	 * @param file
	 *            The file 
	 * @param basedir
	 *            The base dir path
	 * @param extension ".class" or ".java"           
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static String getClassName(File file, String basedir, String extension)
			throws ClassGenerationException {
			LOG.debug(">>>>> getClassName - begin");
			String absoluteFileName = file.getAbsolutePath();
			return getClassName(absoluteFileName, basedir, extension);
	}


	/**
	 * @param classesDirName
	 *            The classes dir name
	 * @param className
	 *            The class name
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static ClassReader getAsmCLassReader(String classesDirName,
			String className) throws ClassGenerationException {

		LOG.debug(">>>>> getAsmCLassReader - begin");

		ClassReader cr = getAsmCLassReader(classesDirName + File.separator
				+ className);

		LOG.debug("<<<<< getAsmCLassReader - end. ClassReader=" + cr);
		return cr;
	}

	/**
	 * This method returns the class reader used for the bytecode manipulation.
	 * 
	 * @param className
	 *            The absolute path of the class (eg: /var/Foo.class).
	 * 
	 * @return The class reader associated to the input class.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception.
	 */
	public static ClassReader getAsmCLassReader(String className)
			throws ClassGenerationException {

		LOG.debug(">>>>> getAsmCLassReader - begin");
		ClassReader cr = null;
		try {

			cr = new ClassReader(new FileInputStream(className));

		} catch (IOException e) {
			Object[] args = new Object[] { className };

			LOG.error("CRB000536_Could_not_instantiate_class_reader_for_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000536_Could_not_instantiate_class_reader_for_class",
					args, e);
		}

		LOG.debug("<<<<< getAsmCLassReader - end. ClassReader=" + cr);
		return cr;
	}


	/**
	 * @param classesDirName
	 *            The classes dir name
	 * @param relativeFileName
	 *            The relative file name
	 * @param newBytecode
	 *            The new Bytecode
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void saveAsJavaClass(String classesDirName,
			String relativeFileName, byte[] newBytecode)
			throws ClassGenerationException {

		LOG.debug(">>>>> saveAs - begin");

		saveAsJavaClass(classesDirName + File.separator + relativeFileName,
				newBytecode);

		LOG.debug("<<<<< saveAs - end");
	}

	// XXX javadoc
	/**
	 * @param absoluteFileName
	 *            The absolute file name
	 * @param newBytecode
	 *            The new Byte code
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void saveAsJavaClass(String absoluteFileName,
			byte[] newBytecode) throws ClassGenerationException {

		LOG.debug(">>>>> saveAs - begin:" + absoluteFileName);

		try {
			FileOutputStream fos = new FileOutputStream(absoluteFileName);

			fos.write(newBytecode);
			fos.close();
		} catch (FileNotFoundException e) {
			Object[] args = new Object[] { absoluteFileName };

			LOG.error("CRB000537_Could_not_instantiate_file_writer_for_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000537_Could_not_instantiate_file_writer_for_class",
					args, e);
		} catch (IOException e) {
			Object[] args = new Object[] { absoluteFileName };

			LOG.error("CRB000538_Could_not_save_class", args, e);
			throw new ClassGenerationException(
					"CRB000538_Could_not_save_class", args, e);
		}

		LOG.debug("<<<<< saveAs - end");
	}

	/**
	 * This method save a java file.
	 * 
	 * @param code
	 *            The code of the file.
	 * @param dir
	 *            The directory where the file will be save.
	 * @param name
	 *            The name of the file.
	 * 
	 * @return The new File.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception.
	 */
	public static File saveAsJavaSource(String code, String dir, String name)
			throws ClassGenerationException {
		LOG.debug(">>>>> saveAsJavaSource - begin");

		File newFile = new File(dir + File.separator + name + ".java");
		FileWriter fw = null;
		try {
			fw = new FileWriter(newFile);
			fw.write(code.toCharArray());
			fw.close();
		} catch (IOException e) {
			Object[] args = new Object[] { name };

			LOG.error("CRB000539_Saving_file_problem", args, e);
			throw new ClassGenerationException("CRB000539_Saving_file_problem",
					args, e);
		}

		LOG.debug("<<<<< saveAsJavaSource - end");
		return newFile;
	}

	/**
	 * This method saves a file.
	 * 
	 * @param content
	 *            The content of the file stored in a string
	 * @param dir
	 *            The directory where the file will be save.
	 * @param name
	 *            The name of the file.
	 * 
	 * @return The new File.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception.
	 */
	public static File saveFile(String code, String dir, String name)
			throws ClassGenerationException {
		LOG.debug(">>>>> saveFile - begin");

		File newFile = new File(dir + File.separator + name);
		FileWriter fw = null;
		try {
			fw = new FileWriter(newFile);
			fw.write(code.toCharArray());
			fw.close();
		} catch (IOException e) {
			Object[] args = new Object[] { name };

			LOG.error("CRB000539_Saving_file_problem", args, e);
			throw new ClassGenerationException("CRB000539_Saving_file_problem",
					args, e);
		}

		LOG.debug("<<<<< saveFile - end");
		return newFile;
	}
	/**
	 * The files with the 'DefaultFactory.java' suffix instantiate a value type
	 * but the implementation of this data types must be provided by the
	 * developers. To automate the process the method creates a new (empty) java
	 * source file that extends the class of the value type.
	 * 
	 * @param workdirsrc
	 *            The directory where the method look for the 'DefaultFactory'
	 *            java files. The generated files will be placed here.
	 * 
	 * @param addToStringEquals
	 *            If true the method toString and equals will be added.
	 * 
	 * @return The list of a full java name of the value types.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static List<String> valueTypesImpl(String workdirsrc,
			boolean addToStringEquals) throws ClassGenerationException {

		LOG.debug(">>>>> valueTypesImpl - begin");

		List<String> vtList = new ArrayList<String>();

		if (workdirsrc == null) {
			LOG.warn("CRB000540_ValueTypesImpl_cannot_work_workdirsrc_is_null");
			LOG.debug("<<<<< valueTypesImpl - end");
			return vtList;
		}

		try {
			final int suffixLen = "DefaultFactory.java".length();
			List<String> valueTypes = Util.findAllDefaultFactory(workdirsrc);

			// XXX this code is not very clean
			for (int i = 0; i < valueTypes.size(); i++) {
				String item = valueTypes.get(i);
				String itemBase = item.substring(0, item.length() - suffixLen);

				String x = replaceSeparatorWithDot(itemBase);

				String pkg = extractPackage(workdirsrc, x.substring(0, x
						.lastIndexOf(".")));

				String clsSuper = x.substring(x.lastIndexOf(".") + 1);
				String cls = clsSuper + "Impl";

				String pt = workdirsrc + File.separator
						+ replaceDotWithSeparator(pkg);

				LOG.debug("valueTypesImpl;\n PACKAGE = " + pkg
						+ ";\n CLASS   = " + cls + ";\n PATH    = " + pt);

				String jsf = generateJavaSourceFile(pkg, cls, clsSuper,
						addToStringEquals);

				saveAsJavaSource(jsf, pt, cls);

				// update the value type list
				vtList.add(pkg + "." + clsSuper);
			}

		} catch (IOException ioe) {
			LOG.error("CRB000541_Error_during_generation_the_implementation_"
					+ "class_of_the_valuetype", ioe);
			throw new ClassGenerationException(
					"CRB000541_Error_during_generati"
							+ "on_the_implementation_class_of_the_valuetype",
					ioe);
		}

		LOG.debug("<<<<< valueTypesImpl - end. #valueTypes:" + vtList.size());
		return vtList;
	}

	/**
	 * This method generates a java class with no methods and no attributes.
	 * 
	 * @param pkg
	 *            The 'package' of the class.
	 * @param className
	 *            The name of the class.
	 * @param parent
	 *            The super class.
	 * 
	 * @param addToStringEquals
	 *            If true the code will contain the methods toString and equals.
	 * 
	 * @return The code generated
	 * 
	 * @throws ClassGenerationException
	 */
	private static String generateJavaSourceFile(String pkg, String className,
			String parent, boolean addToStringEquals) {

		LOG.debug(">>>>> generateJavaSourceFile - begin");

		String code = null;
		if (addToStringEquals) {
			String importString = "import org.apache.commons.lang.builder.EqualsBuilder;\n"
					+ "import org.apache.commons.lang.builder.ReflectionToStringBuilder;\n";

			String toStringMethod = "public String toString() {\n"
					+ "return ReflectionToStringBuilder.toString(this);\n"
					+ "}\n";

			String equalsMethod = "public boolean equals(Object obj) {\n"
					+ "return EqualsBuilder.reflectionEquals(this, obj);\n"
					+ "}\n";

			code = "//it.imolinfo.jbi4corba - auto generated code\n"
					+ "package " + pkg + ";\n" + importString + "public class "
					+ className + " extends " + parent + " {\n "
					+ toStringMethod + equalsMethod + "}\n";
		} else {
			code = "//it.imolinfo.jbi4corba - auto generated code\n"
					+ "package " + pkg + ";\n" + "public class " + className
					+ " extends " + parent + " {\n " + "}\n";
		}

		LOG.debug("<<<<< generateJavaSourceFile - end. code:" + code);
		return code;
	}

	/**
	 * This method replaces the file sperator with a dot.
	 * 
	 * @param s
	 *            The working string.
	 * 
	 * @return The new string.
	 */
	public static String replaceSeparatorWithDot(String s) {
		LOG.debug("replaceSeparatorWithDot. the input is " + s);
		if (s == null) {
			LOG.debug("replaceSeparatorWithDot. "
					+ "the input is null. returning empty String");
			return "";
		}
		// else
		if ("".equals(s)) {
			LOG.debug("replaceSeparatorWithDot. "
					+ "the input is an empty String. returning empty String");
			return "";
		}
		// else
		// PACIUF String re = "\\".equals(File.separator) ? "\\\\" :
		// File.separator;
		// PACIUF String res = s.replaceAll(re, ".").replaceAll("/", ".");
		String res = s.replace('\\', '.').replace('/', '.');

		LOG.debug("replaceSeparatorWithDot. " + "the input is " + s
				+ " returning " + res);
		return res;
	}

	/**
	 * This method replaces the dot with a file sperator.
	 * 
	 * @param s
	 *            The working path.
	 * 
	 * @return The new string.
	 */
	public static String replaceDotWithSeparator(String s) {
		LOG.debug("replaceDotWithSeparator. the input is " + s);
		if (s == null) {
			LOG.debug("replaceDotWithSeparator. "
					+ "the input is null. returning empty String");
			return "";
		}
		// else
		if ("".equals(s)) {
			LOG.debug("replaceDotWithSeparator. "
					+ "the input is an empty String. returning empty String");
			return "";
		}
		// else
		String rep = "\\".equals(File.separator) ? "\\\\" : File.separator;
		String res = s.replaceAll("\\.", rep);
		LOG.debug("replaceDotWithSeparator. " + "the input is " + s
				+ " returning " + res);
		return res;
	}

	/**
	 * This method is used to extract the package of the class in input.
	 * 
	 * @param basedir
	 *            The source directory
	 * @param filename
	 *            The absolute path of the directory that contains a java class.
	 */
	private static String extractPackage(String basedir, String filename) {
		LOG.debug(">>>>> extractPackage - begin");

		// to normalize the basedir and the filename
		// we replace all the separator with a dot
		String bd = replaceSeparatorWithDot(basedir);
		String fn = replaceSeparatorWithDot(filename);
		LOG.debug("\nbasedir  =" + bd + ";\nfilename =" + fn);

		/*
		 * Example: If the basedir is '/src' and the filename is '/src/foo' Then
		 * the normalized names are: '.src' and '.src.foo'
		 * 
		 * ...
		 * 
		 * To extract the package name 'foo' we apply the following algorithm: -
		 * find the basedirIndex basedirIndex = 0 - remove the part of the
		 * filename before the basedir (if any) fn = .src.foo.Bar - remove from
		 * the filename the basedir fn = foo
		 */
		LOG.debug("fn.indexOf(bd)=" + fn.indexOf(bd));
		LOG.debug("fn.substring(fn.indexOf(bd))="
				+ fn.substring(fn.indexOf(bd)));
		LOG.debug("bd.length() + 1=" + (bd.length() + 1));
		LOG.debug("fn.substring(fn.indexOf(bd)).substring(bd.length() + 1)="
				+ fn.substring(fn.indexOf(bd)).substring(bd.length() + 1));
		String pack = fn.substring(fn.indexOf(bd)).substring(bd.length() + 1);
		LOG.debug("PACK=" + pack);

		LOG.debug("<<<<< extractPackage - end");
		return pack;
	}

	/**
	 * 
	 * @param basedir
	 *            The base dir
	 * @param filename
	 *            The file name
	 * @param extension
	 *            The extension
	 * @return The return
	 */
	// XXX javadoc
	private static String extractJavaQualifiedName(String basedir,
			String filename, String extension) {

		LOG.debug(">>>>> extractJavaQualifiedName - begin");

		int extLen = (extension == null) ? 0 : extension.length();

		String bd = replaceSeparatorWithDot(basedir);
		String fn = replaceSeparatorWithDot(filename);
		LOG.debug("\nbasedir  =" + bd + ";\nfilename =" + fn);

		// (filename - .java) - basedir
		int bdIndex = fn.indexOf(bd) + bd.length() + 1;
		String full = fn.substring(bdIndex, fn.length() - extLen);

		LOG.debug("<<<<< extractJavaQualifiedName - end. return=" + full);
		return full;
	}

	/**
	 * XXX javadoc.
	 * 
	 * @param vtList
	 *            The vt list
	 * @param classesDirName
	 *            The classes dir name
	 * @param cl
	 *            The class loader
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 * 
	 */
	public static Map<String, Object> valueTypeMapHandler(List<String> vtList,
			String classesDirName, ClassLoader cl)
			throws ClassGenerationException {

		LOG.debug(">>>>> valueTypeMapHandler - begin");

		Map<String, Object> map = new HashMap<String, Object>();

		// for each valuetype ...
		for (String vt : vtList) {

			// ... get the ID of the valuetype from the helper class ...
			String valueTypeId = getValueTypeIdFromHelperClass(vt + "Helper",
					cl);

			// ... and the factory instance.
			Object valueTypeDefaultFactoryInstance = getValueTypeDefaultFactoryInstance(
					vt + "DefaultFactory", cl);

			if (valueTypeId == null) {
				LOG.warn("CRB000523_ID_null_for_valuetype", vt);
			} else {
				map.put(valueTypeId, valueTypeDefaultFactoryInstance);
			}
		}

		LOG.debug("<<<<< valueTypeMapHandler - end");
		return map;
	}

	/**
	 * XXX javadoc.
	 * 
	 * @param vtList
	 *            The vt list
	 * @param classesDirName
	 *            The classes dir name
	 * @param cl
	 *            The class loader
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 * 
	 */
	public static Map<String, Object> valueTypeMapHandlerWithJbi4corbaFactory(
			List<String> vtList, String classesDirName, ClassLoader cl)
			throws ClassGenerationException {

		LOG.debug(">>>>> valueTypeMapHandler - begin");

		Map<String, Object> map = new HashMap<String, Object>();

		// for each valuetype ...
		for (String vt : vtList) {

			// ... get the ID of the valuetype from the helper class ...
			String valueTypeId = getValueTypeIdFromHelperClass(vt + "Helper",
					cl);

			// ... and the factory instance.
			Object valueTypeDefaultFactoryInstance = getValueTypeDefaultFactoryInstance(
					vt + "DefaultFactoryJbi4corba", cl);

			if (valueTypeId == null) {
				LOG.warn("CRB000523_ID_null_for_valuetype", vt);
			} else {
				map.put(valueTypeId, valueTypeDefaultFactoryInstance);
			}
		}

		LOG.debug("<<<<< valueTypeMapHandler - end");
		return map;
	}

	/**
	 * 
	 * @param factory
	 *            The factory
	 * @param cl
	 *            The class loader
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	private static Object getValueTypeDefaultFactoryInstance(String factory,
			ClassLoader cl) throws ClassGenerationException {

		LOG
				.debug(">>>>> getValueTypeDefaultFactoryInstance - begin:"
						+ factory);

		Class c = null;
		try {

			c = cl.loadClass(factory);

		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000524_Error_creating_an_instance_of", args, e);
			throw new ClassGenerationException(
					"CRB000524_Error_creating_an_instance_of", args, e);
		}
		LOG.debug("FactoryClass:" + c);

		Object o = null;
		try {

			o = c.newInstance();

		} catch (InstantiationException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000524_Error_creating_an_instance_of", args, e);
			throw new ClassGenerationException(
					"CRB000524_Error_creating_an_instance_of", args, e);
		} catch (IllegalAccessException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000524_Error_creating_an_instance_of", args, e);
			throw new ClassGenerationException(
					"CRB000524_Error_creating_an_instance_of", args, e);
		}

		LOG.debug("<<<<< getValueTypeDefaultFactoryInstance - end:" + o);
		return o;
	}

	/**
	 * 
	 * @param helper
	 *            The helper
	 * @param cl
	 *            The class loader
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static String getValueTypeIdFromHelperClass(String helper,
			ClassLoader cl) throws ClassGenerationException {

		LOG.debug(">>>>> getValueTypeIdFromHelperClass - begin:" + helper);

		Class c = null;
		try {
			c = cl.loadClass(helper);
		} catch (ClassNotFoundException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		}
		LOG.debug("HelperClass:" + c);

		Object o = null;
		try {

			o = c.getMethod("id", (Class[]) null).invoke(null, (Object[]) null);

		} catch (IllegalArgumentException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		} catch (SecurityException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		} catch (IllegalAccessException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		} catch (InvocationTargetException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		} catch (NoSuchMethodException e) {
			Object[] args = new Object[] { c };

			LOG.error("CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
			throw new ClassGenerationException(
					"CRB000525_Error_getting_the_value_type_id_from_class",
					args, e);
		}

		LOG.debug("<<<<< getValueTypeIdFromHelperClass - end:" + o);
		return (String) o;
	}

	/**
	 * The list of names of the jars used in the classpath.
	 * 
	 * @param libDirName
	 *            The directory where the jars are located.
	 * 
	 * @return The list of names of the jars used in the classpath.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static List<String> prepareClassPath(String libDirName)
			throws ClassGenerationException {

		LOG.debug(">>>>> prepareClassPath - begin for directory: libDirName");
		List<File> jarFiles = Util.findFilesFromSourceDirectory(libDirName,
				".jar");

		List<String> jarFilesName = new ArrayList<String>();

		for (File jarFile : jarFiles) {
			try {
				LOG.debug("Adding jar " + jarFile.getCanonicalPath() + " ... ");

				jarFilesName.add(jarFile.getCanonicalPath());

				LOG.debug("... jar " + jarFile.getCanonicalPath() + " added.");
			} catch (IOException e) {
				Object[] args = new Object[] { jarFile, e.getMessage() };

				LOG.error("CRB000507_Error_getting_canonicalPath_from_file",
						args, e);
				throw new ClassGenerationException(
						"CRB000507_Error_getting_canonicalPath_from_file",
						args, e);
			}
		}

		LOG.debug("<<<<< prepareClassPath - end");
		return jarFilesName;
	}

	/**
	 * This method generates the new valuetypes factories. For each valuetype we
	 * generate a class that extends the default value factory. The created
	 * classes have the suffix 'Jbi4corba' (e.g.:
	 * MyValueTypeDefaultFactoryJbi4corba.java).
	 * 
	 * @param vtList
	 *            The list of valuetypes.
	 * @param basedir
	 *            The directory where the sources are located.
	 * 
	 * @return The list of new factory (as full java name).
	 * 
	 * @throws ClassGenerationException
	 *             If the one new factory cannot saved.
	 */
	public static List<String> generateValueTypeFactoryJbi4corba(
			List<String> vtList, String basedir)
			throws ClassGenerationException {
		LOG.debug(">>>>> generateValueTypeFactoryJbi4corba - begin");

		List<String> newFactoryList = new ArrayList<String>();

		if (vtList == null || vtList.size() == 0) {
			LOG.debug("No factory to create.");
			return newFactoryList;
		}
		// else
		for (String vt : vtList) {
			int lastDot = vt.lastIndexOf(".");
			String factoryPackage = vt.substring(0, lastDot);
			String factoryName = vt.substring(lastDot + 1) + "DefaultFactory";
			String newFactoryName = factoryName + "Jbi4corba";

			String newFactorySource = "// JBI4CORBA - Code Generation\n"
					+ "package " + factoryPackage + ";\n" + "public class "
					+ newFactoryName + " extends " + factoryName + " {\n\n"
					// no extra code for this class
					+ "}\n";

			newFactoryList.add(factoryPackage + "." + newFactoryName);

			String targetDir = basedir + File.separator
					+ factoryPackage.replace('.', File.separatorChar);
			saveAsJavaSource(newFactorySource, targetDir, newFactoryName);

			if (LOG.isDebugEnabled()) {
				LOG.debug("Generated a new factory:" + factoryPackage + "."
						+ newFactoryName);
			}
		}

		LOG.debug("<<<<< generateValueTypeFactoryJbi4corba - end."
				+ "Number of new factory generated:" + newFactoryList.size());

		return newFactoryList;
	}

	/**
	 * This method modify the bytecode of a class: add Serializable interface
	 * and set the serial version UID.
	 * 
	 * @param absPath
	 *            The absolute path of the class to modify.
	 * @param newSerialVersionUid
	 *            The new serialVersionUID.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static void tweakSerializableDecoration(String absPath,
			Long newSerialVersionUid) throws ClassGenerationException {

		ClassWriter cw = new ClassWriter(true); // visitMaxs
		ClassVisitor cc = new CheckClassAdapter(cw);
		StringWriter sw = new StringWriter();
		ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

		SerializableDecorationAdapter cv = new SerializableDecorationAdapter(
				tv, newSerialVersionUid);

		ClassReader cr = Util.getAsmCLassReader(absPath);

		cr.accept(cv, true);
		LOG.debug("ClassReader.accept ... done");

		LOG.debug("output of tracer during creation of class: " + absPath
				+ "\n" + sw.toString());

		byte[] newBytecode = cw.toByteArray();

		Util.saveAsJavaClass(absPath, newBytecode);
	}

	/**
	 * This method inspect a class to verify if it is Serilizable and to extract
	 * the serial version UID (if exists).
	 * 
	 * @param absPath
	 *            The absolute path of the class to inspect.
	 * 
	 * @return The meta info of the class visited.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static ClassMetaInfo tweakSerializableInspection(String absPath)
			throws ClassGenerationException {

		ClassWriter cw = new ClassWriter(true); // visitMaxs
		ClassVisitor cc = new CheckClassAdapter(cw);
		StringWriter sw = new StringWriter();
		ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

		SerializableInspectorAdapter cv = new SerializableInspectorAdapter(tv);

		ClassReader cr = Util.getAsmCLassReader(absPath);

		cr.accept(cv, true);
		LOG.debug("ClassReader.accept ... done");

		return cv.getClassMetaInfo();
	}

	/**
	 * This method is used to discover the 'oneway' operations inside a corba
	 * stub.
	 * 
	 * @param bin
	 *            The classes directory.
	 * 
	 * @return The map key is the full java name of the class inspected and the
	 *         value is the list of oneway operations associated to the class.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Map<String, List<String>> findOnewayOperations(String bin)
			throws ClassGenerationException {

		// find all the corba stubs ...
		List<File> binList = findFilesFromSourceDirectory(bin, "Stub.class");

		Map<String, List<String>> global = new HashMap<String, List<String>>();

		// ... for each file
		for (File stubFile : binList) {
			String absPath = stubFile.getAbsolutePath();

			// ... inspect the bytecode
			Map<String, List<String>> map = tweakOnewayInspection(absPath);

			if (map.size() == 0) {
				LOG.debug("No 'oneway' operations for " + absPath);
			} else {
				global.putAll(map);
			}
		}

		return global;
	}

	/**
	 * This method is used to inspect the bytecode of a class to discover the
	 * corba 'oneway' requests.
	 * 
	 * @param absPath
	 *            The absolute path of the corba stub.
	 * 
	 * @return The map key is the full java name of the class inspected and the
	 *         value is the list of oneway operations associated to the class.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Map<String, List<String>> tweakOnewayInspection(String absPath)
			throws ClassGenerationException {

		ClassWriter cw = new ClassWriter(true); // visitMaxs
		ClassVisitor cc = new CheckClassAdapter(cw);
		StringWriter sw = new StringWriter();
		ClassVisitor cv = new TraceClassVisitor(cc, new PrintWriter(sw));

		CorbaOnewayAdapter ca = new CorbaOnewayAdapter(cv, sw);

		ClassReader cr = getAsmCLassReader(absPath);

		cr.accept(ca, true);
		LOG.debug("ClassReader.accept ... done");

		LOG.debug("Class=" + ca.getAssociatedInterface()
				+ "; Oneway operations:" + ca.getOnewayOperationList().size());

		Map<String, List<String>> map = new HashMap<String, List<String>>();

		if (ca.getOnewayOperationList().size() != 0) {
			map.put(ca.getAssociatedInterface(), ca.getOnewayOperationList());
		}

		return map;
	}

	/**
	 * This method finds the corba enumerations and replace them with a standard
	 * enum classes. These classes implement the IDLEntity interface and provide
	 * two methods used from the helper classes.
	 * 
	 * @param src
	 *            The source directory.
	 * @param bin
	 *            The classes directory.
	 * 
	 * @return The map that contains all the corba enum. The key of the map is
	 *         the full java name of the corba enum and the value is the list of
	 *         the associated label.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Map<String, List<String>> replaceCorbaEnumaration(String src,
			String bin) throws ClassGenerationException {

		// finding the corba enumeration with the associated labels.
		Map<String, List<String>> corbaEnumMap = findCorbaEnum(bin);

		// for each corba enum ...
		for (String corbaEnum : corbaEnumMap.keySet()) {
			LOG.debug("corbaEnum=" + corbaEnum);

			// ... extracting the associated labels
			List<String> labelList = corbaEnumMap.get(corbaEnum);

			// extract the package and the simple class name
			int lastDot = corbaEnum.lastIndexOf('.');
			String aPack = corbaEnum.substring(0, lastDot);
			String aClas = corbaEnum.substring(lastDot + 1);

			// ... generating a 'special' enum class
			String newEnumSource = generateSpecialEnumClass(aPack, aClas,
					labelList);

			// ... saving the new file (code, dir, name)
			String dir = src + File.separator
					+ aPack.replace('.', File.separatorChar);
			saveAsJavaSource(newEnumSource, dir, aClas);

			// ... removing the old compiled class
			String flatBin = bin.replace('/', File.separatorChar).replace('\\',
					File.separatorChar);

			String zombie = flatBin + File.separator
					+ corbaEnum.replace('.', File.separatorChar) + CLASS_FILE_EXTENSION;

			File zombieFile = new File(zombie);

			if (zombieFile.exists()) {
				LOG.debug("The file " + zombie + " exists.");
			} else {
				LOG.debug("The file " + zombie + " does NOT exists.");
			}

			boolean deleteResult = zombieFile.delete();
			if (deleteResult) {
				LOG.debug("Removing file " + zombie + " ... ok");
			} else {
				LOG.debug("Removing file " + zombie + " ... failure");
			}

		}

		return corbaEnumMap;
	}

	/**
	 * This method generates the source code of a standard enum class with some
	 * special features. It implements IDLEntity and provides the methods
	 * 'value' and 'from_int'.
	 * 
	 * @param aPackage
	 *            The package of the class.
	 * @param aClassName
	 *            The simple class name.
	 * @param labelList
	 *            The list of label for the enumaration.
	 * 
	 * @return The source code generated.
	 */
	public static String generateSpecialEnumClass(String aPackage,
			String aClassName, List<String> labelList) {

		StringBuffer labelSource = new StringBuffer("    ");
		if (labelList == null || labelList.size() == 0) {
			String enumClass = aPackage + "." + aClassName;
			Object[] args = new Object[] { enumClass };
			LOG.warn("CRB000549_EmptyEnum", args);
			labelSource.append(";");
		} else {
			for (int i = 0; i < labelList.size(); i++) {

				if (i == (labelList.size() - 1)) {
					labelSource.append(labelList.get(i) + ";");
				} else {
					labelSource.append(labelList.get(i) + ",");
				}
			}
		}

		StringBuffer staticMembers = new StringBuffer();
		int idx = 0;
		for (String member : labelList) {
			staticMembers.append("public static final int ");
			staticMembers.append("_" + member + " ");
			staticMembers.append("= ");
			staticMembers.append(idx++);
			staticMembers.append(";");
			staticMembers.append("\n");
		}
		StringBuffer source = new StringBuffer();
		source.append("package " + aPackage + ";");
		source.append("\n\n");
		source.append("public enum " + aClassName);
		source.append(" implements org.omg.CORBA.portable.IDLEntity { ");
		source.append("\n\n");
		source.append(labelSource);
		source.append("\n\n");
		source.append("public int value() {return ordinal();} ");
		source.append("\n\n");
		source.append("public static " + aClassName);
		source.append(" from_int (int value) { return " + aClassName
				+ ".values()[value]; } ");
		source.append("\n\n");
		source.append(staticMembers.toString());
		source.append("}");

		return source.toString();
	}

	/**
	 * This method find the corba enum.
	 * 
	 * @param bin
	 *            The classes directory.
	 * 
	 * @return The corba enums. The key of the map is the path of the class and
	 *         the value is the ordered list of the possible enum labels.
	 * 
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	protected static Map<String, List<String>> findCorbaEnum(String bin)
			throws ClassGenerationException {

		Map<String, List<String>> corbaEnumMap = new HashMap<String, List<String>>();

		// finding all the compiled classes ...
		List<File> binList = findFilesFromSourceDirectory(bin, CLASS_FILE_EXTENSION);

		// ... inspecting all of them to find a corba enum class
		for (File currentFile : binList) {

			String absPath = currentFile.getAbsolutePath();

			ClassWriter cw = new ClassWriter(true); // visitMaxs
			ClassVisitor cc = new CheckClassAdapter(cw);
			StringWriter sw = new StringWriter();
			ClassVisitor cv = new TraceClassVisitor(cc, new PrintWriter(sw));

			CorbaEnumAdapter ca = new CorbaEnumAdapter(cv);

			ClassReader cr = getAsmCLassReader(absPath);

			cr.accept(ca, true);
			LOG.debug("ClassReader.accept ... done");

			String className = ca.getInternalClassName().replace('/', '.');

			if (ca.isCorbaEnum()) {
				LOG.debug("The class " + className + " is a corba enum.");

				corbaEnumMap.put(className, ca.getEnumLabelList());
			} else {
				LOG.debug("The class " + className + " is NOT a corba enum.");
			}

		}

		return corbaEnumMap;
	}

	/**
	 * This method load the class 'remoteClassName' using the ClassLoader in
	 * input and generates an implementation class.
	 * 
	 * The method expects an interface as imput class.
	 * 
	 * @param urlClassLoader
	 *            The ClassLoader used to load the class.
	 * @param remoteClassName
	 *            The full name of the Interface to implement.
	 * @param addRemoteSupport
	 *            Check if adding Remote interface to the implementation class is needed	
	 * 
	 * @return The source code generated.
	 * 
	 * @throws ClassGenerationException
	 *             When the class name is empty or the class does not exist.
	 */
	public static String generateImplementationClass(
			ClassLoader urlClassLoader, String remoteClassName, boolean addRemoteSupport)
			throws ClassGenerationException {

		String sRemoteInterface=null;
		if (addRemoteSupport)
		{
			sRemoteInterface = ", java.rmi.Remote";
		}
		else
		{
			sRemoteInterface = "";
		}
		LOG.debug("generateRemoteClassImplementation" + "; ClassLoader="
				 + "; remoteClassName=" + remoteClassName);

		// check
		if (remoteClassName == null || "".equals(remoteClassName)) {
			Object[] arg = new Object[] { "remoteClassName" };
			LOG.error("CRB000551_NotEmptyJavaClassName", arg);
			throw new ClassGenerationException(
					"CRB000551_NotEmptyJavaClassName", arg);
		}

		// collect the method's signature of the abstract methods.
		Class remoteClass = null;
		try {
			remoteClass = urlClassLoader.loadClass(remoteClassName);
		} catch (ClassNotFoundException e) {
			Object[] arg = new Object[] { "remoteClassName" };
			LOG.error("CRB000552_ClassNotFound", arg, e);
			throw new ClassGenerationException("CRB000552_ClassNotFound", arg,
					e);
		}

		if (remoteClass == null) {
			Object[] arg = new Object[] { "remoteClassName" };
			LOG.error("CRB000552_ClassNotFound", arg);
			throw new ClassGenerationException("CRB000552_ClassNotFound", arg);
		}

		// I want to save the class in the same package of the interface.
		String pack = remoteClass.getPackage().getName();

		// The default name of the implementation class.
		String name = remoteClass.getSimpleName() + "Impl";

		LOG.debug("The name of the Implementation Class:" + pack + "." + name);

		List<MethodSignature> msList = new ArrayList<MethodSignature>();
		Method[] arrayOfMethod = remoteClass.getMethods();

		// FOR EACH method ...
		for (Method method : arrayOfMethod) {

			// ... that is an abstract method ...
			int mod = method.getModifiers();

			// check ( ... but is possible ? )
			if (!Modifier.isAbstract(mod)) {
				Object[] arg = new Object[] { method.getName() };
				LOG.warn("CRB000553_UnexpectedNonAbstractMethod", arg);
				continue;
			}

			// ... I create an object that represent the associated signature
			MethodSignature ms = new MethodSignature();

			// method name
			ms.setMethodName(method.getName());

			// parameter types
			List<Param> typeList = new ArrayList<Param>();
			Class[] arrayOfType = method.getParameterTypes();
			for (Class type : arrayOfType) {
				String canonical = type.getCanonicalName();
				Param param = new Param();
				param.setTypeName(canonical);
				typeList.add(param);
				LOG.debug("Parameter=" + canonical);
			}
			ms.setParameters(typeList);
			arrayOfType = null;

			// return type
			Class returnClass = method.getReturnType();
			ms.setReturnType(returnClass.getCanonicalName());

			// exception types
			List<String> exceptions = new ArrayList<String>();
			arrayOfType = method.getExceptionTypes();
			for (Class type : arrayOfType) {
				String canonical = type.getCanonicalName();
				exceptions.add(canonical);

				LOG.debug("Exception=" + canonical);
			}
			ms.setExceptionsType(exceptions);

			// add to the list
			msList.add(ms);
		}

		String cihFullname = "it.imolinfo.jbi4corba.webservice.runtime."
				+ "ConsumerInvocationHandler";

		// When I have collected all the method's signature I use it to generate
		// the source code of the class.
		String implSource = "package " + pack + ";" + "\n\n" + "public class "
				+ name + " implements " + remoteClassName
				+ sRemoteInterface+"{" 
				+ "\n\n" + " protected " + cihFullname
				+ " consumerInvocationHandler;" + "\n\n" + "\n  public " + name
				+ "() {" + "\n    // NOP" + "\n  }" + "\n\n" + "\n  public "
				+ name + "(" + cihFullname + " cih) {"
				+ "\n    consumerInvocationHandler = cih;" + "\n  }" + "\n\n"
				+ methodImplementation(msList,addRemoteSupport) + "\n\n" + "}\n";

		if (LOG.isDebugEnabled()) {
			LOG.debug("\n--------\n" + implSource + "\n--------\n");
		}
		return implSource;
	}

	/**
	 * This method is used to generate the source code of the methods in the
	 * list.
	 * 
	 * @param msList
	 *            A list of method's signature.
	 * 
	 * @return The default source code of all the methods.
	 */
	protected static String methodImplementation(List<MethodSignature> msList, boolean addRemoteSupport) {

		// If I have no methods I return an empty string.
		if (msList == null || msList.isEmpty()) {
			return "";
		}

		// else

		String src = "";

		// FOR EACH method's signature ...
		for (MethodSignature ms : msList) {

			// ... I generate the list of the parameters using the full
			// qualified name
			// for the data type to avoid the explicit import.
			String paramSource = "";
			for (int i = 0; i < ms.getParameters().size(); i++) {
				if (i == 0) {
					paramSource = ms.getParameters().get(i).getTypeName()
							+ " param" + i;
				} else {
					paramSource += ", "
							+ ms.getParameters().get(i).getTypeName()
							+ " param" + i;
				}
			}

			// ... I generate the body of the method using the return type.
			// If the return type is a primitive data type I return zero,
			// otherwise I return null.
			// If I have no return type I simply add the return statement.

			// ... I generate the 'throws' statement of the method.
			String exceptionSource = "";
			for (int i = 0; i < ms.getExceptionsType().size(); i++) {
				if (i == 0) {
					exceptionSource = "throws " + ms.getExceptionsType().get(i);
				} else {
					exceptionSource += ", " + ms.getExceptionsType().get(i);
				}
			}

			// ... and, at the end, I generate the source code of the method.
			String methodSource = "public " + ms.getReturnType() + " "
					+ ms.getMethodName() + "(" + paramSource + ") "
					+ exceptionSource + " { "
					+ generateTheBodyOfTheMethodForImplTie(ms, addRemoteSupport) + " }";

			// ... all the methods is appended to the source.
			src += methodSource + "\n\n";
		}

		return src;
	}

	/**
	 * This method is used to generate the body of the methods of the the class
	 * that must be implement the skeleton (tie) previously created.
	 * 
	 * The body call the method 'public Object invoke(Object proxy, Method
	 * method, Object[] args) throws Throwable;' of a ConsumerInvocationHandler
	 * object where - proxy is this class - method is the method to be invoked -
	 * args are the parameters of the method invoked.
	 * 
	 * This 'call' may raise a 'Throwable' exception that must be caught. By
	 * design the only 2 kind of exception raised are: -
	 * Jbi4CorbaRuntimeException; - Jbi4CorbaException.
	 * 
	 * In the first case: - the method caught the exception - extracts the
	 * exception's message - throws a RemoteException with the same message.
	 * 
	 * In the second case: - the method caught the exception (in thi case we
	 * have a soap fault) - extracts the fault - throws the fault itself
	 * 
	 * @param methodSignature
	 *            The method's signature.
	 */
	protected static String generateTheBodyOfTheMethodForImplTie(
			MethodSignature methodSignature, boolean addRemoteSupport) {

		String sThrowsStatement;
		String sThrowsStatementWithMesssage;
		if (addRemoteSupport)
		{
			sThrowsStatement = "\n   throw new java.rmi.RemoteException(e.getMessage());";
			sThrowsStatementWithMesssage = "\n     throw new java.rmi.RemoteException(message);";
		}
		else
		{
			sThrowsStatement = "\n   throw new RuntimeException(e.getMessage());";
			sThrowsStatementWithMesssage="\n   throw new RuntimeException(message);";
		}
		
		String arrayOfParamValue = "new Object [] {";
		String arrayOfParamType = "new Class [] {";
		int paramSize = methodSignature.getParameters().size();
		for (int iParam = 0; iParam < paramSize; iParam++) {
			if (iParam == 0) {
				arrayOfParamValue += ("param" + iParam);
				arrayOfParamType += methodSignature.getParameters().get(iParam)
						.getTypeName()
						+ CLASS_FILE_EXTENSION;
			} else {
				arrayOfParamValue += (", param" + iParam);
				arrayOfParamType += (", "
						+ methodSignature.getParameters().get(iParam)
								.getTypeName() + CLASS_FILE_EXTENSION);
			}
		}
		arrayOfParamValue += "}";
		arrayOfParamType += "}";
		LOG.debug("arrayOfParamValue=[" + arrayOfParamValue + "]");
		LOG.debug("arrayOfParamType=[" + arrayOfParamType + "]");

		String getMethodAsString = "\n java.lang.reflect.Method method = null;"
				+ "\n try {" + "\n   method = this.getClass().getMethod(\""
				+ methodSignature.getMethodName() + "\", " + arrayOfParamType
				+ ");" + "\n } catch (SecurityException e) {"
				+ sThrowsStatement
				//+ "\n   throw new java.rmi.RemoteException(e.getMessage());"
				+ "\n } catch (NoSuchMethodException e) {"
				//+ "\n   throw new java.rmi.RemoteException(e.getMessage());"
				+ sThrowsStatement
				+ "\n }" + "\n\n";

		/*
		 * public Object invoke(Object proxy, Method method, Object[] args)
		 * throws Throwable;
		 */

		String allEx = "";
		for (String exceptionType : methodSignature.getExceptionsType()) {
			String ex = "\nif (t instanceof " + exceptionType + ") {"
					+ "  throw (" + exceptionType + ") t;" + "}";
			if ("".equals(allEx)) {
				allEx += ex;
			} else {
				allEx += "\n else " + ex;
			}
		}

		// cast the return type
		String returnCast = null;
		if ("double".equals(methodSignature.getReturnType())) {
			returnCast = "return (Double) cih;";
		} else if ("byte".equals(methodSignature.getReturnType())) {
			returnCast = "return (Byte) cih;";
		} else if ("short".equals(methodSignature.getReturnType())) {
			returnCast = "return (Short) cih;";
		} else if ("int".equals(methodSignature.getReturnType())) {
			returnCast = "return (Integer) cih;";
		} else if ("long".equals(methodSignature.getReturnType())) {
			returnCast = "return (Long) cih;";
		} else if ("float".equals(methodSignature.getReturnType())) {
			returnCast = "return (Float) cih;";
		} else if ("boolean".equals(methodSignature.getReturnType())) {
			returnCast = "return (Boolean) cih;";
		} else if ("char".equals(methodSignature.getReturnType())) {
			returnCast = "return (Character) cih;";
		} else if ("char".equals(methodSignature.getReturnType())) {
			returnCast = "return (Character) cih;";
		} else if ("void".equals(methodSignature.getReturnType())) {
			returnCast = "return;"; // FIXME verify
		} else {
			returnCast = "return (" + methodSignature.getReturnType()
					+ ") cih;";
		}

		String runtime = "it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException";

		String invokeAsString = "\n Object cih = null;" + "\n try {"
				+ "\n   cih = consumerInvocationHandler.invoke(this, method, "
				+ arrayOfParamValue + ");" + "\n } catch (Throwable tx) {"
				+ "\n   if (tx instanceof " + runtime + ") {"
				+ "\n     String message = tx.getMessage();"
				+ sThrowsStatementWithMesssage
				//+ "\n     throw new java.rmi.RemoteException(message);"
				+ "\n   } else {" + "\n     Throwable t = tx.getCause();"
				+ allEx + "\n   }" + "\n }" + "\n\n" + returnCast;

		return getMethodAsString + invokeAsString;
	}

	public static Map<Long, String> extractSerialVersionUid(String classesDir,
			List<String> pathList) throws ClassGenerationException {

		Map<Long, String> uidMap = new HashMap<Long, String>();

		if (pathList == null) {
			// TODO
		}
		// else
		for (String path : pathList) {
			File file = new File(path);
			Class clazz = classLoad(classesDir, file);
			Long uid = extractSerialVersionUid(clazz);

			if (uid == null) {
				// TODO
			} else {
				uidMap.put(uid, path);
			}
		}

		return uidMap;
	}

	public static Long extractSerialVersionUid(Class classToSerialize) {
		Long uid = null;

		if (classToSerialize == null) {
			return uid;
		}

		try {
			classToSerialize.getDeclaredField("serialVersionUID");
			LOG.debug("Class: " + classToSerialize.getName()
					+ " with serialVersionUID");
			return uid;
		} catch (NoSuchFieldException e) {
			LOG.debug("Class: " + classToSerialize.getName()
					+ " without serialVersionUID");
		}

		LOG.debug("Looking for class: " + classToSerialize.getName());

		ObjectStreamClass objectStreamClass = ObjectStreamClass
				.lookup(classToSerialize);

		if (objectStreamClass == null) {
			LOG.info("CRB000560_objectStreamClass_null",
					new Object[] { classToSerialize.getName() });

		} else {

			uid = objectStreamClass.getSerialVersionUID();
			LOG.debug(classToSerialize.getName() + " uid: " + uid);
		}

		return uid;
	}

	// ==================================
	// Methods for debuggin
	// ==================================

	/**
	 * @param message
	 *            The message
	 * @param collection
	 *            The collection
	 */
	public static void debug(String message, Collection collection) {
		if (!LOG.isDebugEnabled()) {
			return;
		}
		// else
		String msg = (message == null) ? "" : message;

		if (collection == null) {
			LOG.debug(msg + "; Collection is null.");
			return;
		}
		// else
		if (collection.size() == 0) {
			LOG.debug(msg + "; Collection is empty.");
			return;
		}
		// else
		int counter = 0;
		for (Object o : collection) {
			LOG.debug(msg + "; Collection[" + (counter++) + "]=" + o);
		}
	}

	/**
	 * @param message
	 *            The message
	 * @param map
	 *            A map
	 */
	public static void debug(String message, Map map) {
		if (!LOG.isDebugEnabled()) {
			return;
		}
		// else
		String msg = (message == null) ? "" : message;

		if (map == null) {
			LOG.debug(msg + "; Map is null.");
			return;
		}
		// else
		if (map.size() == 0) {
			LOG.debug(msg + "; Map is empty.");
			return;
		}
		// else
		for (Object k : map.keySet()) {
			LOG.debug(msg + "; Collection[" + k + "]=" + map.get(k));
		}
	}

	/**
	 * Creates an unique directory form the seed (seed.tmp) in the rootDir.
	 * 
	 * @param rootDir
	 * @param seed
	 * @return
	 * @throws IOException
	 */
	public static synchronized File createUniqueDirectory(File rootDir,
			String seed) throws IOException {

		int index = seed.lastIndexOf('.');

		if (index > 0) {
			seed = seed.substring(0, index);
		}
		File result = null;
		int count = 0;
		while (result == null) {
			String name = seed + "." + count + ".tmp";
			File file = new File(rootDir, name);
			if (!file.exists()) {
				file.mkdirs();
				result = file;
			}
			count++;
		}
		return result;
	}

	/**
	 * Builds the directory.
	 * 
	 * @param file
	 * @return
	 */
	public static boolean buildDirectory(File file) {
		return file.exists() || file.mkdirs();
	}

	/**
	 * Copy an input stream to an output.
	 * 
	 * @param in
	 * @param out
	 * @throws IOException
	 */
	public static void copyInputStream(InputStream in, OutputStream out)
			throws IOException {

		byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];

		int len = in.read(buffer);
		while (len >= 0) {
			out.write(buffer, 0, len);
			len = in.read(buffer);
		}
		in.close();
		out.close();
	}

	private static List<Class> findHolderUsed(String dir)
			throws ClassGenerationException {

		LOG.debug("findHolderUsed - Begin");

		List<Class> result = new ArrayList<Class>();
		List<File> filesHolder = findFilesFromSourceDirectory(dir,
				"Holder.class");
		LOG.debug("filesHolder: " + filesHolder);
		for (int i = 0; i < filesHolder.size(); i++) {
			String nameClassHolder = filesHolder.get(i).getName();
			LOG.debug("classHolder Name: " + nameClassHolder);
			nameClassHolder = nameClassHolder.substring(0, nameClassHolder
					.length() - 12);

			List<File> listHolder = findFilesFromSourceDirectory(dir,
					nameClassHolder + CLASS_FILE_EXTENSION);
			if (listHolder != null && listHolder.size() > 0) {
				for (File holder : listHolder) {
					Class clazzHolder = classLoad(dir, holder);
					if (!clazzHolder.isInterface()) {
						LOG.debug("Found class used in the Holder: "
								+ clazzHolder.getName());
						result.add(clazzHolder);
					}
				}
			}
		}
		LOG.debug("findHolderUsed - End");
		return result;
	}

	/**
	 * Try to load the class the class name in the target directory; if no class
	 * is found, NO EXCEPTION IS THROWN.
	 * 
	 * @param dir
	 *            The dir
	 * @param classAsFile
	 *            The class AsFile
	 * @return The return
	 * @throws ClassGenerationException
	 *             The class generation exception
	 */
	public static Class classLoadQuiet(String dir, String className)
			throws ClassGenerationException {

		URLClassLoader urlClassLoader = null;

		try {
			File fcd = new File(dir);

			if (LOG.isDebugEnabled()) {
				LOG
						.debug("ClassesDir.getAbsolutePath="
								+ fcd.getAbsolutePath());
			}

			URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
			urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
					.getClassLoader());

			LOG.debug("url classloader: "
					+ Arrays.asList(urlClassLoader.getURLs()));

			return urlClassLoader.loadClass(className);

		} catch (MalformedURLException e) {
			return null;

		} catch (ClassNotFoundException e) {
			return null;
		}
	}

	/**
	   * isThrowableSubClass 
	   * 
	   * @param cls
	   * @return
	   */
	  public static boolean isThrowableSubClass(Class cls)
	  {
		  if (cls == null || cls.getName() == null)
			  return false;
		  
		  if (cls.getName().equals("java.lang.Throwable"))
		  {
			  return true;
		  }
		  else if (cls.getName().equals("java.lang.Object"))
		  {
			  return false;
		  }
		  else return isThrowableSubClass(cls.getSuperclass());
	  }
	
}
