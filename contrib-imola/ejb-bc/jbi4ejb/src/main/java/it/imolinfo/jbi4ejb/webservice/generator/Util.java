/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.exception.EJBDeployException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.RemoteEnancherAdapter;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.SerializableDecorationAdapter;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.SerializableInspectorAdapter;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * Cut&paset from <code>it.imolinfo.jbi4corba.webservice.generator.Util</code> methods.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class Util {

    /** The Logger. */
    private static final Logger LOG = LoggerFactory.getLogger(Util.class);
    private static final Messages MESSAGES = Messages.getMessages(Util.class);

    /** The file: PROTOCOL. */
    private static final String PROTOCOL;

    // Static initializer
    static {
        if (System.getProperty("os.name").indexOf("Win") >= 0) {
            PROTOCOL = "file:///";
        } else {
            PROTOCOL = "file://";
        }
    }
        
    /**
     * Instantiates a new Helper.
     */
    private Util() {}

 
    /**
     * Compile the java sources.
     * Cut&paste from 
     * <code>from it.imolinfo.jbi4corba.webservice.generator.Util</code>.
     * 
     * @param workdirsrc
     *          Where the sources are
     * @param workdirclasses 
     *          Where the sources are compiled
     * @param javaSources
     *          The java sources
     * @param jarFiles
     *          The jar files to add in the classpath
     * @param extraClassPath
     *          The extra classpath
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    public static void compileJavaClasses(String workdirsrc,
            String workdirclasses, List<String> javaSources,
            List<String> jarFiles, List<String> extraClassPath)
            throws ClassGenerationException {
        
        LOG.debug(">>>>> compileJavaClasses - begin");

        LOG.debug("compileJavaClasses" + ".\n workdirsrc=" + workdirsrc +
                ";\n workdirclasses=" + workdirclasses + ";\n javaSources=" +
                javaSources + ";\n jarFiles=" + jarFiles +
                ";\n extraClassPath=" + extraClassPath);

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
                    
                    String extraParent = f.getParent();
                    LOG.debug("-----> EXTRA=[" + extraParent + "]");
                    classpath += extraParent + File.pathSeparator; // ":" or ";"
                } else {
                    classpath += extra + File.pathSeparator; // ":" or ";"
                }

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

        System.out
                .println("command line: " + Arrays.toString(params.toArray()));

        File classesdir = new File(workdirclasses);
        if (!classesdir.exists()) {
            boolean result = classesdir.mkdirs();

            if (!result) {
            	String msg=MESSAGES.getString("EJB001006_Failure_in_creating_classes_dir");
                LOG.error(msg);
                throw new ClassGenerationException(msg);
            } else {
                LOG.debug("Classes Dir Created:" + classesdir);
            }
        } else {
            LOG.debug("Classes Dir Already Exists:" + classesdir);
        }

        StringWriter stringWriter = new StringWriter();
        PrintWriter printstream = new PrintWriter(stringWriter);

        LOG.debug("Compiling - begin");

        int result = com.sun.tools.javac.Main.compile(params
                .toArray(new String[] {}), printstream);

        LOG.debug("Compiling - end. result=" + result);

        // com.sun.tools.javac.Main.main(params.toArray(new String[] {}));

        LOG.debug("compilation output: \n" + stringWriter.toString());

        if (!(result == 0)) {
        	String msg=MESSAGES.getString("EJB001007_Classes_compilation_failed");
            LOG.error(msg);
            throw new ClassGenerationException(msg);
        } else {
            LOG.debug("Compilation OK.");
        }

        LOG.debug("<<<<< compileJavaClasses - end");
    }
   

    /**
     * Find java sources in a directory.
     * 
     * @param basedir
     *          The base directory
     * @param exclude
     *          The files to exclude
     * @return the list< string>
     * 
     * @throws EJBDeployException
     *          If some problem occurs
     */
    public static List<String> findJavaSources(String basedir,
            List<String> exclude) throws EJBDeployException {

        List<String> javaSourcesNames = new ArrayList<String>();
        List<File> sourceFiles = findFilesFromSourceDirectory(basedir, ".java");

        for (File source : sourceFiles) {

            String src = null;
            try {

                src = source.getCanonicalPath();

            } catch (IOException e) {
            	String msg=MESSAGES.getString("EJB001008_findJavaSources", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new EJBDeployException(msg,e);
            }

            /*
             * Some files is not compiled.
             * 
             * This feature is useful when we add to the classpath some class
             * compiled before this task and bytecode manipulated.
             * 
             */
            if (src.endsWith("src")) {

                LOG.debug("The file " + src + " won't be compiled.");

            } else if (containsIgnoreSlashes(exclude, src)) {

                LOG.debug("The file " + src + " won't be compiled.");

            } else {
                LOG.debug("The file " + src + " will be compiled.");
                javaSourcesNames.add(src);
            }
        }

        LOG.debug("<<<<< findJavaSources(String, List<String>) - end");
        return javaSourcesNames;
    }

    /**
     * Contains ignore slashes.
     * 
     * @param list
     *          The list to test
     * @param myString
     *          The string to search
     * @return true, if successful
     */
    public static boolean containsIgnoreSlashes(List<String> list, String myString) {
        if (myString == null) {
            return false;
        }
        if (list == null) {
            return false;
        }

        String xrr = myString.replace('\\', ' ').replace('/', ' ');

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
     * This method find all the files in a directory according to the filter.
     * 
     * @param basedirString
     *            The directory where the method works.
     * @param extensionFilter
     *            The filter (the suffix of the files).
     * 
     * @return the found files
     */
    public static List<File> findFilesFromSourceDirectory(String basedirString,
            final String extensionFilter) {

        File basedir = new File(basedirString);

        FileFilter filter = new FileFilter() {
            public boolean accept(File file) {

                boolean filterAccept = file.getName().endsWith(extensionFilter);

                return filterAccept;
            }
        };

        List<File> directories = findDirectories(basedir);        
        
        List<File> filterdFiles = new ArrayList<File>();
        for (File dir : directories) {            
            File[] innerFilteredFiles = dir.listFiles(filter);           
            filterdFiles.addAll(Arrays.asList(innerFilteredFiles));
        }

        return filterdFiles;
    }
    
    /**
     * This method find all the files in a directory according to the filter.
     * Its similar to <code>findFilesFromSourceDirectory</code> method, except
     * that looks fpr the EXACT file name and adds the base directory in the
     * search.
     * 
     * @param basedirString
     *            The directory where the method works.
     * @param exactName
     *            The exact file name 
     * @return the found files
     */
    public static List<File> findFilesFromSourceDirectoryFromExactFileName(String basedirString,
            final String exactName) {

        File basedir = new File(basedirString);

        FileFilter filter = new FileFilter() {
            public boolean accept(File file) {                
                
                boolean filterAccept = file.getName().equals(exactName);

                return filterAccept;
            }
        };

        List<File> directories = findDirectories(basedir);        
        directories.add(basedir);
        List<File> filterdFiles = new ArrayList<File>();
        for (File dir : directories) {            
            File[] innerFilteredFiles = dir.listFiles(filter);           
            filterdFiles.addAll(Arrays.asList(innerFilteredFiles));
        }

        return filterdFiles;
    }    

    /**
     * Find directories.
     * 
     * @param basedir
     *          The directory where to search
     * 
     * @return the found directories
     */
    private static List<File> findDirectories(File basedir) {
        List<File> directories = new ArrayList<File>();

        if (basedir == null || "".equals(basedir.getAbsolutePath())) {
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
     * Tweak interface classes addint the <code>java.rmi.Remote</code> interface.
     * 
     * @param portTypeClassName
     *          The interface class name
     * @param classesDirName
     *          The classes directory
     * @return the remote class name
     * 
     * @throws ClassGenerationException
     *              If some problem occurs
     */
    public static String tweakInterfaceClasses(String portTypeClassName,
            String classesDirName) throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> tweakInterfaceClasses - begin");

        LOG.debug("remotizing class: " + portTypeClassName + " in dir: " +
                classesDirName);

        ClassWriter cw = new ClassWriter(true);
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        RemoteEnancherAdapter cv = new RemoteEnancherAdapter(tv,
                getAsFullyQualifiedNameInternalForm(portTypeClassName));

        LOG.debug("new ClassReader - Begin");
        ClassReader cr;
        try {
            cr = new ClassReader(new FileInputStream(getAsFileName(
                    classesDirName, portTypeClassName, ".class")));
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001009_tweakInterfaceClasses", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }

        cr.accept(cv, true);

        LOG.debug("output of tracer during creation of class: " +
                portTypeClassName + "\n" + sw.toString());

        byte[] newBytecode = cw.toByteArray();

        // write class in the right place
        String relativeFileName = cv.getCompleteName().replace('/',
                File.separatorChar);

        LOG.debug("relativeFileName=" + relativeFileName +
                "; cv.getCompleteName()=" + cv.getCompleteName());

        Util.saveAsJavaClass(classesDirName + File.separator +
                relativeFileName + ".class", newBytecode);

        String remoteClassName = cv.getCompleteName().replace('/', '.');

        LOG.debug("<<<<<<<<<< tweakInterfaceClasses - end:" + remoteClassName);
        return remoteClassName;
    }

    /**
     * Gets the java class as file name.
     * 
     * @param basedir
     *          The base directory
     * @param javaName
     *          The java class name
     * @param ext
     *          The files extension
     * 
     * @return the as file name
     */
    private static String getAsFileName(String basedir, String javaName,
            String ext) {
        // FIXME null
        char sep = File.separator.charAt(0);
        String basedirTmp = basedir.replace('\\', sep).replace('/', sep);

        return basedirTmp + sep + javaName.replace('.', sep) + ext;
    }

    /**
     * Gets the as fully qualified name internal form.
     * 
     * @param javaName
     *              The java class name
     * 
     * @return the as fully qualified name internal form
     */
    private static String getAsFullyQualifiedNameInternalForm(String javaName) {
        // FIXME null        
        return javaName.replace('.', '/');
    }

    /**
     * Save the bytecode as java class.
     * 
     * @param absoluteFileName 
     *          The file name     
     * @param newBytecode
     *          The bytecode to save
     * 
     * @throws ClassGenerationException
     *              If some problem occurs
     */
    public static void saveAsJavaClass(String absoluteFileName,
            byte[] newBytecode) throws ClassGenerationException {

        LOG.debug(">>>>> saveAs - begin:" + absoluteFileName);

        try {
            FileOutputStream fos = new FileOutputStream(absoluteFileName);

            fos.write(newBytecode);
            fos.close();
        } catch (FileNotFoundException e) {
        	String msg=MESSAGES.getString("EJB001010_saveAsJavaClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001010_saveAsJavaClass", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }

        LOG.debug("<<<<< saveAs - end");
    }

    /**
     * Gets the URL class loader, setting the parent ClassLoader.
     * 
     * @param absolutePath
     *            The url classloader path
     * @param parent
     *            The parent classloader
     * @return the URL class loader The classloader parent
     * 
     * @throws MalformedURLException
     *             If the URL is not well formed
     */
    public static URLClassLoader getURLClassLoader(String absolutePath, ClassLoader parent)
            throws MalformedURLException {
        URL u = new URL(PROTOCOL + absolutePath + "/");
        URLClassLoader urlClassLoader = new URLClassLoader(new URL[] { u },
                parent);
        LOG
                .debug("url classloader: " +
                        Arrays.asList(urlClassLoader.getURLs()));
        return urlClassLoader;
    }
    
    /**
     * Gets the URL class loader, setting the parent ClassLoader to this class ClassLoader.
     * 
     * @param absolutePath
     *          The urel classloader path
     * @return the URL class loader
     * 
     * @throws MalformedURLException
     *              If the URL is not well formed
     */
    public static URLClassLoader getURLClassLoader(String absolutePath)
            throws MalformedURLException {       
        return getURLClassLoader(absolutePath, Util.class.getClassLoader());
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
     *          If some problem occurs
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

        LOG.debug("output of tracer during creation of class: " + absPath +
                "\n" + sw.toString());

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
     * @return The class visitor where the information are stored.
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    public static ClassVisitor tweakSerializableInspection(String absPath)
            throws ClassGenerationException {

        ClassWriter cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        SerializableInspectorAdapter cv = new SerializableInspectorAdapter(tv);

        ClassReader cr = Util.getAsmCLassReader(absPath);

        cr.accept(cv, true);
        LOG.debug("ClassReader.accept ... done");

        return cv;
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
     *          If some problem occurs
     */
    public static ClassReader getAsmCLassReader(String className)
            throws ClassGenerationException {

        LOG.debug(">>>>> getAsmCLassReader - begin");
        ClassReader cr = null;
        try {

            cr = new ClassReader(new FileInputStream(className));

        } catch (IOException e) {
            String msg=MESSAGES.getString("EJB001011_Could_not_instantiate_class_reader_for_class", new Object[]{className});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }

        LOG.debug("<<<<< getAsmCLassReader - end. ClassReader=" + cr);
        return cr;
    }

    /**
     * This method extracs all the classes used as method's parameter or method's result from a class list.
     * 
     * @param dir
     *          The directory where to search the classes
     * @param classList
     *          The classes file list
     * @return
     *          The classes <code>Set</code>
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    @SuppressWarnings("unchecked")
    public static Set<Class> findClassUsed(String dir, List<File> classList)
            throws ClassGenerationException {
        LOG.debug(">>>>> findClassUsedInTheOperations - begin");

        LOG.debug("operationsClass=" + classList);

        if (classList == null || classList.size() == 0) {
        	LOG.info("EJB001012_Operations_class_not_found");
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

        LOG.debug("<<<<< findClassUsedInTheOperations - end:" + result);
        return result;
    }

    /**
     * Load a class using the classes found in the directory.
     * 
     * @param dir
     *          The classes directory
     * @param classAsFile
     *          The class file
     * 
     * @return the class
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     *              
     */
    @SuppressWarnings("unchecked")
    private static Class classLoad(String dir, File classAsFile)
            throws ClassGenerationException {
        URLClassLoader urlClassLoader = null;

        try {
            File fcd = new File(dir);

            if (LOG.isDebugEnabled()) {
                LOG
                        .debug("ClassesDir.getAbsolutePath=" +
                                fcd.getAbsolutePath());
            }

            URL u = new URL(PROTOCOL + fcd.getAbsolutePath() + "/");
            urlClassLoader = new URLClassLoader(new URL[] { u }, Util.class
                    .getClassLoader());

            LOG.debug("url classloader: " +
                    Arrays.asList(urlClassLoader.getURLs()));

            String className = getClassName(classAsFile, dir);
            LOG.debug("class name: " + className);
            return urlClassLoader.loadClass(className);

        } catch (MalformedURLException e) {
//            Object[] args = new Object[] { PROTOCOL +
//                    new File(dir).getAbsolutePath() + "/" };
        	String msg=MESSAGES.getString("EJB001013_classLoad", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        } catch (ClassNotFoundException e) {
//            Object[] args = new Object[] { PROTOCOL +
//                    new File(dir).getAbsolutePath() + "/" };

        	String msg=MESSAGES.getString("EJB001013_classLoad", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }
    }

    /**
     * Gets the class name.
     * 
     * @param file
     *          The file
     * @param basedir
     *          The base directory
     * 
     * @return the class name
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    private static String getClassName(File file, String basedir)
            throws ClassGenerationException {
        LOG.debug(">>>>> getClassName - begin");

        String absoluteFileName = file.getAbsolutePath();
        String absoulteBaseDir = new File(basedir).getAbsolutePath();

        LOG.debug("absoluteFileName: " + absoluteFileName +
                "; absoulteBaseDir: " + absoulteBaseDir);

        if (!absoluteFileName.startsWith(absoulteBaseDir)) {
            //Object[] args = new Object[] { file.getAbsolutePath(),
            //        absoulteBaseDir };

        	String msg=MESSAGES.getString("EJB001014_getClassName", new Object[]{absoluteFileName}, new Object[]{absoulteBaseDir});
            LOG.error(msg);
            throw new ClassGenerationException(msg);
        }

        // +1 excludes the trailing slash
        String relativeFileName = absoluteFileName.substring(absoulteBaseDir
                .length() + 1);
        LOG.debug("relativeFileName.class=" + relativeFileName);

        // eliminates .class
        if (relativeFileName.endsWith(".class")) {
            relativeFileName = relativeFileName.substring(0, relativeFileName
                    .length() - ".class".length());
            LOG.debug("relativeFileName=" + relativeFileName);
        }

        String className = relativeFileName.replace(File.separator, ".");
        LOG.debug("className=" + className);

        LOG.debug("<<<<< getClassName - end");
        return className;
    }
    
    /**
     * The list of names of the jars used in the classpath.
     *
     * @param    libDirName    The directory where the jars are located.
     *
     * @return    The list of names of the jars used in the classpath.
     *
     * @throws    ClassGenerationException
     *              If some problem occurs
     */
    public static List<String> prepareClassPath(String libDirName)
        throws ClassGenerationException {

        LOG.debug(">>>>> prepareClassPath - begin");
        List<File> jarFiles
            = Util.findFilesFromSourceDirectory(libDirName, ".jar");

        List<String> jarFilesName = new ArrayList<String>();

        for (File jarFile:jarFiles) {
            try {
                LOG.debug("Adding jar " + jarFile.getCanonicalPath() + " ... ");

                jarFilesName.add(jarFile.getCanonicalPath());

                LOG.debug("... jar " + jarFile.getCanonicalPath() + " added.");
            } catch (IOException e) {
                // Object[] args = new Object[] { jarFile, e.getMessage() };
                
            	String msg=MESSAGES.getString("EJB001015_prepareClassPath", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new ClassGenerationException(msg,e);
            }
        }

        LOG.debug("<<<<< prepareClassPath - end");
        return jarFilesName;
    }    
}
