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
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.AddExceptionSuperclass;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.RemoveEJBInterfaceAdapter;
import it.imolinfo.jbi4ejb.webservice.generator.bcm.RemoteInterfaceExceptionAdapter;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

/**
 * The Class EJBUtils.
 * This class contains the helper method added for the jbi4Ejb project.
 * The helper method in the <code>Utils</code> class are from the jbi4corba project
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class EJBUtils {
        
    /** Logger. */
    static final Logger LOG = LoggerFactory.getLogger(EJBUtils.class);
    private static final Messages MESSAGES
    = Messages.getMessages(EJBUtils.class);
    
    /** The BUFFER_LENGTH (1024). */
    private static final int BUFFER_LENGTH = 1024;
    
    /**
     * Private constructor to avoid instantiation.
     */
    private EJBUtils() {}         
    
    /**
     * Creates a temporary directory.
     * 
     * @return the created directory or <code>null</code> if the creation has
     *         failed even though a file has been created.
     * @throws IOException
     *             if a file could not be created.
     */
    public static File createTempDir() throws IOException {
        File f = File.createTempFile("EJBGENERATION_", null);

        /*
         * java.io.File API can create only temporary files: delete the created
         * file and make a directory with the same name
         */
        if (!f.delete()) {
            return null;
        }
        if (!f.mkdir()) {
            return null;
        }
        return f;
    }

    /**
     * Creates the stub.
     * 
     * @param classesDir
     *            The directory containing the classes
     * @param className
     *            The class name to create the stub
     * @param jarFilesName
     *            The jar lists
     * @throws EJBDeployException
     *             If some problem occurs
     */
    public static void createStub(String classesDir, String className, List<String> jarFilesName)
            throws EJBDeployException {
             
        StringBuffer classpathStr = new StringBuffer();
        for (int i = 0; i < jarFilesName.size(); i++) {
            classpathStr.append(jarFilesName.get(i)).append(File.pathSeparator);
        }
        classpathStr.append(classesDir);
        String classpath = classpathStr.toString();        
    
        LOG.debug("Create stub classpath: " + classpath);
    
        List<String> params = new ArrayList<String>(Arrays.asList(new String[] {
                "-classpath", classpath, "-d", classesDir, "-iiop","-poa", "-verbose",
                className }));
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream printstream = new PrintStream(baos);
    
        sun.rmi.rmic.Main main = new sun.rmi.rmic.Main(printstream, "rmic");
    
        boolean result = main.compile(params.toArray(new String[] {}));
    
        if (!result) {
            LOG.debug(baos.toString());
            throw new EJBDeployException("Errore nell'esecuzione di RMIC");
        }
    
        LOG.debug("<<<<< compileRemoteClasses - end");
    }
    
    /**
     * Modify the interface generated from the WSDL, adding the correct application exceptions.
     * 
     * Retrieve the Applicative Exceptions, wrapped by a <code>org.codehaus.xfire.fault.FaultInfoException</code> in the
     * WSDL-generated interface. Adds the correct exception to the throws clause of the interface, with the <code>java.rmi.RemoteException</code>
     * The Exception classes are subclassed form <code>java.lang.Exception</code>  
     * 
     * @param portTypeClassName
     *              The port type class name
     * @param classesDirName
     *              The classes directory
     * tweakRemoteInterfaceGeneratedFromWSDL
     * @throws ClassGenerationException
     *              If some problem occurs
     */
    public static void tweakRemoteInterfaceGeneratedFromWSDL(String portTypeClassName, String classesDirName) throws ClassGenerationException  {

        LOG.debug("Adding the application exceptions to interface: " + portTypeClassName);
        
        ClassWriter cw = new ClassWriter(true);
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));
                
        // Get the classes classloader (to inspect hte declared exceptions)       
        ClassLoader classLoader = null;
        try {
            classLoader = Util.getURLClassLoader(classesDirName);
        } catch (MalformedURLException ex) {
        	String msg=MESSAGES.getString("EJB001003_tweakRemoteInterfaceGeneratedFromWSDL", new Object[]{ex.getMessage()});
            LOG.error(msg,ex);
            throw new ClassGenerationException(msg,ex);
        }
        
        RemoteInterfaceExceptionAdapter cv = new RemoteInterfaceExceptionAdapter(tv, classLoader);
        
        LOG.debug("new ClassReader - Begin");
        ClassReader cr;
        try {
            cr = new ClassReader(new FileInputStream(getAsFileName(
                    classesDirName, portTypeClassName, ".class")));
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001003_tweakRemoteInterfaceGeneratedFromWSDL", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }

        cr.accept(cv, true);

        LOG.debug("output of tracer during creation of class: " +
                portTypeClassName + "\n" + sw.toString());

        byte[] newBytecode = cw.toByteArray();

        // write class in the right place
        String relativeFileName = portTypeClassName.replace('.',
                File.separatorChar);

        LOG.debug("FileName=" + relativeFileName);

        Util.saveAsJavaClass(classesDirName + File.separator +
                relativeFileName + ".class", newBytecode);
        
        // Add the Exception superclass to all the declared exceptions
        List<String> exceptions = cv.getExceptionsAdded();    
        addExceptionSuperclass(exceptions, classesDirName);
    }
    
    /**
     * Add <code>java.lang.Exception</code> as Superclass.
     * The exception classes generated from the WSDL does extends
     * <code>java.lang.Exception</code>.
     * 
     * 
     * @param exceptions
     *          The exceptions to transform
     * @param classesDirName
     *          Where the classes are
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    private static void addExceptionSuperclass(List<String> exceptions, String classesDirName) 
            throws ClassGenerationException {
        
        for (int i = 0; i < exceptions.size(); i++) {
            String exception = exceptions.get(i);
            LOG.debug("Adding Exception superclass to exception: " + exception);
            
            ClassWriter  cw = new ClassWriter(true); // visitMaxs
            ClassVisitor cc = new CheckClassAdapter(cw);
            StringWriter sw = new StringWriter();
            ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));
            
            AddExceptionSuperclass cv = new AddExceptionSuperclass(tv);
            
            ClassReader cr;
            try {
                cr = new ClassReader(new FileInputStream(getAsFileName(
                        classesDirName, exception, ".class")));
            } catch (IOException e) {
            	String msg=MESSAGES.getString("EJB001004_addExceptionSuperclass", new Object[]{e.getMessage()});
                LOG.error(msg,e);
                throw new ClassGenerationException(msg,e);
            }
            cr.accept(cv, true);
            byte[] newBytecode = cw.toByteArray();

            // Save the class bytecode
            String relativeFileName = exception.replace('.', File.separatorChar);
            Util.saveAsJavaClass(classesDirName + File.separator +
                    relativeFileName + ".class", newBytecode);

        }
    }
    
    /**
     * Removes the implements <code>java.rmi.Remote</code> and throws <code>java.rmi.RemoteException</code> clauses, 
     * using recursino (see EJB-53).
     *   
     * @param interfaceClassName
     *              The port type class name
     * @param classesDirName
     *              The classes directory where to save the file
     * tweakRemoteInterfaceGeneratedFromWSDL
     * @throws ClassGenerationException
     *              If some problem occurs
     */
    public static void removeEJBRemoteInterface(String interfaceClassName, String classesDirName) throws ClassGenerationException  {

        LOG.debug("Removing the remote interface from: " + interfaceClassName + " in directory: " + classesDirName);       
        
        ClassWriter cw = new ClassWriter(true);
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));
                       
        RemoveEJBInterfaceAdapter cv = new RemoveEJBInterfaceAdapter(tv, classesDirName);
                             
        String classFileName = getAsFileName(classesDirName, interfaceClassName, ".class");
        File classFile = new File(classFileName);
        if (!classFile.exists()) {
            // TODDO i18n
            String msg  = "Class " + interfaceClassName + " not found in archive";
            throw new ClassGenerationException(msg);
        }
        
        ClassReader cr;
        try {
            cr = new ClassReader(new FileInputStream(classFile));
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB001005_removeEJBRemoteInterface", new Object[]{e.getMessage()});
            LOG.error(msg,e);
            throw new ClassGenerationException(msg,e);
        }

        cr.accept(cv, true);
        byte[] newBytecode = cw.toByteArray();

        // write class in the right place
        String relativeFileName = interfaceClassName.replace('.',
                File.separatorChar);

        Util.saveAsJavaClass(classesDirName + File.separator +
                relativeFileName + ".class", newBytecode);              
    }    
    
    

    
    /**
     * Gets the as class as a file name.
     * 
     * @param basedir
     *          The classes base directory
     * @param javaName
     *          The java name
     * @param ext
     *          The extension
     *      
     * @return the as file name
     */
    private static String getAsFileName(String basedir, String javaName,
            String ext) {
        char sep = File.separator.charAt(0);
        String basedirRep = basedir.replace('\\', sep).replace('/', sep);

        return basedirRep + sep + javaName.replace('.', sep) + ext;
    }
    
    /**
     * Copy directory.
     * 
     * @param sourceLocation
     *          The source directory
     * @param targetLocation
     *          The target directory
     * 
     * @throws IOException
     *          If some problem occurs in copying directory
     */
    public static void copyDirectory(File sourceLocation , File targetLocation)
    throws IOException {        
        
        if (sourceLocation.isDirectory()) {
            if (!targetLocation.exists()) {
                targetLocation.mkdir();
            }

            String[] children = sourceLocation.list();
            for (int i=0; i<children.length; i++) {
                copyDirectory(new File(sourceLocation, children[i]),
                        new File(targetLocation, children[i]));
            }
        } else {

            InputStream in = new FileInputStream(sourceLocation);
            OutputStream out = new FileOutputStream(targetLocation);

            // Copy the bits from instream to outstream
            byte[] buf = new byte[BUFFER_LENGTH];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            in.close();
            out.close();
        }
    }
    
    

}
