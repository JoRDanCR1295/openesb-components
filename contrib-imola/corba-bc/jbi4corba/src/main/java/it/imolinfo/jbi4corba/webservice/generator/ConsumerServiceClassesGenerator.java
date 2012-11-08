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
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.utils.HelperFileUtil;
import it.imolinfo.jbi4corba.webservice.generator.bcm.AddExceptionSuperclass;
import it.imolinfo.jbi4corba.webservice.generator.bcm.RemoteEnhancerAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.UIDAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.ValueTypeAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.WsdlToCorbaAdapter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.VFS;
import org.apache.cxf.service.model.OperationInfo;
import org.apache.cxf.service.model.ServiceInfo;
import org.apache.cxf.tools.common.ToolContext;
import org.apache.cxf.tools.wsdlto.WSDLToJava;
import org.apache.cxf.wsdl11.WSDLServiceBuilder;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;


/**
 * This class generates (at deploy time) the code used by the jbi4corba
 * in 'consumer' mode (at run time).
 *
 */
public class ConsumerServiceClassesGenerator {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(ConsumerServiceClassesGenerator.class);
  private static final Messages MESSAGES = 
  	Messages.getMessages(ConsumerServiceClassesGenerator.class);


  /**
   * Default constructor.
   */
  public ConsumerServiceClassesGenerator() {
    // NOP
  }

  /**
   * This method is used to generate all the consumer code starting from the
   * WSDL of the Endpoint to consume.
   *
   * Steps:
   * 1) code generation: wsdl to java
   * 2) compile the new source
   * 3) bytecode manipultion: the classes must be modified to use rmic
   * 4) compile with rmic to generate the corba classes
   * 5) add the getter, setter and serialVersionUID to the class
   * 6) save all the objects in the ServerCorbaClassesHolder
   *
   *
   * @param wsdlStringUrl   Where the WSDL is located.
   * @param workdir         Where the method can generate the code.
   * @param jars            The jar needed at compile time and run time.
   *
   * @param All the references to the object generated.
   *
   * @throws  ClassGenerationException    When the code cannot be generated.
   *
   */
  public ServerCorbaClassesHolder generateConsumerServiceClasses(
          String wsdlStringUrl,
          String workdir,
          String libDirName,
          JbiServiceDescriptor jbiServiceDescriptor) throws ClassGenerationException {

      LOG.debug(">>>>> generateConsumerServiceClasses - begin");

      // ===================================
      //       find jar list used to compile
      // ===================================
      List<String> jars = Util.prepareClassPath(libDirName);

      //ServerCorbaClassesHolder s
      //    = generateConsumerServiceClasses(wsdlStringUrl, workdir, jars);

      ServerCorbaClassesHolder s
        = generateConsumerServiceClassesDirect(wsdlStringUrl, workdir, jars, jbiServiceDescriptor);

      LOG.debug("<<<<< generateConsumerServiceClasses - end");
      return s;
  }

//    /**
//     * This method implements the follow algorithm:
//     *
//     *  1) retrieving the WSDL.
//     *  2) creating a (unique) directory (called wsdlDir).
//     *  3) copy the WSDL in the wsdlDir.
//     *  4) copy the ir.idl and orb.idl in the wsdlDir.
//     *  5) creating the wsdlDir/src and wsdlDir/classes directories.
//     *  6) creating the list of the jars distribuited with the component.
//     *  7) execute a 'wsdl to java' operation using CXF.
//     *  8) compiling the classes generated.
//     *  9) retrieving the 'interface class' (The one with the suffix ).
//     * 10) adding RMIRemote to the PortType class
//     * 11) compiling all the source code using RMIC to generate the IDLs
//     * 12) moving the IDLs in the wsdlDir
//     * 13) creating the servant classes (and recompiling)
//     * 14) retrieving the classes needed at 'run time' using the class loader.
//     *  ...
//     *
//     * @param    wsdlStringUrl    Where the WSDL is located
//     * @param    workdir          The working directory
//     * @param    jars             The list of the libraries
//     *
//     * @return    The classes generated
//     *
//     * @throws    ClassGenerationException  The class generation exception
//     */
//    public ServerCorbaClassesHolder generateConsumerServiceClasses(
//        String wsdlStringUrl,
//        String workdir,
//        List<String> jars) throws ClassGenerationException {
//
//        LOG.debug(">>>>> generateConsumerServiceClasses - begin");
//
//        LOG.debug("generateConsumerServiceClasses"
//                + ". wsdlStringUrl=" + wsdlStringUrl
//                + "; workdir="       + workdir
//                + "; jars="          + jars);
//
//        // ========================================
//        // copying wsdl from url to local directory
//        // ========================================
//
//        File wsdlDirFile = copyWsdlTo(workdir, wsdlStringUrl);
//
//        String wsdlDirName = null;
//        try {
//            wsdlDirName = wsdlDirFile.getCanonicalPath();
//        } catch (IOException ioe) {
//            LOG.error("CRB000500_Error_accessing_the_WSDL_directory", ioe);
//            throw new ClassGenerationException(
//                    "CRB000500_Error_accessing_the_WSDL_directory", ioe);
//        }
//
//        // ==========================
//        //     add orb.idl and ir.idl
//        // ==========================
//        copyOrbIrIdl(wsdlDirName);
//
//        // ==================================================
//        //                preparing directory (src + classes)
//        // ==================================================
//        String sourceDirName = mkdir(wsdlDirFile, "src");
//        final String classesDirName = mkdir(wsdlDirFile, "classes");
//
//        // =======================================
//        // generating source files  (WSDL to JAVA)
//        // =======================================
//        WsdlInformation wsdlInformation
//          = wsdlToJava(wsdlDirName, sourceDirName);
//
//        List<QName> portTypeList = wsdlInformation.getPortTypeList();
//
//        // ======================
//        // compiling java classes
//        // ======================
//        List<String> sources = javac(sourceDirName, classesDirName, jars);
//
//        // ===============================
//        // finding <Service>PortType.class
//        //================================
//        //String portTypeClassName = findingPortTypeClassName(classesDirName);
//        String portTypeClassName
//                = findingPortTypeClassName(portTypeList, classesDirName);
//        
//        // ========================================
//        // ByteCode Manipulation:
//        // add the correct exceptions to the throws clause
//        // of the remote interface (the generated interface
//        // throws a FaultInfoException).
//        // Decomment when working on issue CRB-118 
//        //=========================================        
//        // tweakRemoteInterfaceGeneratedFromWSDL(portTypeClassName,
//        //                                       classesDirName); 
//
//        // ========================================
//        // ByteCode Manipulation: add Remote
//        //=========================================
//        String remoteClassName = tweakInterfaceClasses(portTypeClassName,
//                                                       classesDirName);
//
//        // ========================================
//        // ByteCode Manipulation: valuetype
//        // ========================================
//        List<String> modifiedClassNames
//          = tweakValueType(remoteClassName, classesDirName);
//
//        Map<String, AnnotationsMaps> annota = tweakAnnotationCollector(
//          modifiedClassNames, classesDirName);
//
//        for (String currentClass : annota.keySet()) {
//            AnnotationsMaps tracer = annota.get(currentClass);
//
//            LOG.debug("TRACER. currentClass=" + currentClass
//              + "; tracer=" + tracer);
//        }
//
//        // compile tweaked class with rmic
//        Util.compileRemoteClassesNoValueMethodsON(classesDirName,
//                                                  remoteClassName,
//                                                  jars);
//        String idlFilename
//            = classesDirName
//            + File.separator
//            + remoteClassName.replace('.', File.separatorChar)
//            + ".idl";
//
//        String destIdlFileName
//            = wsdlDirName
//            + File.separator
//            + remoteClassName.substring(remoteClassName.lastIndexOf('.') + 1)
//            + ".idl";
//
//        // =========================
//        //        move all idl files
//        // =========================
//        copyIDLs(classesDirName, wsdlDirName, remoteClassName);
//
//        // =================================
//        //       compile new idl with poaTie
//        // =================================
//        idl2javaTieModel(sourceDirName, idlFilename, wsdlDirName, destIdlFileName);
//
//        // =================================
//        //      VALUETYPE IDL IMPLEMENTATION
//        // =================================
//        List<String> vtList = Util.valueTypesImpl(sourceDirName, true);
//        LOG.debug("end creating java sources implementation for value types.");
//
//        // =================================
//        // Code Generation: new factories
//        // XxxDefaultFactoryJbi4corba
//        // =================================
//        Util.generateValueTypeFactoryJbi4corba(vtList, sourceDirName);
//
//        // =========================
//        // re-compiling java classes
//        // =========================
//        LOG.debug("re-compiling java classes - begin");
//        try {
//            int last = remoteClassName.lastIndexOf(".");
//            String objectFactoryPackage = remoteClassName.substring(0, last);
//
//            List<String> no = new ArrayList<String>();
//            no.add(objectFactoryPackage + ".ObjectFactory");
//
//            // the list of java file that will be not compile again
//            List<String> excludes
//                = exctractJavaListJavaName(no, sourceDirName);
//
//            sources = Util.findJavaSources(sourceDirName, excludes);
//
//        } catch (Jbi4CorbaException e) {
//            Object[] args = new Object[] { sourceDirName, e.getMessage() };
//
//            LOG.error("CRB000501_Error_generating_sources_list", args, e);
//            throw new ClassGenerationException(
//                    "CRB000501_Error_generating_sources_list", args, e);
//        }
//
//        LOG.debug("re-compiling java classes - pre Util.compileJavaClasses");
//
//        List<String> extra = new ArrayList<String>();
//        extra.add(classesDirName);
//
//        Util.compileJavaClasses(sourceDirName,
//                                classesDirName,
//                                sources,
//                                jars,
//                                extra);
//
//        LOG.debug("re-compiling java classes - end");
//
//        // =================================
//        //  ADAPTING AUTOGENERATED VALUETYPE
//        // =================================
//        tweakWsdlToCorba(vtList, classesDirName);
//        LOG.debug("end creating java sources implementation for value types.");
//
//        tweakAnnotationWriter(classesDirName, annota);
//
//        // =======================
//        //instantiate class loader
//        // =======================
//        URLClassLoader urlClassLoader = getURLClassLoader(classesDirName);
//
//        LOG.debug("instantiate class loader ... done");
//
//        //now we need the following classes
//        //<remoteClass>CorbaInterfaceOperations
//        //<remoteClass>CorbaInterfacePOATie
//        //<remoteClass>ServicePortType
//        //<remoteClass>CorbaInterfaceHelper
//
//        String classRoot = remoteClassName.substring(
//                0, remoteClassName.indexOf("CorbaInterface"));
//
//        LOG.debug("FROM remoteClassName=" + remoteClassName
//                + "; TO classRoot=" + classRoot);
//
//        String operations = classRoot + "CorbaInterfaceOperations";
//        String poaTie     = classRoot + "CorbaInterfacePOATie";
//        //String portType   = classRoot + "ServicePortType";
//        String portType   = portTypeClassName;
//        String helper     = classRoot + "CorbaInterfaceHelper";
//
//        // Takes the service of the first port type
//        String serviceName
//          = extractServiceNameFromPortType(wsdlDirName, portTypeList.get(0));
//        String portTypePackage
//          = portTypeClassName.substring(0, portTypeClassName.lastIndexOf('.'));
//        String serviceImpl = portTypePackage + "." + serviceName + "Impl";
//
//        LOG.debug("classRoot="     + classRoot
//                + "; operations="  + operations
//                + "; poaTie="      + poaTie
//                + "; portType="    + portType
//                + "; helper="      + helper
//                + "; serviceImpl=" + serviceImpl);
//
//        ServerCorbaClassesHolder serverCorbaClassesHolder
//            = new ServerCorbaClassesHolder();
//
//        setCorbaOperations(serverCorbaClassesHolder,
//                           urlClassLoader,
//                           operations);
//
//        setCorbaPOATie(serverCorbaClassesHolder, urlClassLoader, poaTie);
//
//        setWebServiceInterface(serverCorbaClassesHolder,
//                               urlClassLoader,
//                               portType);
//        setCorbaHelper(serverCorbaClassesHolder, urlClassLoader, helper);
//
//        serverCorbaClassesHolder.setUrlClassLoader(urlClassLoader);
//
//        setWebServiceImpl(serverCorbaClassesHolder,
//                          urlClassLoader,
//                          serviceImpl);
//
//        Map<String, Object> map = Util.valueTypeMapHandlerWithJbi4corbaFactory(
//          vtList, classesDirName, urlClassLoader);
//
//        serverCorbaClassesHolder.setValueTypeIdAndInstance(map);
//        serverCorbaClassesHolder.setWsdlInformation(wsdlInformation);
//
//        LOG.debug("<<<<< generateConsumerServiceClasses - end");
//        return serverCorbaClassesHolder;
//    }

  /**
   * This Method copy all the xsd in the work dir 
   * @param  srcDir the XSD source directory URL
   * @param  wsdlDirFile Destination folder
   * @throws ClassGenerationException 
   **/
  private void copyXSDFile(String srcDir,File wsdlDirFile) throws ClassGenerationException{
       
            List<File> xdsList = Util.findFilesFromSourceDirectory(srcDir, ".xsd");
         
            for (File xsd : xdsList) {

                LOG.debug("Copy XSD To " + wsdlDirFile.getPath() + "--->" + xsd.getName());
                File out = new File(wsdlDirFile + File.separator + xsd.getName());
                try {
                    HelperFileUtil.copyFile(xsd, out);
                } catch (IOException ioe) {
                	String msg=MESSAGES.getString("CRB000500_Error_accessing_the_WSDL_directory");
                  LOG.error(msg, ioe);
                  throw new ClassGenerationException(msg, ioe);
                }
            }
       
  }

  /**
   * This method is used to generate all the consumer code starting from the
   * WSDL of the Endpoint to consume.
   * The consumer code can be generated starting from IDL in case it is included inside WSDL (idl element) 
   *
   * Steps:
   * 1) copy wsdl from url to local directory
   * 2) check if IDL content is found inside WSDL
   * 3) call the proper method to generate consumer classes
   *
   *
   * @param wsdlStringUrl   Where the WSDL is located.
   * @param workdir         Where the method can generate the code.
   * @param jars            The jar needed at compile time and run time.
   *
   * @param All the references to the object generated.
   *
   * @throws  ClassGenerationException    When the code cannot be generated.
   *
   */
  	public ServerCorbaClassesHolder generateConsumerServiceClassesDirect(
          String wsdlStringUrl,
          String workdir,
          List<String> jars,
          JbiServiceDescriptor jbiServiceDescriptor) throws ClassGenerationException {

      // ========================================
      // copying wsdl from url to local directory
      // ========================================

      File wsdlDirFile = copyWsdlTo(workdir, wsdlStringUrl);
      //Copy all XSD to work directory
      //File f=new File(wsdlStringUrl);
      File f=new File(workdir);
      String srcDir=f.getParent();
      //30-06-09 Temporary Fix
      //ServiceMix fix
      //Deploy problem after ESB restart phase 
      copyXSDFile(srcDir,f);
      copyXSDFile(srcDir,wsdlDirFile);
      
      String wsdlDirName = null;
      try {
          wsdlDirName = wsdlDirFile.getCanonicalPath();
      } catch (IOException ioe) {
          ioe.printStackTrace();
          String msg = MESSAGES.getString("CRB000500_Error_accessing_the_WSDL_directory");
          LOG.error(msg, ioe);
          throw new ClassGenerationException(msg, ioe);
      }

      // calling the proper method to generate consumer classes 
      if (isIdlIncluded(jbiServiceDescriptor)) {
			WsdlInformation wsdlInformation = extractWsdlInformation(getWsdlFileName(wsdlDirName));
			return generateConsumerServiceClassesDirectFromIDL(wsdlDirFile,
					jars, jbiServiceDescriptor, wsdlInformation);
		} else {
			return generateConsumerServiceClassesDirectFromWSDL(wsdlDirFile,
					jars);
		}
  }
  	
    /**
     * This method is used to generate all the consumer code starting from the
     * WSDL of the Endpoint to consume.
     *
     * Steps:
     * 1) code generation: wsdl to java
     * 2) compile the new source
     * 3) bytecode manipultion: the classes must be modified to use rmic
     * 4) compile with rmic to generate the corba classes
     * 5) add the getter, setter and serialVersionUID to the class
     * 6) save all the objects in the ServerCorbaClassesHolder
     *
     *
     * @param wsdlDirFile     Where the WSDL is located.
     * @param workdir         Where the method can generate the code.
     * @param jars            The jar needed at compile time and run time.
     *
     * @param All the references to the object generated.
     *
     * @throws  ClassGenerationException    When the code cannot be generated.
     *
     */
    public ServerCorbaClassesHolder generateConsumerServiceClassesDirectFromWSDL(
        File wsdlDirFile,
        List<String> jars) throws ClassGenerationException {

        LOG.debug(">>>>> generateConsumerServiceClassesDirectWSDL - begin");

        String wsdlDirName = null;
        try {
            wsdlDirName = wsdlDirFile.getCanonicalPath();
        } catch (IOException ioe) {
        	ioe.printStackTrace();
        	String msg=MESSAGES.getString("CRB000500_Error_accessing_the_WSDL_directory");
            LOG.error(msg, ioe);
            throw new ClassGenerationException(msg, ioe);
        }
        
        // ==========================
        //     add orb.idl and ir.idl
        // ==========================
        copyOrbIrIdl(wsdlDirName);

        // ==================================================
        //                preparing directory (src + classes)
        // ==================================================
        String sourceDirName = mkdir(wsdlDirFile, "src");
        final String classesDirName = mkdir(wsdlDirFile, "classes");

        // =======================================
        // generating source files  (WSDL to JAVA)
        // =======================================
        WsdlInformation wsdlInformation
          = wsdlToJava(wsdlDirName, sourceDirName, jars);

        List<QName> portTypeList = wsdlInformation.getPortTypeList();

        // ======================
        // compiling java classes
        // ======================
        javac(sourceDirName, classesDirName, jars);

        // ===============================
        // finding <Service>PortType.class
        // ===============================
        //String portTypeClassName = findingPortTypeClassName(classesDirName);
        String portTypeClassName
                = findingPortTypeClassName(portTypeList, classesDirName);
        
        // ========================================
        // ByteCode Manipulation:
        // add the correct exceptions to the throws clause
        // of the remote interface (the generated interface
        // throws a FaultInfoException).
        // Decomment when working on issue CRB-118 
        //=========================================        
        // tweakRemoteInterfaceGeneratedFromWSDL(portTypeClassName,
        //                                       classesDirName); 

        // ========================================
        // ByteCode Manipulation: add Remote
        //=========================================
        String remoteClassName = tweakInterfaceClasses(portTypeClassName,
                                                       classesDirName);

        // ========================================
        // ByteCode Manipulation: valuetype
        // ========================================
        List<String> modifiedClassNames
          = tweakValueType(remoteClassName, classesDirName);

//        Map<String, AnnotationsMaps> annota = tweakAnnotationCollector(
//          modifiedClassNames, classesDirName);
//
//        for (String currentClass : annota.keySet()) {
//            AnnotationsMaps tracer = annota.get(currentClass);
//
//            LOG.debug("TRACER. currentClass=" + currentClass
//              + "; tracer=" + tracer);
//        }

        //Map<String, String> smap = tweakSerialVersionUID(modifiedClassNames, classesDirName);
        //Util.debug("SVUID", smap);

        // =======================
        // instantiate class loader
        // =======================
        URLClassLoader urlClassLoader = getURLClassLoader(classesDirName);
        LOG.debug("instantiate class loader ... done");

        // =================================================================
        // Generating the skeleton using the class file instead of the IDLs.
        // =================================================================
        String newSourceCode
          = Util.generateImplementationClass(urlClassLoader, remoteClassName, true);

        String newSourcePath
          = remoteClassName.replace('.', File.separatorChar) + "Impl";

        LOG.debug("newSourcePath=["
            + sourceDirName + File.separatorChar + newSourcePath + "]");

        File newSourceFile
          = Util.saveAsJavaSource(newSourceCode, sourceDirName, newSourcePath);

        List<String> extra = new ArrayList<String>();
        extra.add(classesDirName);

        List<String> newSourceList = new ArrayList<String>();
        newSourceList.add(newSourceFile.getAbsolutePath());
        Util.compileJavaClasses(sourceDirName,
                                classesDirName,
                                newSourceList,
                                jars,
                                extra);

        Util.compileRemoteClassesNoValueMethodsON(classesDirName,
                                                  remoteClassName + "Impl",
                                                  extra);

        List<String> newPathList = add(classesDirName, modifiedClassNames);
        Map<Long, String> uidMap
          = Util.extractSerialVersionUid(classesDirName, newPathList);
        //Util.debug("UID", uidMap);

        // =================================
        //  ADAPTING AUTOGENERATED VALUETYPE
        // =================================
        tweakWsdlToCorba(modifiedClassNames, classesDirName);
        LOG.debug("end creating java sources implementation for value types.");
        tweakUID(uidMap);

        //tweakAnnotationWriter(classesDirName, annota);

        //now we need the following classes

        // NEW poaTie (e.g.: _EchoSCServicePortTypeCorbaInterfaceImpl_Tie.class)
        int lastDot = remoteClassName.lastIndexOf(".");
        String poaPackage = remoteClassName.substring(0, lastDot);
        String poaName = "_" + remoteClassName.substring(lastDot + 1)
                       + "Impl_Tie";
        String poaTie     = poaPackage + "." + poaName;

        // Takes the service of the first port type
        QName serviceName
          = extractServiceNameFromPortType(wsdlDirName, portTypeList.get(0));
        String portTypePackage
          = portTypeClassName.substring(0, portTypeClassName.lastIndexOf('.'));
        QName portTypeName = portTypeList.get(0);
        
        // String serviceImpl = portTypePackage + "." + serviceName.getLocalPart() + "Impl";
        String serviceImpl = portTypePackage + "." + portTypeName.getLocalPart() + "CorbaInterfaceImpl";

        LOG.debug("poaTie=" + poaTie + "; serviceImpl=" + serviceImpl);

        ServerCorbaClassesHolder serverCorbaClassesHolder
            = new ServerCorbaClassesHolder();

        // =======================
        // instantiate class loader (Refresh URLClassLoader
        // =======================
        urlClassLoader = getURLClassLoader(classesDirName);
        LOG.debug("instantiate class loader ... done");

        setCorbaOperations(serverCorbaClassesHolder,
                           urlClassLoader,
                           remoteClassName);

        setCorbaPOATie(serverCorbaClassesHolder, urlClassLoader, poaTie);

        setCorbaImpl(serverCorbaClassesHolder,
                        urlClassLoader,
                        remoteClassName + "Impl");

        setWebServiceInterface(serverCorbaClassesHolder,
                               urlClassLoader,
                               portTypeClassName);
        //setCorbaHelper(serverCorbaClassesHolder, urlClassLoader, helper);

        serverCorbaClassesHolder.setUrlClassLoader(urlClassLoader);

        setWebServiceImpl(serverCorbaClassesHolder,
                          urlClassLoader,
                          serviceImpl);

        serverCorbaClassesHolder.setWsdlInformation(wsdlInformation);

        LOG.debug("<<<<< generateConsumerServiceClasses - end");
        return serverCorbaClassesHolder;
    }

    /**
     * This method is used to generate all the consumer code starting from the
     * IDL found inside the WSDL of the Endpoint to consume.
     *
     *
     * @param wsdlDirFile     Dir where the WSDL is located.
     * @param idlContent      Content of the idl file extracted previously from WSDL
     * @param workdir         Where the method can generate the code.
     * @param jars            The jar needed at compile time and run time.
     *
     * @param All the references to the object generated.
     *
     * @throws  ClassGenerationException    When the code cannot be generated.
     *
     */
    public ServerCorbaClassesHolder generateConsumerServiceClassesDirectFromIDL(
        File wsdlDirFile,
        List<String> jars,
        JbiServiceDescriptor jbiServiceDescriptor,
        WsdlInformation wsdlInformation) 
    	throws ClassGenerationException {
    	
        String wsdlDirName = null;
        try {
            wsdlDirName = wsdlDirFile.getCanonicalPath();
        } catch (IOException ioe) {
        	ioe.printStackTrace();
        	String msg=MESSAGES.getString("CRB000500_Error_accessing_the_WSDL_directory");
            LOG.error(msg, ioe);
            throw new ClassGenerationException(msg, ioe);
        }
    	
		ProviderServiceClassesGenerator serviceGenerator = new ProviderServiceClassesGenerator();
		ArrayList<JbiServiceDescriptor> serviceDescriptorList=new ArrayList<JbiServiceDescriptor>();
        ArrayList<String> portTypeNameList= new ArrayList<String>();
        serviceDescriptorList.add(jbiServiceDescriptor);
        
        portTypeNameList.add(wsdlInformation.getPortTypeList().get(0).getLocalPart());
        
        List<ClientCorbaClassesHolder> classes = serviceGenerator.generateProviderServiceClasses(serviceDescriptorList, wsdlDirName, jars, portTypeNameList);
    	
        ServerCorbaClassesHolder serverCorbaClassesHolder  = generateConsumerCorbaClassesHolder(wsdlDirName, jars, classes, serviceGenerator,jbiServiceDescriptor);
        serverCorbaClassesHolder.setWsdlInformation(wsdlInformation);
        serverCorbaClassesHolder.setGenerateClassesFromIDL(true);
        return serverCorbaClassesHolder;
		
    }
 
    /**
     * This method is used to generate all the consumer code starting from the
     * classes already generated with provider class generation API.
     *
     * @param serviceGenerator  ProviderServiceClassesGenerator
     * @param classes      		ClientCorbaClassesHolder
     * @param workdir         	Where the method can generate the code.
     * @param jars            	The jar needed at compile time and run time.
     *
     * @return All the references to the object generated.
     *
     * @throws  ClassGenerationException    When the code cannot be generated.
     *
     */
    public ServerCorbaClassesHolder generateConsumerCorbaClassesHolder(
    		String workdir,
    		List<String> jars,
    		List<ClientCorbaClassesHolder> classes,
    		ProviderServiceClassesGenerator serviceGenerator,
                JbiServiceDescriptor jbidesc
                )
    		throws ClassGenerationException
    {
        //Index is used for the correct interface selection
    	int index=0; 
    	String workdirsrc = workdir + "/src";
    	
    	// The implementation class must be created in the classes directory that 
    	// generates the corba servant. 
    	String implementationDirClasses = workdir + "/origclasses";
        
        String operationsClassNameFullName="";
        //List<File> operationsClass = Util.findFilesFromSourceDirectory(implementationDirClasses, "Operations.class");
        String operationsClassName="";
        try {
            // (dir) + pkg + name + .class
                if(classes.size()>1){

                    for(int i=0;i<classes.size();i++){
                  //select the class by the name
                        String operationClassName=classes.get(i).getOperationsClass().getName();
                        LOG.debug("operationClassName: "+operationClassName);
                        int start=operationClassName.lastIndexOf(".")+1;
                        int end =operationClassName.lastIndexOf("Operations");
                        LOG.debug("Extracted string: "+operationClassName.substring(start, end));
                        LOG.debug("PortTypeLocalPart: "+jbidesc.getPortTypeName().getLocalPart());
                         if (jbidesc.getPortTypeName().getLocalPart().equals(operationClassName.substring(start, end))) {
                            operationsClassNameFullName = new File(implementationDirClasses+File.separator+operationClassName.replace(".", File.separator)+".class").getCanonicalPath();
                            LOG.debug("operationsClassFileNameFullName: "+operationsClassNameFullName+" index: "+i);
                            //LOG.debug(i+"simo ClientCorbaClassesHolder: "+classes.get(i));
                            //LOG.debug("operationsClass: "+Arrays.toString(operationsClass.toArray()));
                            //LOG.debug("ClientCorbaHolderList: "+Arrays.toString(classes.toArray()));
                            //if there are many interface it select the correct interface
                            index=i;
                        break;
                        }
                    }
                } 
                else{
                    String operationClassName=classes.get(0).getOperationsClass().getName();
                    operationsClassNameFullName = new File(implementationDirClasses+File.separator+operationClassName.replace(".", File.separator)+".class").getCanonicalPath();
                    index=0;
                }
        	LOG.debug("Operation Class Name -->"+operationsClassNameFullName);
        	operationsClassName
                = operationsClassNameFullName.substring(
                		implementationDirClasses.length() + 1,
                		operationsClassNameFullName.length() - ".class".length());
            operationsClassName
                = operationsClassName.replace('/', '.').replace('\\', '.');
            LOG.debug("finding Operation classes - " + operationsClassName);
        } catch (IOException e) {
            Object[] args = new Object[] { classes.get(index).getOperationsClass().getName() };
            LOG.error("CRB000503_Error_getting_name", args, e);
            throw new ClassGenerationException(
                    "CRB000503_Error_getting_name", args, e);
        }
        
        String classRoot = operationsClassName.substring(0, operationsClassName.lastIndexOf("Operations"));
        String poaTieClassName     	= classRoot + "POATie";
        String helperClassName     	= classRoot + "Helper";        

		// =======================
		// instantiate class loaders
		// =======================

        URLClassLoader urlClassLoader = serviceGenerator.getUrlClassLoader();
		
		URLClassLoader originalClassLoader = serviceGenerator
				.getOriginalClassLoader();				
		
		LOG.debug("Obtained class loaders (original and modified) ... done");		
        
        // =================================================================
        // Generating the Impl class
        // =================================================================
        String serviceImplClassName = operationsClassName + "Impl";
        
        // Generates the implementation class and saves it
        String newSourceCode = Util.generateImplementationClass(originalClassLoader, operationsClassName, false);                
        String newSourcePath = operationsClassName.replace('.', File.separatorChar) + "Impl";
        LOG.debug("newSourcePath=[" + implementationDirClasses + File.separatorChar + newSourcePath + "]");
        File newSourceFile = 
        	Util.saveAsJavaSource(newSourceCode, workdirsrc, newSourcePath);              
        List<String> extra = new ArrayList<String>();
        extra.add(implementationDirClasses);

        List<String> newSourceList = new ArrayList<String>();
        newSourceList.add(newSourceFile.getAbsolutePath());
        // Compiles the generated source 
        Util.compileJavaClasses(workdirsrc,
        		implementationDirClasses,
                                newSourceList,
                                jars,
                                extra);

        LOG.debug("=======================================================================");
        LOG.debug("                     CONSUMER CLASS ASSOCIATION                        ");
        LOG.debug("=======================================================================");
        LOG.debug("Operation Class Name -->"+classes.get(index).getOperationsClass().getName());
        LOG.debug("POA TIE  NAME -->"+poaTieClassName);
        LOG.debug("SERVICE CLASS  NAME -->"+serviceImplClassName);
        LOG.debug("=======================================================================");
        LOG.debug("                    END CONSUMER CLASS GENERATION                      ");
        LOG.debug("=======================================================================");
        // Creates the ServerCorbaClassesHolder to put into the genrated classes
        ServerCorbaClassesHolder serverCorbaClassesHolder  = new ServerCorbaClassesHolder();
        
        setCorbaOperations(serverCorbaClassesHolder, originalClassLoader, operationsClassName);
        setCorbaPOATie(serverCorbaClassesHolder, originalClassLoader, poaTieClassName);        
        setCorbaImpl(serverCorbaClassesHolder, originalClassLoader,serviceImplClassName);
        setWebServiceInterface(serverCorbaClassesHolder, originalClassLoader,operationsClassName);
        setCorbaHelper(serverCorbaClassesHolder, originalClassLoader, helperClassName);
        serverCorbaClassesHolder.setUrlClassLoader(originalClassLoader);
        // The WebService interface generated in the ClassesGeneration
        serverCorbaClassesHolder.setWebServiceImpl(classes.get(index).getOperationsClass());        
        // All the metadata stuff collected for the runtime part.
	serverCorbaClassesHolder.setValueTypeIdAndInstance(classes.get(index).getValueTypeIdAndInstance());
	serverCorbaClassesHolder.setMethodSignatures(classes.get(index).getMethodSignatures());
	serverCorbaClassesHolder.setAllUniontypes(classes.get(index).getAllUnionTypesMap());
	serverCorbaClassesHolder.setSubstitutedUnionFields(classes.get(index).getSubstitutedUnionFields());
	serverCorbaClassesHolder.setAllInterfacetypes(classes.get(index).getAllInterafceTypesMap());
	serverCorbaClassesHolder.setSubstitutedInterfaceFields(classes.get(index).getSubstitutedInterfaceFields());
	serverCorbaClassesHolder.setAllIDLTypes(classes.get(index).getAllIDLTypes());
	serverCorbaClassesHolder.setCorbaEnumMap(classes.get(index).getCorbaEnumMap());	
        serverCorbaClassesHolder.setIdToClassNameMap(classes.get(index).getIdToClassMap());
	serverCorbaClassesHolder.setUrlClassLoader(urlClassLoader);
	serverCorbaClassesHolder.setOriginalClassLoader(originalClassLoader);
	serverCorbaClassesHolder.setTypeDefs(classes.get(index).getTypeDefs());
                
	return serverCorbaClassesHolder;
    }
    
    
    /**
     * Adds the basedir to the pathList.    
     * @param dir
     * @param pathList
     * @return
     */
    private List<String> add(String dir, List<String> pathList) {
      List<String> newPathList = new ArrayList<String>();

      if (pathList == null || pathList.size() == 0) {
        return newPathList;
      }

      String basedir = null;
      if (dir == null) {
        basedir = "";
      } else {
        basedir = dir;
      }

      for (String path : pathList) {
        String np = basedir + File.separator
                            + path.replace('.', File.separatorChar)
                            + ".class";
        newPathList.add(np);
        LOG.debug("NEWPATH=" + np);
      }
      return newPathList;
    }

    /**
     * This method changes the bytecode of some class to avoid abstract
     * declaration of the valuetype.
     *
     * @param remoteClassName   The class inspected to find the value types
     * @param classesDirName    The directory of the classes files
     * 
     * @return  The return
     *
     * @throws ClassGenerationException    The class generation exception
     */
    private List<String> tweakValueType(String remoteClassName,
      String classesDirName) throws ClassGenerationException {

        LOG.debug("CRB000555_tweakValueType_begin");

        List<String> javaClassNameOfTheModifiedClasses = new ArrayList<String>();

        String cn = remoteClassName.replace('.', File.separator.charAt(0));

        List<File> classList = new ArrayList<File>();
        classList.add(new File(classesDirName + File.separator + cn));

        Set<Class> classesToChange
            = Util.findClassUsed(classesDirName, classList);

        // for each class that represent a valuetype ...
        for (Class c : classesToChange) {
            // ... extracting the class name of the class to manipulate
            String className = classesDirName
                            + File.separator
                            + c.getName().replace('.', '/')
                            + ".class";
            LOG.debug("CRB000556_tweakValueType_class", new Object[]{className});

            javaClassNameOfTheModifiedClasses.add(c.getName());

            // ... preparing the object for the bytecode manipulation
            ClassWriter  cw = new ClassWriter(true); // visitMaxs
            ClassVisitor cc = new CheckClassAdapter(cw);
            StringWriter sw = new StringWriter();
            ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

            ClassAdapter cv = new ValueTypeAdapter(tv, cw, className);

            ClassReader cr = Util.getAsmCLassReader(className);
            LOG.debug("getAsmCLassReader ... done");

            // ... execute
            cr.accept(cv, true);
            LOG.debug("ClassReader.accept ... done");

            if (LOG.isDebugEnabled()) {
                LOG.debug("output of tracer during creation of class: "
                        + className + "\n" + sw.toString());
            }

            // ...extracting the new bytecode
            byte [] newBytecode = cw.toByteArray();

            // ... and replace the old class
            String relativeFileName = className.replace('/', File.separatorChar);
            Util.saveAsJavaClass(relativeFileName, newBytecode);
        }

        LOG.debug("<<<<< tweakValueType - end");
        return javaClassNameOfTheModifiedClasses;
    }

//    /**
//     * XXX javadoc.
//     * 
//     * @param list The list
//     * @param classesDirName  The class dir name
//     * @throws ClassGenerationException  The class generation exception
//     * @return  A map where the key is a class name and the value is an object
//     *          that contains all the annotations of the associated class.
//     */
//    private Map<String, AnnotationsMaps> tweakAnnotationCollector(
//      List<String> list,String classesDirName) throws ClassGenerationException {
//
//      LOG.debug(">>>>> tweakAnnotationCollector - begin");
//
//      Map<String, AnnotationsMaps> annotationTracerMap
//        = new HashMap<String, AnnotationsMaps>();
//
//      ClassAdapter cv = null;
//
//      // for each class to modify ...
//      for (String currentClassName : list) {
//        String fullPath = classesDirName
//                        + File.separator
//                        + currentClassName.replace('.', '/')
//                        + ".class";
//
//        LOG.debug("tweakAnnotationCollector for " + fullPath);
//
//        // ... preparing the object for the bytecode manipulation
//        ClassWriter  cw = new ClassWriter(true); // visitMaxs
//        ClassVisitor cc = new CheckClassAdapter(cw);
//        StringWriter sw = new StringWriter();
//        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));
//
//        AnnotationsMaps annotationTracer = new AnnotationsMaps();
//
//        cv = new AnnotationCollectorAdapter(tv,
//                                   cw,
//                                   fullPath,
//                                   annotationTracer,
//                                   currentClassName);
//
//        annotationTracerMap.put(currentClassName, annotationTracer);
//
//        ClassReader cr = Util.getAsmCLassReader(fullPath);
//        LOG.debug("getAsmCLassReader ... done");
//
//        // ... execute
//        cr.accept(cv, true);
//        LOG.debug("ClassReader.accept ... done");
//
//      }
//
//      LOG.debug("<<<<< tweakAnnotationCollector - end");
//      return annotationTracerMap;
//    }


//    /**
//     * XXX javadoc.
//     * 
//     * @param classesDirName  The class dir name
//     * @param annota  The annotation map
//     * @throws ClassGenerationException The class generation exception
//     */
//    private void tweakAnnotationWriter(String classesDirName,
//      Map<String, AnnotationsMaps> annota) throws ClassGenerationException {
//
//      LOG.debug(">>>>> tweakAnnotationWriter - begin");
//
//      // for each class with annotations ...
//      for (String javaClassName : annota.keySet()) {
//          // ... extracting the class name of the class to manipulate
//          String fullPath = classesDirName
//                          + File.separator
//                          + javaClassName.replace('.', '/')
//                          + ".class";
//
//        LOG.debug("tweakAnnotationWriter for " + fullPath);
//
//        // ... preparing the object for the bytecode manipulation
//        ClassWriter  cw = new ClassWriter(true); // visitMaxs
//        ClassVisitor cc = new CheckClassAdapter(cw);
//        StringWriter sw = new StringWriter();
//        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));
//
//        ClassAdapter cv
//          = new AnnotationWriterAdapter(tv, annota.get(javaClassName));
//
//        ClassReader cr = Util.getAsmCLassReader(fullPath);
//        LOG.debug("getAsmCLassReader ... done");
//
//        // ... execute
//        cr.accept(cv, true);
//        LOG.debug("ClassReader.accept ... done");
//
//        if (LOG.isDebugEnabled()) {
//            LOG.debug("output of tracer during creation of class: "
//                    + fullPath + "\n" + sw.toString());
//        }
//
//        // ...extracting the new bytecode
//        byte [] newBytecode = cw.toByteArray();
//
//        // ... and replace the old class
//        String absoluteFileName = fullPath.replace('/', File.separatorChar);
//        Util.saveAsJavaClass(absoluteFileName, newBytecode);
//      }
//
//      LOG.debug("<<<<< tweakAnnotationWriter - end");
//    }

//    /**
//     * XXX javadoc.
//     * 
//     * @param javaName  The java name
//     * @param newBasedir  The new base dir
//     * @return  The return
//     */
//    private List<String> exctractJavaListJavaName(
//            List<String> javaName, String newBasedir) {
//
//            List<String> result = new ArrayList<String>();
//
//            if (javaName != null) {
//                for (String current : javaName) {
//                    // (dir) + (pkg.name) + (.java)
//                    String j = newBasedir
//                            + File.separator
//                            + current.replace('.', '/')
//                            + ".java";
//
//                    LOG.debug("exctractJavaListJavaName:" + j);
//                    result.add(j);
//                }
//            }
//
//            return result;
//    }


    /**
     * This method manipulate the bytecode to comply Corba specifications.
     *
     * @param    files           The list of IDL files used to deduce
     *                           the name of the class to manipulate.
     * @param    classesDirName  The class dir name
     *
     * @throws ClassGenerationException  The class generation exception
     *
     */
    private void tweakWsdlToCorba(
      List<String> files,
      String classesDirName) throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> tweakAdapting - begin");

        if (files == null || files.size() == 0) {
            LOG.debug("<<<<<<<<<< tweakAdapting - end."
                    + "No files to manipulate.");
            return;
        }

        // else

        // for each valuetype ...
        for (String vt : files) {

            // (dir) + (pkg.name) + (.class)
            String className = classesDirName
                            + File.separator
                            + vt.replace('.', '/')
                            + ".class";

            ClassWriter  cw = new ClassWriter(true); // visitMaxs
            ClassVisitor cc = new CheckClassAdapter(cw);
            StringWriter sw = new StringWriter();
            ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

            ClassAdapter cv
              = new WsdlToCorbaAdapter(tv, cw, className, classesDirName);

            ClassReader cr = Util.getAsmCLassReader(className);
            LOG.debug("getAsmCLassReader ... done");

            cr.accept(cv, true);
            LOG.debug("ClassReader.accept ... done");

            LOG.debug("output of tracer during creation of class: "
                    + className + "\n" + sw.toString());

            byte [] newBytecode = cw.toByteArray();

            // write class in the right place
            String relativeFileName = className.replace('/', File.separatorChar);
            Util.saveAsJavaClass(relativeFileName, newBytecode);
        }

        LOG.debug("<<<<<<<<<< tweakAdapting - end");
    }

    private void tweakUID(Map<Long, String> uidPathMap)
      throws ClassGenerationException {

      LOG.debug(">>>>>>>>>> tweakUID - begin");

      if (uidPathMap == null || uidPathMap.size() == 0) {
          LOG.debug("<<<<<<<<<< tweakUID - end."
                  + "No files to manipulate.");
          return;
      }

      // else

      // for each valuetype ...
      for (Long uid : uidPathMap.keySet()) {

        String path = uidPathMap.get(uid);
        LOG.debug("tweakUID. path=" + path + "; uid=" + uid);

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        ClassAdapter cv = new UIDAdapter(tv, cw, uid);

        ClassReader cr = Util.getAsmCLassReader(path);
        LOG.debug("tweakUID. getAsmCLassReader ... done");

        cr.accept(cv, true);
        LOG.debug("tweakUID. ClassReader.accept ... done");

        LOG.debug("tweakUID. output of tracer during creation of class: "
                + path + "\n" + sw.toString());

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place
        Util.saveAsJavaClass(path, newBytecode);
      }

      LOG.debug("<<<<<<<<<< tweakAdapting - end");
    }


    /**
     * Copy all the IDLs with care about (sub)path.
     *
     * @param    classesDirName    The directory where we can find the IDL files.
     * @param    wsdlDirName        The directory where we copy he IDL files.
     * @param    remoteClassName    The remote class name.
     *
     * @return    The list of the IDL files in classesDirName.
     *
     * @throws    ClassGenerationException  The class generation exception
     */
    public List<String> copyIDLs(
        String classesDirName,
        String wsdlDirName,
        String remoteClassName) throws ClassGenerationException {

        LOG.debug(">>>>> copyIDLs - begin");

        // remoteClassName: full class name without the '.class'

        String idlFilename = classesDirName + File.separator
            + remoteClassName.replace('.', File.separatorChar)
            + ".idl";

        String destIdlFileName
            = wsdlDirName + File.separator
            + remoteClassName.substring(remoteClassName.lastIndexOf('.') + 1)
            + ".idl";

        copyFile(idlFilename, destIdlFileName);

        List<String> idls = null;
        try {

            idls = Util.findIdlFiles(classesDirName);

        } catch (Jbi4CorbaException e) {
            LOG.error(e.getMessage(), e);
            throw new ClassGenerationException(e.getMessage(),e);
        }

        // creating the directory of the package
        File wf = new File(wsdlDirName);
        for (int i = 0; i < (idls == null ? 0 : idls.size()); i++) {
            LOG.debug("copying idls[" + i + "]=" + idls.get(i));

            if (idls.get(i).equals(idlFilename)) {

                LOG.debug("already copied:" + idls.get(i));

            } else {
                String pkgAsDir = subBasedir(idls.get(i), classesDirName);

                mkdir(wf, pkgAsDir);

                String fn = pkgAsDir + File.separator
                        + new File(idls.get(i)).getName();

                copyFile(classesDirName + File.separator + fn,
                        wsdlDirName    + File.separator + fn);
            }

        }

        LOG.debug("<<<<< copyIDLs - end");
        return idls;
    }


    /**
     * Remove the base dir in the directory class name.
     *
     * @param    fullFilename    Canonical file name of the file
     * @param    basedir         The basedir
     *
     * @return    The result.
     */
    private String subBasedir(String fullFilename, String basedir) {
        // full = basedir + pkg + filename.idl
        LOG.debug(">>>>> subBasedir - begin");
        String s = fullFilename.substring(basedir.length() + 1);

        int limit = s.lastIndexOf(File.separator);

        s = s.substring(0, limit);

        LOG.debug("<<<<< subBasedir - end:" + s);
        return s;
    }




    /**
     * @param portTypesList  The port types list
     * @param classesDirName  The class dir name
     * @throws ClassGenerationException  The class generation exception
     * @return  The full java class name (package + class).
     */
    private String findingPortTypeClassName(List<QName> portTypesList,
        String classesDirName) throws ClassGenerationException {

        LOG.debug(">>>>> finding portTypeClassName - begin");

        int size = (portTypesList == null) ? 0 : portTypesList.size();
        if (size != 1) {
            LOG.error("CRB000504_Expected_exactly_one_class", size);
            throw new ClassGenerationException(
                    "CRB000504_Expected_exactly_one_class",
                    new Object[] { size }, null);
        }

        String c = portTypesList.get(0).getLocalPart() + ".class";
        //String p = portTypesList.get(0).getNamespaceURI().replace('.', '/');

        // FIXME what happen if I have 2 classes with the same name but
        //       different package.
        List<File> portTypeClasses = Util.findFilesFromSourceDirectory(
                classesDirName, c);

        if (portTypeClasses.size() != 1) {
            LOG.error("CRB000505_PortType_class_not_found", c);
            throw new ClassGenerationException(
                    "CRB000505_PortType_class_not_found", new Object[] { c },
                    null);
        } else {
            LOG.debug("PortType class not found:" + portTypeClasses.get(0));
        }

        String portTypeClassName;
        try {
            // (dir) + pkg + name + .class
            portTypeClassName = portTypeClasses.get(0).getCanonicalPath();

            portTypeClassName
                = portTypeClasses.get(0).getCanonicalPath().substring(
                        classesDirName.length() + 1,
                        portTypeClassName.length() - ".class".length());

            portTypeClassName
                = portTypeClassName.replace('/', '.').replace('\\', '.');

            LOG.debug("finding PortType class - " + portTypeClassName);
        } catch (IOException e) {
            Object[] args = new Object[] { portTypeClasses.get(0) };

            LOG.error("CRB000503_Error_getting_name", args, e);
            throw new ClassGenerationException(
                    "CRB000503_Error_getting_name", args, e);
        }

        LOG.debug("<<<<< finding portTypeClassName - end");
        return portTypeClassName;
    }

    /**
     * XXX javadoc.
     * 
     * @param sourceDirName  The source dir name
     * @param classesDirName  The classes dir name
     * @param jarFilesName  The jar files name
     * @return  The return
     * @throws ClassGenerationException  The class generation exception
     */
    private List<String> javac(
        String sourceDirName,
        String classesDirName,
        List<String> jarFilesName) throws ClassGenerationException {

        LOG.debug(">>>>> javac - begin");
        List<String> sources;
        try {

            sources = Util.findJavaSources(sourceDirName);

        } catch (Jbi4CorbaException e) {
            Object[] args = new Object[] { sourceDirName, e.getMessage() } ;

            String msg=MESSAGES.getString("CRB000501_Error_generating_sources_list",
        			new java.lang.Object[] {args}, new java.lang.Object[] {e});
            LOG.error(msg);
            throw new ClassGenerationException(msg);
        }
        LOG.debug("compiling java classes - sources found");

        Util.compileJavaClasses(sourceDirName,
                                classesDirName,
                                sources,
                                jarFilesName,
                                null);

        LOG.debug("<<<<< javac - end");
        return sources;
    }

/**
 * 
 * @param serverCorbaClassesHolder   The server corba classes holder
 * @param urlClassLoader             The url class loader
 * @param helper                     The helper
 * @throws ClassGenerationException  The class generation exception
 */
    private void setCorbaHelper(
        ServerCorbaClassesHolder serverCorbaClassesHolder,
        URLClassLoader urlClassLoader,
        String helper) throws ClassGenerationException {

        LOG.debug(">>>>> setCorbaHelper - begin");
        try {
            serverCorbaClassesHolder.setCorbaHelper(
                    urlClassLoader.loadClass(helper));

        } catch (ClassNotFoundException e) {
            Object[] args
                    = new Object[] { "setCorbaHelper", helper, urlClassLoader };

            LOG.error("CRB000506_Error_during_method", args, e);
            throw new ClassGenerationException(
                    "CRB000506_Error_during_method", args, e);
        }
        LOG.debug("<<<<< setCorbaHelper - end");
    }


  /**
   * 
   * @param serverCorbaClassesHolder   The server corba classes holder
   * @param urlClassLoader             The url class loader
   * @param portType                   The port type
   * @throws ClassGenerationException  The class generation exception
   */
    private void setWebServiceInterface(
        ServerCorbaClassesHolder serverCorbaClassesHolder,
        URLClassLoader urlClassLoader,
        String portType) throws ClassGenerationException {

        LOG.debug(">>>>> setWebServiceInterface - begin");
        try {
            serverCorbaClassesHolder.setWebServiceInterface(
                    urlClassLoader.loadClass(portType));

        } catch (ClassNotFoundException e) {
            Object[] args = new Object[] {
                    "setWebServiceInterface", portType, urlClassLoader };

            LOG.error("CRB000506_Error_during_method", args, e);
            throw new ClassGenerationException(
                    "CRB000506_Error_during_method", args, e);
        }
        LOG.debug("<<<<< setWebServiceInterface - end");
    }

 /**
  * 
  * @param serverCorbaClassesHolder   The server corba classesHolder
  * @param urlClassLoader             The url class loader
  * @param implClass                  The impl class
  * @throws ClassGenerationException  The class generation exception
  */
    private void setWebServiceImpl(
        ServerCorbaClassesHolder serverCorbaClassesHolder,
        URLClassLoader urlClassLoader,
        String implClass) throws ClassGenerationException {

        LOG.debug(">>>>> setWebServiceImpl - begin");
        try {
            serverCorbaClassesHolder.setWebServiceImpl(
                    urlClassLoader.loadClass(implClass));

        } catch (ClassNotFoundException e) {

            Object[] args = new Object[] {
                    "setWebServiceImplementation", implClass, urlClassLoader };

            LOG.error("CRB000506_Error_during_method", args, e);
            throw new ClassGenerationException(
                    "CRB000506_Error_during_method", args, e);
        }
        LOG.debug("<<<<< setWebServiceImplementation - end");
    }


 /**
  * 
  * @param serverCorbaClassesHolder   The server corba classes holder
  * @param urlClassLoader             The url class loader
  * @param poaTie                     The poa tie
  * @throws ClassGenerationException  the class generation exception
  */
  private void setCorbaPOATie(
    ServerCorbaClassesHolder serverCorbaClassesHolder,
    URLClassLoader urlClassLoader,
    String poaTie) throws ClassGenerationException {

    LOG.debug(">>>>> setCorbaPOATie - begin");
    try {

      serverCorbaClassesHolder.setCorbaPOATie(
              urlClassLoader.loadClass(poaTie));

    } catch (ClassNotFoundException e) {
      Object[] args = new Object[] {
              "setCorbaPOATie", poaTie, urlClassLoader };

      LOG.error("CRB000506_Error_during_method", args, e);
      throw new ClassGenerationException(
              "CRB000506_Error_during_method", args, e);
    }
    LOG.debug("<<<<< setCorbaPOATie - end");
  }

  /**
   * 
   * @param serverCorbaClassesHolder  The server corba classes holder
   * @param urlClassLoader            The url class loader
   * @param corbaImpl                 The implementation of the corba interface.
   * 
   * @throws ClassGenerationException  the class generation exception
   */
   private void setCorbaImpl(
     ServerCorbaClassesHolder serverCorbaClassesHolder,
     URLClassLoader urlClassLoader,
     String corbaImpl) throws ClassGenerationException {

     LOG.debug(">>>>> setCorbaImpl - begin");
     try {

       serverCorbaClassesHolder.setCorbaImplClass(
               urlClassLoader.loadClass(corbaImpl));

     } catch (ClassNotFoundException e) {
       Object[] args = new Object[] {
               "setCorbaImpl", corbaImpl, urlClassLoader };

       LOG.error("CRB000506_Error_during_method", args, e);
       throw new ClassGenerationException(
               "CRB000506_Error_during_method", args, e);
     }
     LOG.debug("<<<<< setCorbaImpl - end");
   }

  /**
   * 
   * @param serverCorbaClassesHolder   The server corba classes holder
   * @param urlClassLoader             The url class loader
   * @param operations                 The operations
   * @throws ClassGenerationException  The class generation exception
   */
    private void setCorbaOperations(
        ServerCorbaClassesHolder serverCorbaClassesHolder,
        URLClassLoader urlClassLoader,
        String operations) throws ClassGenerationException {

        LOG.debug(">>>>> setCorbaOperations - begin");
        try {
            serverCorbaClassesHolder.setCorbaOperations(
                    urlClassLoader.loadClass(operations));

        } catch (ClassNotFoundException e) {
            Object[] args = new Object[] {
                    "setCorbaOperations", operations, urlClassLoader };

            LOG.debug("urlClassLoader.getURLs()="
                      + Arrays.asList(urlClassLoader.getURLs()));
            LOG.error("CRB000506_Error_during_method", args, e);
            throw new ClassGenerationException(
                    "CRB000506_Error_during_method", args, e);
        }
        LOG.debug("<<<<< setCorbaOperations - end");
    }

    /**
     * The list of names of the jars used in the classpath.
     *
     * @param    libDirName    The directory where the jars are located.
     *
     * @return    The list of names of the jars used in the classpath.
     *
     * @throws    ClassGenerationException
     */
//    private List<String> prepareClassPath(String libDirName)
//        throws ClassGenerationException {
//
//        LOG.debug(">>>>> prepareClassPath - begin");
//        List<File> jarFiles
//            = Util.findFilesFromSourceDirectory(libDirName, ".jar");
//
//        List<String> jarFilesName = new ArrayList<String>();
//
//        for (File jarFile:jarFiles) {
//            try {
//                LOG.debug("Adding jar " + jarFile.getCanonicalPath() + " ... ");
//
//                jarFilesName.add(jarFile.getCanonicalPath());
//
//                LOG.debug("... jar " + jarFile.getCanonicalPath() + " added.");
//            } catch (IOException e) {
//                Object[] args = new Object[] { jarFile, e.getMessage() };
//
//                LOG.error("CRB000507_Error_getting_canonicalPath_from_file",
//                          args, e);
//                throw new ClassGenerationException(
//                        "CRB000507_Error_getting_canonicalPath_from_file",
//                        args, e);
//            }
//        }
//
//        LOG.debug("<<<<< prepareClassPath - end");
//        return jarFilesName;
//    }

    /**
     * This method copies orb.idl and ir.idl in wsdlDirName.
     *
     * @param    wsdlDirName        The directory where the files are copied.
     *
     * @throws    ClassGenerationException  The class generation exception
     */
    private void copyOrbIrIdl(String wsdlDirName)
        throws ClassGenerationException {

        LOG.debug(">>>>> copyOrbIrIdl - begin");

        //add orb.idl and ir.idl
        InputStream orbis
            =  this.getClass().getClassLoader().getResourceAsStream("orb.idl");

        if (orbis == null) {
            LOG.error("CRB000508_Could_not_find_orb.idl_in_path");
            throw new ClassGenerationException(
                    "CRB000508_Could_not_find_orb.idl_in_path");
        } else {
            LOG.debug("orbis not null ... orb.idl found");
        }

        InputStream iris
            = this.getClass().getClassLoader().getResourceAsStream("ir.idl");

        if (iris == null) {
            LOG.error("CRB000509_Could_not_find_ir.idl_in_path");
            throw new ClassGenerationException(
                    "CRB000509_Could_not_find_ir.idl_in_path");
        } else {
            LOG.debug("iris not null ... ir.idl found");
        }

        String orbFileName = wsdlDirName + File.separator + "orb.idl";
        String irFileName  = wsdlDirName + File.separator + "ir.idl";

        copyFile(orbis, orbFileName);
        copyFile(iris, irFileName);

        LOG.debug("copyFile (orb.idl and ir.idl) to " + wsdlDirName + " ...ok");

        LOG.debug("<<<<< copyOrbIrIdl - end");
    }

  /**
   * Adds the "remote" charateristics to the PortType class generated.
   * The modifications are:
   * 1) add the java.rmi.Remote interface to the class
   * 2) add the java.rmi.RemoteException to the 'throws' clause of each method.
   * 
   * @param portTypeClassName  The port type class name
   * @param classesDirName     The classes dir name
   * @return                   The return
   * @throws ClassGenerationException  The class generation exception
   */
    private String tweakInterfaceClasses(
            String portTypeClassName,
            String classesDirName) throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> tweakInterfaceClasses - begin");

        LOG.debug("remotizing class: " + portTypeClassName
                + " in dir: " + classesDirName);

        ClassWriter cw = new ClassWriter(true);
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        RemoteEnhancerAdapter cv = new RemoteEnhancerAdapter(tv,
                getAsFullyQualifiedNameInternalForm(portTypeClassName));

        LOG.debug("new ClassReader - Begin");
        ClassReader cr;
        try {
            cr = new ClassReader(new FileInputStream(getAsFileName(
                    classesDirName, portTypeClassName, ".class")));
        } catch (IOException e) {
            Object[] args = new Object[] { portTypeClassName, classesDirName };

            LOG.error("CRB000510_Could_not_instantiate_class_reader", args, e);
            throw new ClassGenerationException(
                    "CRB000510_Could_not_instantiate_class_reader", args, e);
        }
        LOG.debug("new ClassReader - End");

        cr.accept(cv, true);

        LOG.debug("output of tracer during creation of class: "
                + portTypeClassName + "\n" + sw.toString());

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place
        String relativeFileName
            = cv.getCompleteName().replace('/', File.separatorChar);

        LOG.debug("relativeFileName=" + relativeFileName
                + "; cv.getCompleteName()=" + cv.getCompleteName());

        Util.saveAsJavaClass(classesDirName + File.separator + relativeFileName
                + ".class", newBytecode);

        String remoteClassName = cv.getCompleteName().replace('/', '.');

        LOG.debug("<<<<<<<<<< tweakInterfaceClasses - end:" + remoteClassName);
        return remoteClassName;
    }



    // ========================
    //          Utility Methods
    // ========================

/**
 * @param sourceFileName             The source file name
 * @param destFileName               The dest file name
 * @throws ClassGenerationException  The class generation exception
 */
    private void copyFile(String sourceFileName,String destFileName)
        throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> copyFile(String, String) - begin");

        LOG.debug("copying: " + sourceFileName + " to: " + destFileName);

        FileInputStream is;
        try {
            is = new FileInputStream(sourceFileName);
        } catch (FileNotFoundException e) {
            Object[] args = new Object[] { sourceFileName, e.getMessage() };

            LOG.error("CRB000511_Error_opening", args, e);
            throw new ClassGenerationException(
                    "CRB000511_Error_opening", args, e);
        }

        copyFile(is, destFileName);
        LOG.debug("<<<<<<<<<< copyFile(String, String) - end");
    }

 /**
  * @param sourceInputStream          The source input stream
  * @param destFileName               The dest file name
  * @throws ClassGenerationException  The class generation exception
  */
    private void copyFile(InputStream sourceInputStream, String destFileName)
        throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> copyFile(InputStream, String) - begin");

        FileOutputStream fos;

        LOG.debug("copying: " + sourceInputStream + " to: " + destFileName);
        try {

            fos = new FileOutputStream(new File(destFileName));

        } catch (FileNotFoundException e) {
            Object[] args = new Object[] { destFileName, e.getMessage() };

            LOG.error("CRB000512_Error_creating", args, e);
            throw new ClassGenerationException(
                    "CRB000512_Error_creating", args, e);
        }

        LOG.debug("FileUtil.copyInputStream");

        try {
            Util.copyInputStream(sourceInputStream, fos);
        } catch (IOException e) {
            Object[] args = new Object[] {
                    sourceInputStream, destFileName, e.getMessage() };

            LOG.error("CRB000513_Error_copying_input_stream", args, e);
            throw new ClassGenerationException(
                    "CRB000513_Error_copying_input_stream", args, e);
        }

        LOG.debug("<<<<<<<<<< copyFile(InputStream, String) - end");
    }

//    /**
//     * Generetes Java source from IDL files (TIE model).
//     *
//     * @param    sourceDirName  The source dir name
//     * @param    idlFilename  The idl file name
//     * @param    wsdlDirName  The wsdl dir name
//     * @param    destIdlFileName  The dest idl file name
//     *
//     */
//    private void idl2javaTieModel(String sourceDirName, String idlFilename,
//        String wsdlDirName, String destIdlFileName) {
//
//        LOG.debug(">>>>>>>>>> idl2java - begin");
//
//        LOG.debug("\n workdir: "         + sourceDirName
//                + ";\n idlfilename: "    + idlFilename
//                + ";\n wsdlDirName:"     + wsdlDirName
//                + ";\n destIdlFileName:" + destIdlFileName);
//
//        /* Run the IDL-to-Java compiler, idlj, TWICE on the IDL file
//         * to create stubs and skeletons.
//         * This step assumes that you have included the path to the java/bin
//         * directory in your path.
//         *
//         * idlj -fall  Hello.idl
//         * idlj -fallTie Hello.idl
//         *
//         * You must use the -fall option with the idlj compiler
//         * to generate both client and server-side bindings.
//         * This command line will generate the default server-side bindings,
//         * which assumes the POA programming model.
//         *
//         * The -fallTie option generates another file, HelloPOATie,
//         * which is used to create a Tie.
//         */
//
//        /* OPTIONS:
//         *  -fall       To generate both client and server-side bindings.
//         *     -td    <dir>   Use <dir> for the output instead of the current dir.
//         *  -i <dir>    The current dir is scanned for included files.
//         *              This option adds another directory.
//         *  -emitAll    Emit all types, including those found in #include files.
//         *  -v          Verbose.
//         */
//
//        String [] optionsFALL = new String[] { "-fall", "-td", sourceDirName,
//          "-i", wsdlDirName, "-emitAll", destIdlFileName };
//
//        if (LOG.isDebugEnabled()) {
//          LOG.debug("Compile. 1) -fall");
//          //LOG.debug("If debug is enable you see a warning message of IDLJ:"
//          //        + "'Error reading Messages File.'");
//
//          // whe debug is enable we use the option 'verbose'.
//          optionsFALL = new String[] { "-fall", "-td", sourceDirName, "-v",
//            "-i", wsdlDirName, "-emitAll", destIdlFileName };
//        }
//
//
//        com.sun.tools.corba.se.idl.toJavaPortable.Compile.main(optionsFALL);
//
//
//        String [] optionsFallTie = new String[] { "-fallTIE", "-td",
//          sourceDirName, "-i", wsdlDirName, "-emitAll", destIdlFileName };
//
//        if (LOG.isDebugEnabled()) {
//            LOG.debug("Compile. 2) -fallTIE");
//            //LOG.debug("If debug is enable you see a warning message of IDLJ:"
//            //        + "'Error reading Messages File.'");
//
//            // whe debug is enable we use the option 'verbose'.
//            optionsFallTie = new String[] {"-fallTIE", "-td", sourceDirName,
//              "-v", "-i", wsdlDirName, "-emitAll", destIdlFileName };
//          }
//
//        com.sun.tools.corba.se.idl.toJavaPortable.Compile.main(optionsFallTie);
//
//        LOG.debug("<<<<<<<<<< idl2java - end");
//    }

/**
 * @return  The return
 * @throws ClassGenerationException  The class generation exception
 */
    private FileSystemManager getFileSystemManager()
        throws ClassGenerationException {

        try {
            return VFS.getManager();
        } catch (FileSystemException e) {
            Object[] args = new Object[] { e.getMessage() };

            LOG.error("CRB000514_Error_inizializing_virtual_filesystem",
                      args, e);
            throw new ClassGenerationException(
                    "CRB000514_Error_inizializing_virtual_filesystem", args, e);
        }
    }

/**
 * @param classesDirName  The classes dir name 
 * @return  The return
 * @throws ClassGenerationException  The class generation exception
 */
    private URLClassLoader getURLClassLoader(String classesDirName)
        throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> getURLClassLoader - begin");

        URLClassLoader urlClassLoader = null;
        String protocol = null;
        try {

          if (System.getProperty("os.name").indexOf("Win") >=0) {
              protocol = "file:///";
          } else {
              protocol = "file://";
          }

          urlClassLoader = new URLClassLoader(
                new URL[] { new URL(protocol + classesDirName + "/") },
                  this.getClass().getClassLoader());

          LOG.debug("url classloader: "
                + Arrays.asList(urlClassLoader.getURLs()));
        } catch (MalformedURLException e) {
            Object[] args = new Object[] { protocol + classesDirName + "/" };

            LOG.error("CRB000515_Could_not_instantiate_url_class_loader",
                      args, e);
            throw new ClassGenerationException(
                    "CRB000515_Could_not_instantiate_url_class_loader", args,
                    e);
        }

        LOG.debug("<<<<<<<<<< getURLClassLoader - end");
        return urlClassLoader;
    }



    /**
     * Copy a WSDL file in workdir with the name 'service.wsdl'.
     *
     * @param    workdir            Where to copy the wsdl.
     * @param    wsdlStringUrl    Where the wsdl is located.
     *
     * @return    wsdl dir (as file).
     *
     * @throws    ClassGenerationException  The class generation exception
     */
    private File copyWsdlTo(String workdir, String wsdlStringUrl)
        throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> copyWsdlTo - begin");

        LOG.debug("workdir=" + workdir + "; wsdlStringUrl=" + wsdlStringUrl);

        File workdirFile = new File(workdir);
        FileSystemManager fsManager = getFileSystemManager();

        FileObject wsdlFile;
        try {
            wsdlFile = fsManager.resolveFile(wsdlStringUrl);
        } catch (FileSystemException e) {
            Object[] args = new Object[] { wsdlStringUrl, e.getMessage() };

            LOG.error("CRB000516_Error_resolving_file", args, e);
            throw new ClassGenerationException(
                    "CRB000516_Error_resolving_file", args, e);
        }

        LOG.debug("wsdl location resolved");
        
        InputStream wsdlInputStream;
        try {
            wsdlInputStream = wsdlFile.getContent().getInputStream();
        } catch (IOException e) {
            Object[] args = new Object[] { wsdlFile, e.getMessage() };

            LOG.error("CRB000517_Error_opening_url_inputstream", args, e);
            throw new ClassGenerationException(
                    "CRB000517_Error_opening_url_inputstream", args, e);
        }

        LOG.debug("wsdl content extracted");

        File wsdlDirFile;
        String wsdlDirName;
        try {
            wsdlDirFile = Util.createUniqueDirectory(workdirFile, "wsdl");
            wsdlDirName = wsdlDirFile.getCanonicalPath();
        } catch (IOException e) {
            Object[] args = new Object[] { e.getMessage() };

            LOG.error("CRB000518_Error_creating_dir_for_wsdl", args, e);
            throw new ClassGenerationException(
                    "CRB000518_Error_creating_dir_for_wsdl", args, e);
        }

        LOG.debug("wsdl dir created:" + wsdlDirName);

        String wsdlFileName =  getWsdlFileName(wsdlDirName);

        copyFile(wsdlInputStream, wsdlFileName);

        LOG.debug("wsdl copied: " + wsdlFileName);
        LOG.debug("<<<<<<<<<< copyWsdlTo - end");
        return wsdlDirFile;
    }

   /**
    * @param wsdlDirName  The wsdl dir name
    * @return  The return
    */
    private String getWsdlFileName(String wsdlDirName) {
        return wsdlDirName + File.separator + "service.wsdl";
    }

    /**
     * This method create a new directory.
     *
     * @param    basedir        Where the new directory must be created.
     * @param    newdir        The name of the new directory.
     *
     * @return    The canonical path of the new directory.
     *
     * @throws    ClassGenerationException  The classgeneration exception
     */
    private String mkdir(File basedir, String newdir)
        throws ClassGenerationException {

        LOG.debug(">>>>> mkdir - begin");

            File sourceDir = new File(basedir, newdir);
            Util.buildDirectory(sourceDir);
            String sourceDirName;
        try {
            sourceDirName = sourceDir.getCanonicalPath();
        } catch (IOException e) {
            Object[] args = new Object[] { sourceDir, e.getMessage() };

            LOG.error("CRB000519_Error_getting_canonical_path", args, e);
            throw new ClassGenerationException(
                    "CRB000519_Error_getting_canonical_path", args, e);
        }

        LOG.debug("<<<<< mkdir - end. newdir=" + sourceDirName);
        return sourceDirName;
    }

    /**
     * This method is used to generate the java source code that maps a WSDL.
     *
     * @param    wsdlDirName        Where the wsdl is located.
     * @param    sourceDirName    Where the source will be generate.
     *
     * @return   The information extracted from the WSDL file.
     *
     * @throws   ClassGenerationException  The class generation exception
     */
    private WsdlInformation wsdlToJava(String wsdlDirName, String sourceDirName, List<String> jars)
        throws ClassGenerationException {

        LOG.debug(">>>>> wsdlToJava - begin");
        
        String wsdlFileName = getWsdlFileName(wsdlDirName);

        ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader(); 
        
        WsdlInformation wsdlInformation = null;

        try {
            // Gets the factory classloader to make the PlugiTools to correctly load the ObjectFactory class
            org.apache.cxf.tools.plugin.ObjectFactory factory = new org.apache.cxf.tools.plugin.ObjectFactory();
            ClassLoader factoryClassLoader = factory.getClass().getClassLoader();
            Thread.currentThread().setContextClassLoader(factoryClassLoader);       

            WSDLToJava gen = new WSDLToJava();

            String[] w2jArgs = new String[]{"-d", sourceDirName, wsdlFileName};                                       
            gen.setArguments(w2jArgs);        
            wsdlInformation = extractWsdlInformation(wsdlFileName);

            LOG.debug("wsdlDirName="       + wsdlDirName
                      + "; wsdlFileName="    + wsdlFileName
                      + "; OutputDirectory=" + sourceDirName);

            gen.run(new ToolContext());                                   

        } catch (Exception e) {
        	e.printStackTrace();
            Object[] args = new Object[] { wsdlFileName, e.getMessage() };

            LOG.error("CRB000520_Error_generating_source_file", args, e);
            throw new ClassGenerationException(
                    "CRB000520_Error_generating_source_file", args, e);
        } finally {
            // Sets back the previous classloader
            Thread.currentThread().setContextClassLoader(oldClassLoader);    
        }

        LOG.debug("<<<<< wsdlToJava - end");
        return wsdlInformation;
    }

    /**
     * This method extract the PortTypes present in the wsdl.
     *
     * @param    wsdlFileName    The absolute file name of the wsdl.
     *
     * @return   The object that contains the information of the WSDL.
     *
     * @throws   ClassGenerationException  The class generation exception
     */
    private WsdlInformation extractWsdlInformation(String wsdlFileName)
        throws ClassGenerationException {
        LOG.debug(">>>>> extractWsdlInformation - begin");

        WsdlInformation wi = new WsdlInformation();

        try {
            
            // Gets the Service model from the service
            CXFUtils.setupBus();
            WSDLServiceBuilder b = new WSDLServiceBuilder(CXFUtils.getBus());
            
            // Gets the WSDL Definition
            WSDLFactory wsdlFactory = WSDLFactory.newInstance();
            final WSDLReader reader =  wsdlFactory.newWSDLReader();
            reader.setFeature(Constants.FEATURE_VERBOSE, false);
            reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
          
            final Definition def = reader.readWSDL(wsdlFileName);
            
            List<ServiceInfo> serviceInfo = b.buildServices(def);
                                   
            for (Object o : serviceInfo) {
                ServiceInfo sInfo = (ServiceInfo) o;
                
                // Gets the porttype (interface) name
                QName pt = sInfo.getInterface().getName();
                wi.getPortTypeList().add(pt);
                LOG.debug("PortType[" + pt + "] in WsdlFile=" + wsdlFileName);

                wi.getServiceAndPortType().put(sInfo.getName(), pt);

                wi = findOperationByMEP(sInfo, pt, wi);
            }

        } catch (Exception e) {
            LOG.error("CRB000521_Extracting_PortTypes_error", e);
            throw new ClassGenerationException(
                    "CRB000521_Extracting_PortTypes_error", null, e);
        }

        LOG.debug("<<<< extractWsdlInformation - end");
        return wi;
    }

    /**
     * This method extracts asychronous and synchronous operation and store
     * them in the WsdlInformation object.
     *
     * @param   service   The service to inspect.
     * @param   portType  The portType associated to the operations.
     * @param   wi        The WsdlInformation used.
     *
     * @return  The WsdlInformation updated.
     */
    protected WsdlInformation findOperationByMEP(ServiceInfo serviceInfo,
      QName portType, WsdlInformation wi) {

      if (wi == null) {
        wi = new WsdlInformation();
      }
      if (portType == null) {
        LOG.debug("No operations for a nillable PortType!");
        return wi;
      }

      // init
      List<QName> asyncList = new ArrayList<QName>();
      List<QName> syncList = new ArrayList<QName>();
      wi.getAsynchOperationMap().put(portType, asyncList);
      wi.getSynchOperationMap().put(portType, syncList);

      if (serviceInfo == null) {
        LOG.debug("No operations for a nillable service!");
        return wi;
      }
      
      Iterator opIterator
        = serviceInfo.getInterface().getOperations().iterator();

      while (opIterator.hasNext()) {
        OperationInfo op = (OperationInfo) opIterator.next();

        /* isAsync doesn't work as I expected ... as usual :-(
         *
         * So if the operation has no output I suppose is oneway
         * 
         */
        LOG.debug("OperationInfo.isAsync:" + op.isOneWay()
                + "; OperationInfo.hasInput()=" + op.hasInput()
                + "; OperationInfo.hasOutput()=" + op.hasOutput());

        if (! op.hasOutput()) {
          LOG.debug("Asynchronous Operation; operationName=" + op.getName());

          wi.getAsynchOperationMap().get(portType).add(op.getName());
        } else {
          LOG.debug("Synchronous Operation; operationName=" + op.getName());

          wi.getSynchOperationMap().get(portType).add(op.getName());
        }
      }

      return wi;
    }


    /**
     * This method extract the service name for the PortType from the WSDL.
     *
     * @param    wsdlDirName    The wsdl dir name
     * @param    portType       The port type
     *
     * @return   The service name
     *
     * @throws   ClassGenerationException  The class generation exception 
     */
    private QName extractServiceNameFromPortType(String wsdlDirName, QName portType)
        throws ClassGenerationException {
        LOG.debug(">>>>> extractPortTypes - begin");

        String wsdlFileName = getWsdlFileName(wsdlDirName);
        try {

            // Gets the Service model from the service
            WSDLServiceBuilder b = new WSDLServiceBuilder(CXFUtils.getBus());
            
            // Gets the WSDL Definition
            WSDLFactory wsdlFactory = WSDLFactory.newInstance();
            final WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader();
            reader.setFeature(Constants.FEATURE_VERBOSE, false);
            reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
                                    
            final Definition def = reader.readWSDL(wsdlFileName);
            
            List<ServiceInfo> serviceInfo = b.buildServices(def);
            
            for (Object o : serviceInfo) {
                QName pt = ((ServiceInfo) o).getInterface().getName();
                if (pt.equals(portType)) {
                    QName serviceName = ((ServiceInfo) o).getName();
                    LOG.debug("Service[" + serviceName + "] found in WsdlFile=" + wsdlFileName + " for portType:" + portType);
                    return serviceName;
                }
            }
            // IF no service is found, throws an exception
            LOG.error("CRB000542_Error_during_service_name_lookup", String.valueOf(portType));
            throw new ClassGenerationException("CRB000542_Error_during_service_name_lookup", new Object[]{String.valueOf(portType)});

        } catch (Exception e) {
            LOG.error("CRB000542_Error_during_service_name_lookup", e);
            throw new ClassGenerationException(
                    "CRB000542_Error_during_service_name_lookup", null, e);
        }
    }


    /**
     * 
     * @param basedir   The base dir
     * @param javaName  The java name
     * @param ext  The ext
     * @return  The return
     */
    private String getAsFileName(String basedir, String javaName, String ext) {
        // FIXME null
        char sep = File.separator.charAt(0);
        basedir = basedir.replace('\\', sep).replace('/', sep);

        return basedir + sep + javaName.replace('.', sep) + ext;
    }

  /**
   * 
   * @param javaName  The java name
   * @return  The return
   */
    private String getAsFullyQualifiedNameInternalForm(String javaName) {
        // FIXME null
        return javaName.replace('.', '/');
    }
    
    
    /**
     * Add <code>java.lang.Exception</code> as Superclass.
     * The exception classes generated from the WSDL does not extends
     * <code>java.lang.Exception</code>.
     * 
     * Code taken from the JBi4Ejb project
     * 
     * @param exceptions
     *          The exceptions to transform
     * @param classesDirName
     *          Where the classes are
     * 
     * @throws ClassGenerationException
     *          If some problem occurs
     */
    private void addExceptionSuperclass(List<String> exceptions, String classesDirName) 
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
            	String msg=MESSAGES.getString("CRB000557_Failure_generating_ClassReader_in_addExceptionSuperclass", 
            			new Object[] {e.getMessage()});
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
	 * Returns true if the string parameter is null or empty.
	 * @param str
	 *            The string to be checked
	 *             
	 * @return The return
	 */
    private boolean isEmpty(String str) {
    	return ((str == null) || (str.trim().length() <= 0));
    }
 
	/**
	 * Returns true if the jbiServiceDescriptor object contains details that point to a valid idl file.
	 * @param jbiServiceDescriptor
	 *            The Object that contains details about IDL (file name and directory)
	 *             
	 * @return The return
	 */
    private boolean isIdlIncluded(JbiServiceDescriptor jbiServiceDescriptor)
			throws ClassGenerationException {
		boolean result = false;
		
		if (jbiServiceDescriptor == null || 
			isEmpty(jbiServiceDescriptor.getIdlFileName()) || 
			isEmpty(jbiServiceDescriptor.getIdlFileNameDirectory())) {
			return false;
		}

		String filepath = jbiServiceDescriptor.getIdlFileNameDirectory()
				+ File.separator + jbiServiceDescriptor.getIdlFileName();

		try {
			File idlFile = new File(filepath);
			if (idlFile.exists()) {
				String idl = HelperFileUtil.readFileAsString(idlFile).trim();
				result = (! isEmpty(idl));
			}
		} catch (Exception e) {
			Object[] args = new Object[] { filepath, e.getMessage() };
			LOG.error("CRB000512_Error_creating", args, e);
			throw new ClassGenerationException("CRB000512_Error_creating",
					args, e);
		}
		return result;
	}
}
