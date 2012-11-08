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
import it.imolinfo.jbi4corba.utils.HelperIDLJUtil;
import it.imolinfo.jbi4corba.webservice.generator.bcm.ConstructorAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.IdlToWsdlAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.InterfaceTypeClassAdapter;
import it.imolinfo.jbi4corba.webservice.generator.bcm.WebServiceAnnotationAdapter;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDefUtil;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.cxf.helpers.ServiceUtils;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.util.CheckClassAdapter;
import org.objectweb.asm.util.TraceClassVisitor;



/**
 * This class provides the 'IDL to WSDL' code generation.
 */
@SuppressWarnings("unchecked")
public class ProviderServiceClassesGenerator {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(ProviderServiceClassesGenerator.class);
  
  private static final Messages MESSAGES = 
  	Messages.getMessages(ProviderServiceClassesGenerator.class);

  /**
   * The properties used to control the behavior during the code generation.
   */
  protected static CodeGenerationProperties codeGenerationProperties
    = new CodeGenerationProperties();

  //URL CLassLoader
  private URLClassLoader urlClassLoader;
  //URL Original
  private ChildFirstClassLoader originalClassLoader;
  
  /**
   *The Hashtable used for contain the generated classes  
   * Ther is a ClientCorbaClassesHolder 
   * String is the IDL File
   * HashTable is the HashTable that indexing classes by JbiDesc 
   */
  
  private static Hashtable<String, Hashtable<JbiServiceDescriptor,ClientCorbaClassesHolder>> 
          idlClassTable =new Hashtable<String, Hashtable<JbiServiceDescriptor,ClientCorbaClassesHolder>>();
  
   /**
    * A handle to the unique Singleton instance.
    */
   static private ProviderServiceClassesGenerator instance = null; 
  
  
  /**
   * Empty Constructor.
   */
  public ProviderServiceClassesGenerator() {   
  }

  /**
   * Empty Constructor.
   *
   * @param prop
   *    This object contains all the configuration to perform the activities.
   *
   */
  public ProviderServiceClassesGenerator(CodeGenerationProperties prop) {
    codeGenerationProperties = prop;
  }

 
  /**
   * This class is used to generate a service from a IDL file.
   *
   * Algorithm:
   * <br/>
   * - idl to java
   * <br/>
   * - provide value type implementation inspecting 'DefaultFactory' files.
   * <br/>
   * - compile java files.
   * <br/>
   * - bytecode manipulation:
   *   all the classes used in the 'xxxOperations.class' is visited.
   *   Where there is a non private attribute a getter/setter pair method
   *   will be added and all the array will be initialized to avoid
   *   NullPointerException.
   * <br/>
   * - return a list of holder classes.
   *
   * @param serviceDescriptor        Used to estract 'idlFileNameDirectory'
   *                                 and 'idlFileName'.
   *
   * @param workdir    The java source files and compiled files are placed
   *                     in 'workdir/src' and 'workdir/classes'
   *
   * @param libDirName The directory where the method looks for the jars.
   *
   * @return    The list of 'holder' classes containing the reference to
   *          corba object, helper and operations classes.
   *
   * @throws ClassGenerationException  The Class Generation Exception
   */
  public List<ClientCorbaClassesHolder> generateProviderServiceClasses(
          JbiServiceDescriptor serviceDescriptor,
          String workdir,
          String libDirName, String portTypeName) throws ClassGenerationException {
          
          List<JbiServiceDescriptor> serviceDescriptorList=new ArrayList<JbiServiceDescriptor>();
          List<String> portTypeNameList= new ArrayList<String>();
          
          serviceDescriptorList.add(serviceDescriptor);
          portTypeNameList.add(portTypeName);
          
         return generateProviderServiceClasses(serviceDescriptorList, workdir, libDirName, portTypeNameList);
           
  }
  
  
   public List<ClientCorbaClassesHolder> generateProviderServiceClasses(
         JbiServiceDescriptor serviceDescriptor,
          String workdir,
          List<String> jars, String portTypeName) throws ClassGenerationException {
   
   
          ArrayList<JbiServiceDescriptor> serviceDescriptorList=new ArrayList<JbiServiceDescriptor>();
          ArrayList<String> portTypeNameList= new ArrayList<String>();
          
          serviceDescriptorList.add(serviceDescriptor);
          portTypeNameList.add(portTypeName);
          
          return generateProviderServiceClasses(serviceDescriptorList, workdir, jars, portTypeNameList);
   
   }
  
  
  /**
   * This class is used to generate a service from a IDL file.
   *
   * Algorithm:
   * <br/>
   * - idl to java
   * <br/>
   * - provide value type implementation inspecting 'DefaultFactory' files.
   * <br/>
   * - compile java files.
   * <br/>
   * - bytecode manipulation:
   *   all the classes used in the 'xxxOperations.class' is visited.
   *   Where there is a non private attribute a getter/setter pair method
   *   will be added and all the array will be initialized to avoid
   *   NullPointerException.
   * <br/>
   * - return a list of holder classes.
   *
   * @param serviceDescriptor        Used to estract 'idlFileNameDirectory'
   *                                 and 'idlFileName'.
   *
   * @param workdir    The java source files and compiled files are placed
   *                     in 'workdir/src' and 'workdir/classes'
   *
   * @param libDirName The directory where the method looks for the jars.
   *
   * @return    The list of 'holder' classes containing the reference to
   *          corba object, helper and operations classes.
   *
   * @throws ClassGenerationException  The Class Generation Exception
   */
  public List<ClientCorbaClassesHolder> generateProviderServiceClasses(
          List<JbiServiceDescriptor> serviceDescriptorList,
          String workdir,
          String libDirName, List<String> portTypeName) throws ClassGenerationException   {
      
    
     portTypeName=new ArrayList<String>();
     for(int i=0;i<serviceDescriptorList.size();i++){
        portTypeName.add(serviceDescriptorList.get(i).getPortTypeName().getLocalPart());
     }
      
    LOG.debug(">>>>> generateProviderServiceClasses - begin");

    // ===================================
    //       find jar list used to compile
    // ===================================
    List<String> jars = null;
    LOG.debug("codeGenerationProperties.isValueTypeImplementationWithToStringAndEquals():" + codeGenerationProperties.isValueTypeImplementationWithToStringAndEquals());
    
    if (codeGenerationProperties.isValueTypeImplementationWithToStringAndEquals()) {
      jars = Util.prepareClassPath(libDirName);
    }
    
    // MARCO 22/09/2008: if  the jars is null, the jaxb jar location must be found!
    // With no jaxb jar in the classpath, the sources cannot be compiled.
    // With the jaxb jar file path, all the jar in the same directory are added.
    if (jars == null) {
                
        Class xmlAccessTypeClass=javax.xml.bind.annotation.XmlSeeAlso.class;
        if ((xmlAccessTypeClass != null) 
                && (xmlAccessTypeClass.getProtectionDomain() != null)
                && (xmlAccessTypeClass.getProtectionDomain().getCodeSource() != null)) { 
            URL jaxbJarLocation = xmlAccessTypeClass.getProtectionDomain().getCodeSource().getLocation();
            LOG.info("JAXB Location: " + jaxbJarLocation);
            try {
            LOG.info("JAXB Location to URI: " + jaxbJarLocation.toURI());
            } catch(URISyntaxException e) {}
            
            LOG.info("JAXB Location getPath: " + jaxbJarLocation.getPath());
            if ((jaxbJarLocation != null) && (jaxbJarLocation.getFile() != null)) {
                File jaxbLocationFile;
                // That's weird...but it seems the only way to safely
                // convert an URL to a File in a portable way 
                // (i mean a way that works only in Windows SO with path with spaces).
                // See CRB-171 Jira issue.
                try {
                	jaxbLocationFile = new File(jaxbJarLocation.toURI());
                } catch(URISyntaxException e) {
                	jaxbLocationFile = new File(jaxbJarLocation.getPath());
                } catch(IllegalArgumentException e) {     
                    // Can be thrown a java.lang.IllegalArgumentException by the File constructor.
                    jaxbLocationFile = new File(jaxbJarLocation.getPath());
                }

                //String jaxbLocationDirectory = jaxbLocationFile.getParent();
                //LOG.debug("jaxbLocationDirectory:" + jaxbLocationDirectory);
                jars = new ArrayList<String>();
                try {
					jars.add(jaxbLocationFile.getCanonicalPath());
				} catch (IOException e) {
					Object[] args = new Object[] { jaxbLocationFile, e.getMessage() };
					LOG.error("CRB000507_Error_getting_canonicalPath_from_file",
							args, e);
					throw new ClassGenerationException(
							"CRB000507_Error_getting_canonicalPath_from_file",
							args, e);
				}                
            }
        }
    }
 
    List<ClientCorbaClassesHolder> list = generateProviderServiceClasses(
            serviceDescriptorList,
            workdir,
            jars, 
            portTypeName);

    LOG.debug(">>>>> generateProviderServiceClasses - end");
    return list;
  }
  
  
  /**
     * This class is used to generate a service from a IDL file.
     * 
     * Algorithm: <br/> - idl to java <br/> - provide value type implementation
     * inspecting 'DefaultFactory' files. <br/> - compile java files. <br/> -
     * bytecode manipulation: all the classes used in the 'xxxOperations.class'
     * is visited. Where there is a non private attribute a getter/setter pair
     * method will be added and all the array will be initialized to avoid
     * NullPointerException. <br/> - return a list of holder classes.
     * 
     * @param serviceDescriptor Used to estract 'idlFileNameDirectory' and
     *                'idlFileName'.
     * @param workdir The java source files and compiled files are placed in
     *                'workdir/src' and 'workdir/classes'
     * @param jars The list of jars used to generate the classes.
     * @param portTypeName the porttype name (for the wsdl annotations). Can be null (in this case, the class name is used).
     * 
     * @return The list of 'holder' classes containing the reference to corba
     *         object, helper and operations classes.
     * 
     * @throws ClassGenerationException The class generation exception
     * 
     * The portTypeName is necessary for the generation of the classes
     */
  public List<ClientCorbaClassesHolder> generateProviderServiceClasses(
          List<JbiServiceDescriptor> serviceDescriptorList,
          String workdir,
          List<String> jars, List<String> portTypeName) throws ClassGenerationException {
          
    LOG.debug(">>>>> generateProviderServiceClasses - begin");
    
    boolean isRoleConsumer = (
    		serviceDescriptorList.size() > 0 && 
    		serviceDescriptorList.get(0).getRole() != null && 
    		serviceDescriptorList.get(0).getRole().equalsIgnoreCase(JbiServiceDescriptor.CONSUMER));

   
    String workdirsrc = workdir + "/src";
    String workdirclasses = workdir + "/classes";
    String workdirOrigclasses = workdir + "/origclasses"; 
    String serviceNameSpace = null;
    
    //The IdlFilenameDrectory and the IdlFileName is the Same for Each Interface
   
    String idlFilename = serviceDescriptorList.get(0).getIdlFileNameDirectory()
                       + File.separator
                       + serviceDescriptorList.get(0).getIdlFileName();
                       
    
    // =================================
    //                IDL to Java (idlj)
    // =================================
    
    if (isRoleConsumer) {
    	HelperIDLJUtil.idljPoaTie(workdirsrc,
    	        serviceDescriptorList.get(0).getIdlFileNameDirectory(),
    	        idlFilename);
    }
    else
    {
        HelperIDLJUtil.idlj(workdirsrc,
                serviceDescriptorList.get(0).getIdlFileNameDirectory(),
                idlFilename);
    }


    // =================================
    //       Collecting Method Signature
    // =================================
    Map<String, List<MethodSignature>> mapOperationsMethod
      = Util.extractMethodSignatureOfTheCorbaOperations(workdirsrc);
    
    // =================================
    //      VALUETYPE IDL IMPLEMENTATION
    // =================================
    List<String> vtList = Util.valueTypesImpl(workdirsrc,
      codeGenerationProperties.isValueTypeImplementationWithToStringAndEquals());
    LOG.debug("end creating java sources implementation for value types.");   
    
    // =================================
    //                       COMPILE 1/2
    // =================================
    List<String> javaSources = null;
    try {
      javaSources = Util.findJavaSources(workdir);
      LOG.debug("find java sources ... done");
    } catch (Jbi4CorbaException e) {
      Object[] args = new Object[] { workdir };

      LOG.error("CRB000522_Error_finding_java_classes", args, e);
      throw new ClassGenerationException(
              "CRB000522_Error_finding_java_classes", args, e);
    }

    Util.compileJavaClasses(workdirsrc,
                            workdirclasses,
                            javaSources,
                            jars,
                            null);

    LOG.debug("compileJavaClasses 1 of 2 ... done");

    // =================================
    //                        CORBA ENUM
    // =================================
    Map<String, List<String>> corbaEnumMap
      = Util.replaceCorbaEnumaration(workdirsrc, workdirclasses);

    // =================================
    //                       COMPILE 2/2
    // =================================
    javaSources = null;
    try {
      javaSources = Util.findJavaSources(workdir);
      LOG.debug("find java sources ... done");
    } catch (Jbi4CorbaException e) {
      Object[] args = new Object[] { workdir };

      LOG.error("CRB000522_Error_finding_java_classes", args, e);
      throw new ClassGenerationException(
              "CRB000522_Error_finding_java_classes", args, e);
    }
    
    // =================================
    // FOR EACH PACKAGE, ADDS THE PACKAGE-INFO.JAVA
    // =================================        
    List<String> packages = new ArrayList<String>();    
    List<String> filesCreated = new ArrayList<String>();
    LOG.debug("javaSources: "+javaSources);
    for (String source : javaSources) {
        // System Operation Linux
        String tempworkdirsrc = workdirsrc;
        if (System.getProperty("os.name").indexOf("Win") >= 0) {
            // System Operation Windows
            LOG.debug("System Operation [ Windows ]");
            //tempworkdirsrc = tempworkdirsrc.replace("/", File.separator);
            try {
                File myTempDir = new File(workdirsrc);
                tempworkdirsrc = myTempDir.getCanonicalPath();
                
            } catch (IOException ioe) {
                String msg = "Error in getting temp dir:" + ioe.getMessage();
                throw new ClassGenerationException(msg, ioe);
            }
        }
        // LOG.debug(">>>> tempworkdirsrc : "+tempworkdirsrc);
        int posPath = source.indexOf(tempworkdirsrc);
        
        String classSourceName = source.substring(posPath + tempworkdirsrc.length() + 1 , source.length());  
        
        String className = classSourceName.replace(File.separator, ".");
        String classNameMinusJava = className;
        if ((className.endsWith(".java"))) {
            classNameMinusJava = className.substring(0,className.length() - 5);            
        }
        String namespace = ServiceUtils.makeNamespaceFromClassName(classNameMinusJava, "http");        
        int lastSlashIndex = classSourceName.lastIndexOf(File.separator);
        String packageNameWithSlash = lastSlashIndex!= -1?classSourceName.substring(0,lastSlashIndex):classSourceName;
        if (packageNameWithSlash.startsWith(File.separator)){
            packageNameWithSlash=packageNameWithSlash.substring(1);
        }
        LOG.debug("packageNameWithSlash: "+packageNameWithSlash);
        String packageName =  packageNameWithSlash.replace(File.separator, ".");         
        if(classSourceName.equals(className)){
        	packageName="";
        }
         
        if (!packages.contains(packageName) && !packageName.equals("")) {
            String returnPath = createsPackageInfo(namespace, packageName, workdirsrc);
            filesCreated.add(returnPath);
            packages.add(packageName);
        }
    }
    javaSources.addAll(filesCreated);
    //Compile the class
    //We have to store a Copy of class in another folder for reuse it on runtime invokation
    //For Invoke Servant with correct parameter
    Util.compileJavaClasses(workdirsrc,
                            workdirclasses,
                            javaSources,
                            jars,
                            null);
    //Compile and produce the Original Class for the correct invocation of corba 
    //Servant
     Util.compileJavaClasses(workdirsrc,
                            workdirOrigclasses,
                            javaSources,
                            jars,
                            null);
     LOG.debug("compileJavaClasses 2 of 2 ... done");     
     
     // =================================
     // TYPEDEF identifications and classes creation. Collect all the
     // typedefs (identified by the Helper without interface class)
     // and then creates the class
     // =================================     
     // Creates the type def classes and gets the TypeDef data
     Map<String,TypeDef> typeDefs = null;
     try {
      	// Geth the typeDefs informations
     	typeDefs = TypeDefUtil.getTypeDefs(workdirclasses);
     	
     	if (LOG.isDebugEnabled()) {     
     		LOG.debug("TypeDefs found: " + typeDefs.size());
     		Iterator itTypeDefs = typeDefs.keySet().iterator();
     		while (itTypeDefs.hasNext()) {
     			LOG.debug("Found TypeDef:" + typeDefs.get(itTypeDefs.next()));
     		}
     	}
     } catch (Jbi4CorbaException e) {
         Object[] args = new Object[] { workdir };
         LOG.error("CRB000522_Error_creating_typedef_classes", args, e);
         throw new ClassGenerationException(
                 "CRB000522_Error_creating_typedef_classes", args, e);
     }        
     
     // =================================
     //           TYPEDEF CLASSES CREATION
     // =================================
  	Iterator itTypeDefs = typeDefs.keySet().iterator();
  	while (itTypeDefs.hasNext()) {
  		String typeDefKey = (String) itTypeDefs.next();
  		if (LOG.isDebugEnabled()) {
  			LOG.debug("Creating class for TypeDef:" + typeDefKey);
  		} 		
  		typeDefs.get(typeDefKey).createTypeDefClass(workdirclasses);  		
  		typeDefs.get(typeDefKey).createTypeDefClass(workdirOrigclasses);
  	}     

    // =================================
    //                 ONEWAY OPERATIONS
    // =================================
    Map<String, List<String>> onewayMap
      = Util.findOnewayOperations(workdirclasses);

    updateOnewayMethodSignature(onewayMap, mapOperationsMethod);

    // =================================
    //                       ANNOTATIONS
    // =================================    
    Set<String> exceptions = new HashSet<String>();
    
    
    //Each class holder is generated by an EndpointName an an NameSpace
    //The NameSpaces can be Different
    //This is The part when the List of interface's from a WSDL  a  is translated to single WSDL
    //*********************************************************************************************************
    // Union types
    Map<String, UnionType> allUnionTypes = new HashMap<String,UnionType>();
    
    // Interface Types 
    Map<String, InterfaceType> allInterfaceTypes = new HashMap<String,InterfaceType>();
       
    // Here ALL the types, used for ANY 
    Set<Class> allTypes = Util.findAllTypesUsedInIDL(workdirclasses);
                
    //Obtain all the id used in the idl Helper an the associated class
    Map<String,String> idToClassMap =Util.getTypesMap(workdirclasses);

    // Find all UnionTypes
    UnionTypeUtils unionUt = new UnionTypeUtils();
    unionUt.processTypes(allTypes, workdirclasses, allUnionTypes,TypeUtils.UNION);
    addUnionWrappers(allUnionTypes, workdirclasses);
    
    //Find all InterfaceType
    InterfaceTypeUtils iu = new InterfaceTypeUtils();
    iu.processTypes(allTypes, workdirclasses, allInterfaceTypes,TypeUtils.INTERFACE);

    //Add the Object type as interface type
    //Added to manage the Object type...
    allInterfaceTypes.put(org.omg.CORBA.Object.class.getName(), new InterfaceType(org.omg.CORBA.Object.class.getName()));
 
    for (String corbaOperation : mapOperationsMethod.keySet()) {
    	
        JbiServiceDescriptor serviceDescriptor =null;
        String portypeName=null;
    	
        
        // Map the correct Interface with the correct namespace's
        // The deploy regard's only a subset of interfaces , for the interface not binded 
        // tha namespace is generated by the idl as default namespace
        
        //Multiple interfaces
        if(serviceDescriptorList.size()>1){
        	//Take the portType name from the name of the interface
        	int start=corbaOperation.lastIndexOf(".")+1;
            int end=corbaOperation.lastIndexOf("Operations");

            String namespace="http://"+corbaOperation.substring(0, end);
            portypeName=corbaOperation.substring(start, end);

            for (JbiServiceDescriptor jbiserviceDescriptor : serviceDescriptorList){
        		
                //Added NamespaceCheck
                if(jbiserviceDescriptor.getPortTypeName().getNamespaceURI().equals("")){
                    if(jbiserviceDescriptor.getPortTypeName().getLocalPart().equals(portypeName)){
                        serviceDescriptor=jbiserviceDescriptor;
                        break;
                    }
                }else{
                    if(jbiserviceDescriptor.getPortTypeName().getNamespaceURI().equals(namespace) && jbiserviceDescriptor.getPortTypeName().getLocalPart().equals(portypeName)){
                        serviceDescriptor=jbiserviceDescriptor;
                        break;
                    }
                }

        	}
        
        }else{
        //Single interface
             serviceDescriptor=serviceDescriptorList.get(0);
             portypeName=portTypeName.get(0);
             
        }
        //This is possible when the the assembly contains only a subset of the entire interfaces declared in the IDL
        //Default namespace associations for the service descriptor ????
        if (serviceDescriptor==null){
        	serviceDescriptor=serviceDescriptorList.get(0);
            portypeName=portTypeName.get(0);
        	
        }
               
        serviceNameSpace = serviceDescriptor.getServiceNameSpace();  
        
        
       LOG.debug("JbiServiceDescriptor=" + serviceDescriptor
            + "; workdir="            + workdir
            + "; workdirsrc="         + workdirsrc
            + "; workdirclasses="     + workdirclasses
            + "; workdirorigclasses=" + workdirOrigclasses
            + "; idlFilename="        + idlFilename
            + "; serviceNameSpace="   + serviceNameSpace);
      
      //Add the Orignal Class Loader for the correct Service Invokation
   
      setOrginalClassLoader(workdirOrigclasses); 
      LOG.debug("Added OriginalClassLoader "+getOriginalClassLoader()); 
      // CALCULATE THE HOLDER VALUE TYPE FOR THE OPERATION. The classes dir is necessary to 
      // get the correct holder value class with introspection.
      List <MethodSignature> corbaOperationSignatures =  mapOperationsMethod.get(corbaOperation);
      
      //**********************************************************************
      // INOUT
      HolderUtils.populateCorbaHolderValueType(workdirclasses, corbaOperationSignatures, allUnionTypes.keySet(), allInterfaceTypes.keySet());
      //**********************************************************************
 
      
      //***************************************************************************
      //UnionTypes
      tweakUnionTypes(workdirclasses, allUnionTypes.values()); // replace modifier private with public for verifyXXX
      //*************************************************************************
      
       //********************************************************************** 
      
      //replace interface return type and parameter type with W3CEndpointReference
      if(allInterfaceTypes.size()>0) {
    	  tweakInterfacesTypes(workdirclasses,corbaOperation, allInterfaceTypes);
      }
      //*************************************************************************
   
      LOG.debug("Adding to portype: " + portypeName +" the namespace:" + serviceNameSpace);
      
      // Adds the Classes annotation
      
      Set<String> exceptionFromOperation = tweakCorbaOperation(corbaOperation,
                          workdirclasses,
                          mapOperationsMethod.get(corbaOperation), serviceNameSpace,portypeName, allUnionTypes,allInterfaceTypes, allTypes);
      exceptions.addAll(exceptionFromOperation);
      
    }          
    //*********************************************************************************************************
    
    // =================================
    //            BYTE CODE MANIPULATION
    // =================================
    Set<Class> cs = Util.findClassUsedInTheOperations(workdirclasses);
    //Util.debug("findClassUsedInTheOperations - result", cs);
    Map<String,Map<String, String>> allFieldsUniontypeMapping  = new HashMap<String, Map<String, String>>();
    //This map contain all- associations fieldname:Type for return correct type during RunTime
    Map<String,Map<String, String>> allFiledsInterfacetypeMapping  = new HashMap<String, Map<String, String>>();    
    
    if (cs == null || cs.size() == 0) {
        LOG.debug("No class to tweak.");
    } else {
        Iterator<Class> i = cs.iterator();
        while (i.hasNext()) {
          String fullname = i.next().getName();
          
          boolean skip = false;
          // Skips the IdlToWsdl bytecode manipulation if:
          // - It's a enum
          // IT's a typeDef (already generated)
          if ((corbaEnumMap.containsKey(fullname)) || (typeDefs.containsKey(fullname))) {
        	  skip = true;
          }
                    
          if (skip) {
            LOG.debug("(SKIPPED) tweaking for " + fullname);
          } else {
            LOG.debug("tweaking for " + fullname);

            String f = replaceDotWithSlash(fullname) + ".class";
            
            boolean isException = false;                       
            
            // Test if this clas is one of the exceptions
            if ((f.endsWith(".class"))) {
                String classNameMinusClass = f.substring(0,f.length() - 6);                
                if (exceptions.contains(classNameMinusClass)) {
                    isException = true;
                }                                
            }
            Map<String,String> fieldsUniontypeMapping  = new HashMap<String, String>();  
            Map<String,String> fieldsInterfacetypeMapping  = new HashMap<String, String>();  
            if(allInterfaceTypes.get(fullname)==null){
                //Bytecode Manipulation 
            	tweakIdlToWsdlClasses(f, workdirclasses, allUnionTypes, fieldsUniontypeMapping,allInterfaceTypes,fieldsInterfacetypeMapping, isException,false);
                tweakIdlToWsdlClasses(f, workdirOrigclasses, allUnionTypes, fieldsUniontypeMapping, allInterfaceTypes, fieldsInterfacetypeMapping, isException, true);
            	//change Parameters Type in the constructor Class 
            	if(allInterfaceTypes.size()>0){
            		tweakConstructor(f,workdirclasses,allInterfaceTypes, isException);
                }
            }
            allFieldsUniontypeMapping.put(f, fieldsUniontypeMapping);
            allFiledsInterfacetypeMapping.put(f,fieldsInterfacetypeMapping);
          }
        }

    }
          
    // replace union parameters/return type in the method signatures
    List<File> operationsClass
    = Util.findFilesFromSourceDirectory(workdirclasses, "Operations.class");
    for (int i = 0; i < operationsClass.size(); i++) {
        Class clazz = Util.classLoad(workdirclasses, operationsClass.get(i));
        
        tweakOperationsWithUnionTypes(workdirclasses, clazz, allUnionTypes);
        tweakOperationsWithAnyTypes(workdirclasses, clazz);
    }
    
    // =================================
    //                      FIND SERVICE
    // =================================
    List<ClientCorbaClassesHolder> generatedServices
        = findGeneratedServices(workdirclasses);

    LOG.debug("generatedServices=" + generatedServices);

	//The Value types id must be collected in the Orginal classloader
    Map<String, Object> map = Util.valueTypeMapHandler(
		 vtList, workdirclasses, getOriginalClassLoader());	
  
    // update the list with the map and with the method signature info
    for (ClientCorbaClassesHolder holder : generatedServices) {
      holder.setValueTypeIdAndInstance(map);
      //The Holder 
      
      List <MethodSignature> corbaOperationSignatures = mapOperationsMethod.get(holder.getOperationsClass().getName());
      holder.setMethodSignatures(corbaOperationSignatures);
      
      //add union types for operation
      holder.addUnionTypes(allUnionTypes);
      //store correspondance between substituted uniontypes with Object and uniontypes
      holder.setSubstitutedUnionFields(allFieldsUniontypeMapping);
      
       //add Interface types for operation
      holder.addInterfaceTypes(allInterfaceTypes);
      //store correspondance between substituted uniontypes with Object and uniontypes
      holder.setSubstitutedInterfaceFields(allFiledsInterfacetypeMapping);
      
      holder.setAllIDLTypes(allTypes);
      
      holder.setIdToClassMap(idToClassMap);
      
      holder.setCorbaEnumMap(corbaEnumMap);
      
      holder.setTypeDefs(typeDefs);
      
      
      // For each method, loads this actual <code>Method</code> object that can be changed (if holder are included)
      for (MethodSignature methodSignature: corbaOperationSignatures) {
          //**********************************************************************
          // INOUT
          HolderUtils.populateChangedMethod(methodSignature, workdirclasses, allUnionTypes.keySet(), allInterfaceTypes.keySet());
          
          //**********************************************************************                    
          
      }
    }

    LOG.debug("<<<<<<<<<< generateProviderServiceClasses - end");
    return generatedServices;
  }
  
  
  
  /**
   * addUnionWrappers 
   * Create wrapper classes for union types
   * 
   * @param operationUnionTypes
   * @param workdirclasses
   * @throws ClassGenerationException
   */
  private void addUnionWrappers(Map<String, UnionType> operationUnionTypes,
		String workdirclasses) throws ClassGenerationException {

	
	for (Entry<String, UnionType> entry : operationUnionTypes.entrySet())
	{
		UnionType union = entry.getValue();
		
		UnionTypeUtils.createUnionClassWrapper(union, operationUnionTypes, workdirclasses);
	}
	
}

/**
   * tweakConstructor
   * 
   * This method uses ASM to change the the return type in class with method that return an interface: 
   * and take as parameters an Interface 
   * 
   * @author Luca Acquaviva
   */
   private String tweakConstructor(
            String className,
            String classesDirName, 
            Map<String, InterfaceType> allInterfaceTypes, boolean isException) throws ClassGenerationException {

           	

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        
        ConstructorAdapter cv = new ConstructorAdapter(tv, cw, className,allInterfaceTypes, isException);

        ClassReader cr = Util.getAsmCLassReader(classesDirName, className);

        
        cr.accept(cv, true);

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place

        String relativeFileName = className.replace('/', File.separatorChar);

        Util.saveAsJavaClass(classesDirName, relativeFileName, newBytecode);
        
        return className.replace('/', '.');
    }
   /**
   * tweakInterfacesTypes
   * 
   * This method uses ASM to change the the return type in class with method that return an interface: 
   * and take as parameters an Interface 
   * @param workdirclasses directory that contain the source class	
   * @param Operation the class to tweak
   * @param The map that contains all the type identified as Interface
   * 
   * @author Luca Acquaviva
   */
  private void tweakInterfacesTypes(String workdirclasses,String Operation, Map<String, InterfaceType> opIntTypes) throws ClassGenerationException {

	        
	        ClassWriter  cw = new ClassWriter(true); // visitMaxs
		    ClassVisitor cc = new CheckClassAdapter(cw);
		  
		    InterfaceTypeClassAdapter clsa = new InterfaceTypeClassAdapter(cc, opIntTypes);
		    
		    String absPath = workdirclasses + File.separator
		      + Operation.replace('.', File.separatorChar) + ".class";

		    ClassReader cr = Util.getAsmCLassReader(absPath);

		    cr.accept(clsa, true);  

		    byte [] newBytecode = cw.toByteArray();
		    
		    Util.saveAsJavaClass(absPath, newBytecode);
		
}
    
  /**
     * This class is used to generate a service from a IDL file.
     * 
     * Algorithm: <br/> - idl to java <br/> - provide value type implementation
     * inspecting 'DefaultFactory' files. <br/> - compile java files. <br/> -
     * bytecode manipulation: all the classes used in the 'xxxOperations.class'
     * is visited. Where there is a non private attribute a getter/setter pair
     * method will be added and all the array will be initialized to avoid
     * NullPointerException. <br/> - return a list of holder classes.
     * 
     * @param serviceDescriptor Used to estract 'idlFileNameDirectory' and
     *                'idlFileName'.
     * @param workdir The java source files and compiled files are placed in
     *                'workdir/src' and 'workdir/classes'
     * @param jars The list of jars used to generate the classes.
     * @param portTypeName the porttype name (for the wsdl annotations). Can be null (in this case, the class name is used).
     * 
     * @return The list of 'holder' classes containing the reference to corba
     *         object, helper and operations classes.
     * 
     * @throws ClassGenerationException The class generation exception
     */
    
 

/**
   * tweakUnionTypes
   * 
   * This method uses ASM to change the union types classes: replace modifier 'private' to 'public'
   * of method verifyXXX where XXX represents the fields of the union types 
   * 
   */
  private void tweakUnionTypes(String workdirclasses, Collection<UnionType> operationUnionTypes) throws ClassGenerationException {

	  for ( UnionType union : operationUnionTypes)
		{
			
			ClassWriter  cw = new ClassWriter(true); // visitMaxs
		    ClassVisitor cc = new CheckClassAdapter(cw);
		    
		    UnionTypeClassAdapter clsa = new UnionTypeClassAdapter(cc, union.getTypeFieldNameList());
		    
		    String absPath = workdirclasses + File.separator
		      + union.getTypeName().replace('.', File.separatorChar) + ".class";

		    ClassReader cr = Util.getAsmCLassReader(absPath);

		    cr.accept(clsa, true);  

		    byte [] newBytecode = cw.toByteArray();

		    Util.saveAsJavaClass(absPath, newBytecode);
		}
}
  
  /**
   * tweakOperationsWithUnionTypes
   * 
   * This method uses ASM to create uniontype wrappers for operations parameters/return type 
   * 
   */
  private void tweakOperationsWithUnionTypes(String workdirclasses, Class operationClass, Map<String, UnionType> allUnionTypes) throws ClassGenerationException {

			ClassWriter  cw = new ClassWriter(true); // visitMaxs
		    ClassVisitor cc = new CheckClassAdapter(cw);
		    
		    OperationWithUnionTypesAdapter clsa = new OperationWithUnionTypesAdapter(cc, allUnionTypes, workdirclasses);
		    
		    String absPath = workdirclasses + File.separator
		      + operationClass.getName().replace('.', File.separatorChar) + ".class";

		    ClassReader cr = Util.getAsmCLassReader(absPath);

		    cr.accept(clsa, true);  

		    byte [] newBytecode = cw.toByteArray();

		    Util.saveAsJavaClass(absPath, newBytecode);
	
}

  /**
   * tweakOperationsWithAnyTypes
   * 
   * @param clazz
 * @throws ClassGenerationException 
   */  
private void tweakOperationsWithAnyTypes(String workdirclasses, Class operationClass) throws ClassGenerationException {

	    ClassWriter  cw = new ClassWriter(true); // visitMaxs
	    ClassVisitor cc = new CheckClassAdapter(cw);
	    
	    OperationWithAnyTypesAdapter clsa = new OperationWithAnyTypesAdapter(cc);
	    
	    String absPath = workdirclasses + File.separator
	      + operationClass.getName().replace('.', File.separatorChar) + ".class";

	    ClassReader cr = Util.getAsmCLassReader(absPath);

	    cr.accept(clsa, true);  

	    byte [] newBytecode = cw.toByteArray();

	    Util.saveAsJavaClass(absPath, newBytecode);
		
	}
  
/**
   * Creates the pacakge-info.java file for the correct jaxb mapping.
   * @param namespace
   * @param packagePath
   * @param dirClasses
   */
  public String createsPackageInfo(String namespace, String packagePath, String dirClasses) throws ClassGenerationException {
           
      String returnPath = null;
      String packageInfoJavaFileName=null;
      if(!packagePath.equals("")){
    	  packageInfoJavaFileName = dirClasses + File.separator + packagePath.replace(".", File.separator) + File.separator + "package-info.java";      
      }else{
    	  packageInfoJavaFileName = dirClasses  + File.separator + "package-info.java";
      }
      LOG.debug("Creating the package-info.java file into the directory: " + packageInfoJavaFileName);      
      File packageInfoJavaFile = new File(packageInfoJavaFileName);
      StringBuffer str = new StringBuffer();
      str.append("@javax.xml.bind.annotation.XmlSchema(namespace=\"")
      .append(namespace)
      .append("\"")
      .append(",attributeFormDefault=javax.xml.bind.annotation.XmlNsForm.QUALIFIED")
      .append("," +
      		"elementFormDefault=javax.xml.bind.annotation.XmlNsForm.QUALIFIED)")      		
      .append("\n")
      .append("package ")
      .append(packagePath)
      .append(";");
      try {
    	  
        packageInfoJavaFile.createNewFile();
        FileWriter fout = new FileWriter(packageInfoJavaFile);
        fout.write(str.toString());
        fout.flush();
        fout.close();
        returnPath = packageInfoJavaFile.getCanonicalPath();
    } catch (IOException e) {
    	String msg=MESSAGES.getString("CRB000561_Error_creating_package_info_file", 
    			new Object[] {e.getMessage()});
        LOG.error(msg,e);
        throw new ClassGenerationException(msg,e);

    }
    return returnPath;      
  }
 

  /**
   * This method is used to update the 'oneway' property
   * of the method signature.
   *
   * @param onewayMap
   *            A map where the key is a full java class name
   *            and the value is a list of oneway methods.
   *
   * @param mapOperationsMethod
   *            A map where the key is a full java class name
   *            and the value is a list method signatures. 
   */
  protected void updateOnewayMethodSignature(
    Map<String, List<String>> onewayMap,
    Map<String, List<MethodSignature>> mapOperationsMethod) {

    Set<String> onewayClasses = onewayMap.keySet();

    // for each class that contains one or more oneway methods ...
    for (String onewayClass : onewayClasses) {
      // ... extracting the list of method's signature associated
      List<MethodSignature> methodSignatureList
         = mapOperationsMethod.get(onewayClass);
      LOG.debug("onewayClass=" + onewayClass
        + " => methodSignatureList=" + methodSignatureList);

      // ... extracting the list of the oneway methods
      List<String> onewayMethodList = onewayMap.get(onewayClass);

      // for each oneway method find the associated method signature to update
      // the oneway property.
      for (String onewayMethod : onewayMethodList) {
        boolean ok = setOnewayPropertyOnTheMethodSignature(onewayMethod,
                                                           methodSignatureList);
        if (! ok) {
          Object [] args = new Object[] {onewayMethod};
          LOG.warn("CRB000547_OnewayMethodNotFound", args);
        }
      }

    }
  }

  /**
   * This method searches the right signature
   * and set to true the 'oneway' property.
   *
   * @param   onewayMethod
   *              The name of the oneway method.
   * @param   methodSignatureList
   *              The list of the method's signature to inspect
   *
   * @return  false when the oneway method is not found in the list.
   */
  protected boolean setOnewayPropertyOnTheMethodSignature(
    String onewayMethod, List<MethodSignature> methodSignatureList) {

    if (onewayMethod == null || methodSignatureList == null) {
      LOG.warn("CRB000548_NullParameter");
      return false;
    }

    boolean found = false;

    for (MethodSignature methodSignature : methodSignatureList) {

      /* The IDL doesn't support the overloading of a method.
       */
      if (methodSignature.getMethodName().equals(onewayMethod)) {
        found = true;
        methodSignature.setOneway(true);
      }
    }

    LOG.debug("oneway method found:" + found);
    return found;
  }
  
  
  /**
     * Adds the jsr-181 annotations for the parameter and the class.
     * 
     * @param qualifiedJavaName The qualified java name
     * @param dir The dir
     * @param methodSignatureList The method signature list
     * 
     * @return the Set of thrown exceptions
     * 
     * @throws ClassGenerationException The class generation exception
     */
  private Set<String> tweakCorbaOperation(String qualifiedJavaName, String dir,
    List<MethodSignature> methodSignatureList, String nameSpace, String portTypeName, Map<String, UnionType> unionTypes, Map<String, InterfaceType> interfaceTypes, Set<Class> allTypes) throws ClassGenerationException {

    ClassWriter  cw = new ClassWriter(true); // visitMaxs
    ClassVisitor cc = new CheckClassAdapter(cw);
    StringWriter sw = new StringWriter();
    ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

    // ADds oals
    WebServiceAnnotationAdapter cv
      = new WebServiceAnnotationAdapter(tv, cw, methodSignatureList, nameSpace, portTypeName, dir, unionTypes,interfaceTypes, allTypes);  

    String absPath = dir + File.separator
      + qualifiedJavaName.replace('.', File.separatorChar) + ".class";

    ClassReader cr = Util.getAsmCLassReader(absPath);

    cr.accept(cv, true);
    LOG.debug("ClassReader.accept ... done");

    LOG.debug("output of tracer during creation of class: " + absPath + "\n"
      + sw.toString());

    byte [] newBytecode = cw.toByteArray();

    Util.saveAsJavaClass(absPath, newBytecode);
    
    return cv.getExceptionsThrown(); 
  }

    /**
     * Bytecode manipulation. Adding the getter and setter methods for all the
     * public members. Initialize all the array. Add the methods toString() and
     * equals(Object).
     * 
     * @param className The class to manipulate.
     * @param classesDirName The directory where the class is located.
     * @param isException if the class is an exception
     * @param excludeType if The Type Any,Interface,Union BytecodeManipulatios has to be disabled  
     * 
     * @return The full class name.
     * 
     * @throws ClassGenerationException The class generation exception
     */
    private String tweakIdlToWsdlClasses(
            String className,
            String classesDirName, Map<String, UnionType> allUnionTypes, Map<String,String> fieldsUniontype,Map<String, InterfaceType> allInterfaceTypes, Map<String,String> fieldsInterfacetype, boolean isException,boolean excludeType) throws ClassGenerationException {

           	LOG.debug("CRB000562_tweakIdlToWsdlClasses_begin");

    		LOG.debug("CRB000563_tweakIdlToWsdlClasses_class_in_dir", 
        		new Object[]{className, classesDirName});

        ClassWriter  cw = new ClassWriter(true); // visitMaxs
        ClassVisitor cc = new CheckClassAdapter(cw);
        StringWriter sw = new StringWriter();
        ClassVisitor tv = new TraceClassVisitor(cc, new PrintWriter(sw));

        
        IdlToWsdlAdapter cv = new IdlToWsdlAdapter(tv, cw, className, allUnionTypes, fieldsUniontype,allInterfaceTypes,fieldsInterfacetype, isException,excludeType);

        ClassReader cr = Util.getAsmCLassReader(classesDirName, className);

        LOG.debug("getAsmCLassReader ... done");

        cr.accept(cv, true);
        LOG.debug("ClassReader.accept ... done");

        LOG.debug("output of tracer during creation of class: "
                + className + "\n" + sw.toString());

        byte [] newBytecode = cw.toByteArray();

        // write class in the right place

        String relativeFileName = className.replace('/', File.separatorChar);

        Util.saveAsJavaClass(classesDirName, relativeFileName, newBytecode);

        
        
        LOG.debug("<<<<<<<<<< tweakIdlToWsdlClasses - end");
        return className.replace('/', '.');
    }

    // =================================
    //                         Utilities
    // =================================

    /**
    * This method replaces the dot with a '/'.
    *
    * @param s  The working path.
    *
    * @return   The new string.
    */
    private String replaceDotWithSlash(String s) {
        LOG.debug("replaceDotWithSlash. the input is " + s);
        if (s == null) {
            LOG.debug("replaceDotWithSlash. "
                    + "the input is null. returning empty String");
            return "";
        }
        // else
        if ("".equals(s)) {
            LOG.debug("replaceDotWithSlash. "
                    + "the input is an empty String. returning empty String");
            return "";
        }
        // else
        String res = s.replaceAll("\\.", "/");
        LOG.debug("replaceDotWithSlash. "
                + "the input is " + s + " returning " + res);
        return res;
    }

    /**
    * This method find the classes with all the following properties:
    * - is an interface;
    * - is a org.omg.CORBA.Object;
    * - exists an associated Helper class;
    * - exists an associated Operations class.

    * @param classesDir    The directory to inspect.
    *
    * @return    The result.
    *
    * @throws ClassGenerationException  The class generation exception
    */
    private List<ClientCorbaClassesHolder> findGeneratedServices(
            String classesDir) throws ClassGenerationException {

        LOG.debug(">>>>>>>>>> findGeneratedServices - begin");
       
        urlClassLoader = null;
        String protocol = null;
        try {

            if (System.getProperty("os.name").indexOf("Win") >=0) {
                protocol = "file:///";
            } else {
                protocol = "file://";
            }

            File fcd = new File(classesDir);
            LOG.debug("ClassesDir.getAbsolutePath=" + fcd.getAbsolutePath());

            urlClassLoader = new URLClassLoader(
                new URL[] { new URL(protocol + fcd.getAbsolutePath()  + "/") },
                this.getClass().getClassLoader());
            
          

            LOG.debug("url classloader: "
                    + Arrays.asList(urlClassLoader.getURLs()));
        } catch (MalformedURLException e) {
            Object[] args = new Object[] {
                    protocol + new File(classesDir).getAbsolutePath() + "/" };

            LOG.error("CRB000526_Could_not_instantiate_the_url_class_loader",
                      args, e);
            throw new ClassGenerationException(
                    "CRB000526_Could_not_instantiate_the_url_class_loader",
                    args, e);
        }

        LOG.debug("classes dir: " + classesDir);

        List<Class> generatedClasses
            = Util.findGeneratedClasses(classesDir, urlClassLoader);

        LOG.debug("generated classes: " + generatedClasses);
        for (int i = 0; i < generatedClasses.size(); i++) {
          LOG.debug("CRB000564_Generated_class", 
        			new Object[] {generatedClasses.get(i)});
        }

        LOG.debug("<<<<<<<<<< findGeneratedServices - end");
        return filterInterfaceClasses(generatedClasses, urlClassLoader);
        // return filterInterfaceClasses(generatedClasses, originalClassLoader);
    }
    
    private void setOrginalClassLoader(String origclassesDir) throws ClassGenerationException{
    	 originalClassLoader=null;
    	 String protocol = null;
    	 try {

             if (System.getProperty("os.name").indexOf("Win") >=0) {
                 protocol = "file:///";
             } else {
                 protocol = "file://";
             }

             File fcd = new File(origclassesDir);
             LOG.debug("ClassesDir.getAbsolutePath=" + fcd.getAbsolutePath());
             
             originalClassLoader=new ChildFirstClassLoader(new URL[] { new URL(protocol + fcd.getAbsolutePath()  + "/") },
					   this.getClass().getClassLoader());
             
                          
             LOG.debug("url classloader: "
                     + Arrays.asList(originalClassLoader.getURLs()));
         } catch (MalformedURLException e) {
             Object[] args = new Object[] {
                     protocol + new File(origclassesDir).getAbsolutePath() + "/" };

             LOG.error("CRB000526_Could_not_instantiate_the_class_loader",
                       args, e);
             throw new ClassGenerationException(
                     "CRB000526_Could_not_instantiate_the_class_loader",
                     args, e);
         }
    	 // Util.findGeneratedClasses(origclassesDir, originalClassLoader);
    	
    }

    /**
    * This method filters the list in input leaving only the classes with all
    * the following properties:
    * - is an interface;
    * - is a org.omg.CORBA.Object;
    * - exists an associated Helper class;
    * - exists an associated Operations class.
    *
    * @param    classes            The list to filter.
    * @param    classLoader        Used to instantiate the associated classes.
    *
    * @return    The list of the filtered classes.
    */
    private List<ClientCorbaClassesHolder> filterInterfaceClasses(
            List<Class> classes,
            ClassLoader classLoader) {

        LOG.debug(">>>>>>>>>> filterInterfaceClasses - begin");

        List<ClientCorbaClassesHolder> result
                = new ArrayList<ClientCorbaClassesHolder>();

        for (Class clazz : classes) {

            // clazz is included if and only if is an interface and extends
            // org.omg.CORBA.Object
            if (clazz.isInterface()
                    && org.omg.CORBA.Object.class.isAssignableFrom(clazz)
                    && (! clazz.getName().startsWith("org.omg"))) {

                /* IF (EXISTS clazzHelper) AND (EXISTS clazzOperations)
                * THEN
                *   list.add( {clazz, clazzHelper, clazzOperations} )
                * ELSE
                *   <<no operations to do>>
                */
                try {
                    LOG.debug("trying if [" + clazz.getName()
                            + "Helper] exists");
                    Class helperClass
                            = classLoader.loadClass(clazz.getName() + "Helper");

                    LOG.debug("trying if [" + clazz.getName()
                            + "Operations] exists");
                    Class operationsClass = classLoader.loadClass(
                            clazz.getName() + "Operations");

                    LOG.debug("adding " + clazz.getName()
                            + " to service interfaces");

                    ClientCorbaClassesHolder corbaClasses
                        = new ClientCorbaClassesHolder();

                    corbaClasses.setCorbaObjectClass(clazz);
                    corbaClasses.setOperationsClass(operationsClass);
                    corbaClasses.setHelperClass(helperClass);                    

                    LOG.debug("result.add - corbaClasses=" + corbaClasses);
                    result.add(corbaClasses);
                } catch (ClassNotFoundException e) {
                    String className = clazz.getName();

                    LOG.error("CRB000527_Class_Helper_or_Operations_not_found",
                              new Object[] { className, className }, e);
                }
            } else {
            	LOG.debug("CRB000565_Class_not_referred_to_corba_client", 
                		new Object[]{clazz.getName()});
            }
        }
        LOG.debug("<<<<<<<<<< filterInterfaceClasses - end. result:" + result);
        return result;
    }

  

  /**
   * @return  the return
   */
    public URLClassLoader getUrlClassLoader() {
        return urlClassLoader;
    }
    
   /**
   * @return  the return
   */
    public ChildFirstClassLoader getOriginalClassLoader() {
        return originalClassLoader;
    }

/**
 * @param workdirsrc                 The work dir src
 * @throws ClassGenerationException  The class generation exception
 */
  protected void addEnumClass(String workdirsrc) throws ClassGenerationException {
    String enumFileName
      = "/it/imolinfo/jbi4corba/test/webservice/generator/EchoComplexEnum.java";

    String filename = workdirsrc + enumFileName;

    String enumSource
      = "package it.imolinfo.jbi4corba.test.webservice.generator; "
      + "\n\n"
      + "public enum EchoComplexEnum implements org.omg.CORBA.portable.IDLEntity { "
      + "\n\n"
      + "E1, E2, E3;"
      + "\n\n"
      + "public int value() {return ordinal();} "
      + "\n\n"
      + "public static EchoComplexEnum from_int (int value) { return EchoComplexEnum.values()[value]; } "
      + "\n\n"
      + "}"
      ;

    try {
      PrintWriter pw
        = new PrintWriter(new BufferedWriter(new FileWriter(filename)));
      pw.println(enumSource);
      pw.flush();
      pw.close();
    } catch (IOException e) {
    	String msg=MESSAGES.getString("CRB000566_Failure_in_stopping_endpoint", 
    			new Object[] {e.getMessage()});
        LOG.error(msg,e);
        throw new ClassGenerationException(msg,e);

    }
  }
  

  
  
}
