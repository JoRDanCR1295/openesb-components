package it.imolinfo.jbi4corba.utils;

import it.imolinfo.jbi4corba.utils.plugin.wsdl.Jbi4CorbaPartnerLink;
import it.imolinfo.jbi4corba.utils.plugin.wsdl.Jbi4CorbaPartnerLinkExtension;
import it.imolinfo.jbi4corba.utils.plugin.wsdl.Role;
import it.imolinfo.jbi4corba.schema.DefinitionAndSchema;
import it.imolinfo.jbi4corba.webservice.generator.WSDLDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.WSDLGenerator;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import com.ibm.wsdl.util.xml.DOMUtils;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtension;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.PrintStream;
import java.util.Date;
import java.util.Properties;
import java.util.Vector;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;

/**
 * MainCommandLine - class with the main method
 * @author Federico Pavoncelli
 */
public class IDL2WSDLTool {
    
    
        /**
     * The Column Number that contain the number of Interface Selected for Creation
     */
    private final static int SELECTED_FOR_CREATION=0;
    
         /**
     * The Column that contain the Endpoint Name
     */
    private final static int ENDPOINT_NAME=1;
    
     /**
     * The Column that contain the descriptor info
     */
    private final static int DESC_INFO=3; 
    
    private final static String LOG_FILE_NAME="IDL2WSDLTool.log";
    
    private final static String LINE_SEPARATOR="-----------------------------------------------------------";
 
    private static PrintStream pos = null;
    
    public static void main(String[] args) {
        
        
        //args=new String[2];
        // testAny
        // AlarmIRPSystem
        //args[0]="AlarmIRPSystem.idl";
        //args[0]="C:/Users/fedeb/AppData/Local/Temp/jbi4corba.properties";
        //args[1]="C:/Users/fedeb/AppData/Local/Temp/jbi4corba.properties";

        if ((args==null)||(args.length!=2)) {
            logConsole("Two arguments are required: idl_file_name property_file_name");
        }
        
        String fn =null;
        
        File work = new File(args[0]);
        try {
            fn = work.getCanonicalPath();
        } catch (IOException e) {
            logConsole("File idl not found");
        }
        /*
        File logDir = new File (fn);
        String logFile;
        try {
            logFile = logDir.getParentFile().getCanonicalPath();
            FileOutputStream fos = new FileOutputStream(logFile+File.separator+LOG_FILE_NAME,false);
            pos = new PrintStream(fos);
            pos.println("WSDL generation started at "+new Date());
        }  catch (IOException ex) {
            ex.printStackTrace();
            System.exit(-1);
        }  
       */

        final String baseTempPath = System.getProperty("java.io.tmpdir");
        try {
            String fileNameAbsolute = new File(baseTempPath).getCanonicalPath()+File.separator+LOG_FILE_NAME;
            FileOutputStream fos = new FileOutputStream(fileNameAbsolute,false);
            pos = new PrintStream(fos);
            pos.println("WSDL generation started at "+new Date());
            pos.println(LINE_SEPARATOR);
            pos.println("Arg0 "+args[0] );
            pos.println("Arg1 "+args[1] );
            pos.println(LINE_SEPARATOR);
        }  catch (IOException ex) {
            ex.printStackTrace();
            System.exit(-1);
        } 
        
        
        Properties properties = new Properties();
        try {
            properties.load(new FileInputStream(args[1]));
        } catch (IOException e) {
            logConsole("Property file not found",e);
        }
           
        String num = properties.getProperty("InterfaceCount");
        
        if (num==null) {
              logConsole("InterfaceCount property not found");
        }
        int iNum =0;
        try {
            iNum = Integer.parseInt(num);
            if (iNum<1) throw new NumberFormatException();
        } catch (NumberFormatException nfe) {
            logConsole("Property iNum must be a number greater than 0");
        }

        Object[][] descInfo = new Object[iNum][4];

        for (int i = 0; i < iNum; i++) {
            String PRE = "Interface" + i + ".";
            
            String IDLInterfaceName = properties.getProperty(PRE + "IDLInterfaceName");
            String fileName = properties.getProperty(PRE + "FileName");
            String address = properties.getProperty(PRE + "Address");
            String localizationType = properties.getProperty(PRE + "LocalizationType");
            
            if (IDLInterfaceName==null) {
                logConsole("IDLInterfaceName property not found");
            } else if (fileName==null) {
                logConsole("FileName property not found");
            } else if (address==null) {
                logConsole("Address property not found");
            } else if (localizationType==null) {
                logConsole("LocalizationType property not found");
            }
         
            descInfo[i][0] = Boolean.TRUE;
            descInfo[i][1] = IDLInterfaceName;
            descInfo[i][2] = fileName;

            DescInfo di = new DescInfo();
            di.setAddress(address);
            di.setLocalizationType(localizationType);
            //int val = Integer.parseInt(properties.getProperty(PRE + "ORBType"));
            //di.setORBType(val);

            Vector prop = new Vector();
            
            String ORBPropertiesCount = properties.getProperty(PRE + "ORBPropertiesCount");
            int nProp = 0;
            if (ORBPropertiesCount!=null) {
                try {
                    nProp = Integer.parseInt(ORBPropertiesCount);
                    if (nProp<1) throw new NumberFormatException();
             
                } catch (NumberFormatException nfe) {
                    logConsole("ORBPropertiesCount property must be a number greater than 0");
                }

                for (int z = 0; z < nProp; z++) {
                    Object[] arr = new Object[2];

                    String name = properties.getProperty(PRE + "ORBPropertyName" + z);
                    String value = properties.getProperty(PRE + "ORBPropertyValue" + z);
                    
                    if (name==null) {
                      logConsole("ORBPropertyName property not found");
                    }
                    if (value==null) {
                      logConsole("ORBPropertyValue property not found");
                    }
                    arr[0] = name;
                    arr[1] = value;
                    prop.add(arr);
                }

                di.setProperties(prop);
                descInfo[i][3] = di;
            }

        }
        // start generating WSDL

        File idl = new File(fn);
        List<IdlFileDataHolder> idldata = loadInterfaces(idl);
        ArrayList<WSDLDescriptor> descList = createWSDLDescList(descInfo, idldata);

        WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
        try {

            //****Component Side*****
            List<Definition> defList = null;
            defList = generator.generateWSDLListfromIDL(idl, descList);



            DefinitionAndSchema defSchema = generator.createRemoveXSD(defList);
            int index = 0;

            //WSDL Creation For Each Definition

            for (Definition def : defSchema.getDefinitions()) {


                String nameFile = def.getQName().getLocalPart();
                String wsdlFileName = idl.getParent() + File.separatorChar + nameFile + ".wsdl";

                File wsdl = new File(wsdlFileName);

                //Change File Name for correct generation
                int i = 1;
                while (wsdl.exists()) {

                    wsdlFileName = idl.getParent() + File.separatorChar + nameFile + "_" + i + ".wsdl";
                    wsdl = new File(wsdlFileName);
                    i++;
                }

                // adding the partner link to the generated file
                if (def != null) {
                    def = addPartnerLinkToWSDL(def, def.getQName().getLocalPart());
                    //def
                    createFileFromWSDL(def, wsdl);
                }

            }

            //System.err.println("efSchema.isContainsW3c():" + defSchema.isContainsW3c());
            //System.err.println("idl.getParent():" + idl.getParent());



            //Write XSD
            if (defSchema.isContainsW3c()) {
                it.imolinfo.jbi4corba.schema.SchemaUtil.createW3CSchema(idl.getParent());
            }

            //System.err.println("defSchema.getSchemas().size();:" + defSchema.getSchemas().size());
            //System.out.println("WSDL successfully generated at "+new Date());
            
        
            it.imolinfo.jbi4corba.schema.SchemaUtil.createXSD(defSchema.getSchemas(), idl.getParent(), "TypeDef");


        //} finally {
        //    Thread.currentThread().setContextClassLoader(oldCl);
        //}

        } catch (Exception ex) {
            logConsole("Internal Error :  "+ex.getMessage(), ex);

        } 
        pos.println("WSDL successfully generated at "+new Date());
        pos.close();
        System.exit(0);
    }


    /**
   * Adds the deafult partner-link to the wsdl
   */
  private static Definition addPartnerLinkToWSDL(Definition definition, String corbaServiceName) throws WSDLException, IOException {


    Jbi4CorbaPartnerLink ptnlnk = new Jbi4CorbaPartnerLink();

    // check if name space is present
    String prefix = definition.getPrefix(Jbi4CorbaPartnerLinkExtension.NS_PTNLNK);
    if (prefix == null) {
      // add the namespace
      definition.addNamespace(Jbi4CorbaPartnerLinkExtension.NS_PREFIX, Jbi4CorbaPartnerLinkExtension.NS_PTNLNK);
      prefix = Jbi4CorbaPartnerLinkExtension.NS_PREFIX;
    }

    // FEDE TODO
    //log.fine("The used prefix: " + prefix);

    // use the existing prefix
    ptnlnk.setPrefix(prefix);

    // set the partner link name as the corba service name
    ptnlnk.setName(corbaServiceName);

    // set the required to true
    ptnlnk.setRequired(true);

    // set the element type
    ptnlnk.setElementType(Jbi4CorbaPartnerLinkExtension.Q_ELEMENT_PTNLNK);

    // isolating the portType
    Map portTypes = definition.getAllPortTypes();
    Role role = null;
    if (portTypes.size() > 0) {
      Iterator iterKeys = portTypes.keySet().iterator();
      Iterator iterValues = portTypes.values().iterator();
      do {
        QName key = (QName) iterKeys.next();
        javax.wsdl.PortType value = (javax.wsdl.PortType) iterValues.next();

        // find the target name space
        String tns = value.getQName().getNamespaceURI();
        if (tns == null) {
          // Impossible: the tns must be defined
          // FEDE TODO
          //String msg = NbBundle.getMessage(CreateWSDLAction.class, "MSG_TargetNamespaceNotFound");
          //NotifyDescriptor nd = new NotifyDescriptor.Message(msg, NotifyDescriptor.ERROR_MESSAGE);


          //DialogDisplayer.getDefault().notify(nd);
          logConsole("Errore: MSG_TargetNamespaceNotFound");
          return null;
        }
        role = new Role(corbaServiceName + Jbi4CorbaPartnerLinkExtension.ROLE_NAME_SUFFIX, DOMUtils.getPrefix(tns, definition) + ":" + value.getQName().getLocalPart());
        ptnlnk.addRole(role);
        portTypes.remove(key);
      } while (!portTypes.isEmpty());
    }

    // adds the extension
    definition.addExtensibilityElement(ptnlnk);

    return definition;
  }


   /**
   * Creates the wsdl file from the <code>Definition</code>.
   */
  private static boolean createFileFromWSDL(Definition definition, File wsdlFile) throws WSDLException, IOException {
    boolean result = false;

    FileWriter fw = null;

    try {
      WSDLFactory factory = WSDLFactory.newInstance();
      // creating the registry for the WSDL4J Extension
      ExtensionRegistry registry = factory.newPopulatedExtensionRegistry();
      Jbi4CorbaPartnerLinkExtension.register(registry);
      Jbi4CorbaExtension.register(registry);
      it.imolinfo.jbi4corba.schema.SchemaUtil.registerSchema(registry);
      definition.setExtensionRegistry(registry);

      // creating the writer
      WSDLWriter writer = factory.newWSDLWriter();

      // creating the new File
      fw = new FileWriter(wsdlFile);

      // write the file
      writer.writeWSDL(definition, fw);

      result = true;
    } finally {
      if (fw != null) {
        fw.close();
      }
    }

    return result;
  }
    /**
    * Prepare the  WSDLDescriptors for the wsdl creation
    */
    private static ArrayList<WSDLDescriptor> createWSDLDescList(Object[][] descInfos,List<IdlFileDataHolder> idlHolder){
        
        
        ArrayList<WSDLDescriptor> descList=new ArrayList<WSDLDescriptor>();
        String namespace="";
       
        //Create the WSDL DESCRIPTOR
        for (int i=0;i<descInfos.length;i++){
            boolean  selected =(Boolean)descInfos[i][SELECTED_FOR_CREATION];
            DescInfo descInfo=(DescInfo)descInfos[i][DESC_INFO];
            String name=(String)descInfos[i][ENDPOINT_NAME];
            //If the interface is selected for the creation , we have to fill the WSDLDescriptor
            if(selected){
                for(int k=0;k<idlHolder.size();k++){
                    if(idlHolder.get(k).getInterfaceName().equals(name)){
                        namespace=idlHolder.get(k).getInterfaceNameSpace();
                    }
                }
                
                String locType="";
                String corbaSVCName="";
                if(descInfo.isStateless()){
                    locType=descInfo.getLocalizationType();    
                    corbaSVCName=descInfo.getAddress();
                }
              
                WSDLDescriptor desc =new WSDLDescriptor(corbaSVCName,locType,namespace,name);      
                desc.setOrbProperties(descInfo.getProperties());
                descList.add(desc);
            }
                
        }
       
        return descList;
    }
    
    /**
     * This Method Load the Interfaces and namespaces from idl and store it 
     * in a IdlDataHolder 
     **/
     private static List<IdlFileDataHolder> loadInterfaces(File idl){
        List<IdlFileDataHolder> idlData=null;
        idlData=new ArrayList<IdlFileDataHolder>(); 
        WSDLGenerator generator = WSDLGenerator.getWSDLGenerator();
        try {
            idlData = generator.getIdlFileData(idl);
        } catch (Exception ex) {
            logConsole(ex.getMessage(),ex );
        }
  
       return idlData;
        
    }
     
     private static void logConsole(String message, Exception e) {
        System.err.println(message);
        pos.println(message);
        if (e!=null) {
            pos.println("\nStackTrace :");
            pos.println(e);
        }
        pos.println(LINE_SEPARATOR);
        pos.close();
        System.exit(-1);
     }
     private static void logConsole(String message) {
         logConsole(message,null);
     }
     
     
     

}

