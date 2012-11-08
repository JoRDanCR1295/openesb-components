/*
 * SUScriptExecutor.java
 *
 * Created on 9 April, 2007, 11:26 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import com.zaz.jbi.engine.screenscrapingse.scrconfig.Parameter;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.Returntype;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.Scriptconfig;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;

import java.lang.reflect.Method;

import java.net.URL;
import java.net.URLClassLoader;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.SimpleBindings;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

/**
 * This class is used to execute the Script files that are present as part of the SU.ZIP file.
 * This class illustrates the use case wherein, the user will not send any input xml file, but
 * just invokes a invoke method with input parameters of Script.
 *
 * @author prashanthbr
 */
public class SUScriptExecutor extends ScriptExecutorImpl {

    private static Logger msLogger = null;
    private static String CONSTANT_PACKAGE_NAME = "domparsingdemo";
    private String mScrFileName = null;
    private String mEngineName = null;
    private ScriptInfoVO mScrInfo = null;
    /** DOCUMENT ME! */
    JarFileLoader mUrlCl = null;
    private JAXBContext mJAXBContext = null;

    /**
     * Creates a new instance of SUScriptExecutor
     *
     * @param aSURootPath
     * @param aScrVo
     */
    public SUScriptExecutor(String aSURootPath, ScriptInfoVO aScrVo) {
        super(aSURootPath);
        mScrFileName = aScrVo.getMScriptFileName();
        mEngineName = aScrVo.getMEngineName();
        mScrInfo = aScrVo;
        msLogger = Logger.getLogger(this.getClass().getName());

        initJAXBContext();
    }

    /**
     *
     */
    public void initJAXBContext() {
        System.out.println("Inside the initJAXBContext method....");

        try {
            PackageInfoHandler lHandler = new PackageInfoHandler();

            //Creating the SAXParser to get a list of Package names.
            SAXParserFactory lFactory = SAXParserFactory.newInstance();
            SAXParser lParser = lFactory.newSAXParser();
            lParser.parse(new File(mSU_InstallPath,
                    ScreenScrapingSEConstants.PACKAGE_INFO_XML), lHandler);

            StringBuffer lBuffer = new StringBuffer();

            for (String lPackageName : lHandler.mList) {
                lBuffer.append(lPackageName).append(":");
            }

            if (lBuffer.length() <= 0) {
                lBuffer = new StringBuffer(CONSTANT_PACKAGE_NAME);
            }

            String lPackName = lBuffer.toString();

            if (lPackName.endsWith(":")) {
                lPackName = lPackName.substring(0, lPackName.lastIndexOf(":"));
            }

            System.out.println(
                    "The Value of StrinBuffer Containing Packages are ::" +
                    lPackName);

            ClassLoader lClassLoader = this.getClass().getClassLoader();
            msLogger.log(Level.INFO,
                    "The Additional Jar File Path is \n" + "file:///" +
                    mSU_InstallPath + "/" +
                    ScreenScrapingSEConstants.ADDITIONAL_SC_JAR);

            //@Todo Remove the hard coding here.put the Install root path of the SU
            URL[] lUrl = {
                new URL("file:///" + mSU_InstallPath + "/" +
                ScreenScrapingSEConstants.ADDITIONAL_SC_JAR)
            };

            final ArrayList<String> lPackageList = lHandler.mList;

            mUrlCl = new JarFileLoader(lUrl,
                    Thread.currentThread().getContextClassLoader());
            mJAXBContext = JAXBContext.newInstance(lPackName, mUrlCl);
        } catch (Exception e) {
            e.printStackTrace();
        }
    } //void initJAXBContext ends.


    /**
     * This method will execute the script by taking the input parameter as the script
     * input Parameter.
     *
     * @param aParameter
     *
     * @return Result of Script execution.
     */
    public Object executeSUScript(String aParameter) {
        Object lResult = null;
        String lFilePath = mSU_InstallPath;
        msLogger.log(Level.INFO, "The FilePath name is " + lFilePath);

        if (lFilePath == null) {
            msLogger.log(Level.SEVERE, "The String pathname is null\\erroneous");

            return lResult;
        } //  if (lFilePath == null) ends.
        // Just trimming to eliminate any spaces...

        lFilePath = lFilePath.trim();

        //It means that Script file is part of the SU, and needs to be picked from there
        File lFile = new File(mSU_InstallPath, mScrFileName);
        msLogger.log(Level.INFO,
                "\n\nThe ScriptFile path in the SU is ::" +
                lFile.getAbsolutePath());

        //Creating the file Reader
        FileReader lFileRdr = null;

        try {
            lFileRdr = new FileReader(lFile);

            //Instantiating the script Engine

            //Instantiating the script Engine
            mEngine = ScreenScrapingSERepository.GetInstance().getScriptEngine(mEngineName);

            // Trying the create the Script Context, so that in case there are any input parameters
            // then these can be provided to the Script Engine under the ENGINE.SCOPE for
            // Script Execution.
            Bindings lBinding = getBindings(aParameter);

            //Check of any input parameter availability
            if ((lBinding == null) || (lBinding.size() == 0)) {
                // Evaluating the script without any Bindings from the Script Engine.
                lResult = mEngine.eval(lFileRdr);
            } else {
                //Evaluating the Script by providing bindings.
                lResult = mEngine.eval(lFileRdr, lBinding);
            }

            printResult(lResult);
        } catch (Exception ex) {
            msLogger.log(Level.SEVERE, ex.toString());
            ex.printStackTrace();
        } finally {
            if (lFileRdr != null) {
                try {
                    //Closing the File Reader
                    lFileRdr.close();
                } catch (IOException ex) {
                    msLogger.log(Level.INFO, "Unable to close the FileReader");
                    ex.printStackTrace();
                }
            }
        }

        //Convert the result in to a Result object
        lResult = wrapResult(lResult);

        return lResult;
    } //executeSUScript method ends.


    /**
     * This method will execute the script by taking the input parameter as the script
     * input Parameter.
     *
     * @param aParameter
     *
     * @return Result of Script execution.
     */
    public Source processScript(Source aParameter) {
        Source lOutPut = aParameter;
        Object lResult = null;
        String lFilePath = mSU_InstallPath;
        msLogger.log(Level.INFO, "The FilePath name is " + lFilePath);

        if (lFilePath == null) {
            msLogger.log(Level.SEVERE, "The String pathname is null\\erroneous");

            return lOutPut;
        } //  if (lFilePath == null) ends.
        // Just trimming to eliminate any spaces...

        lFilePath = lFilePath.trim();

        //It means that Script file is part of the SU, and needs to be picked from there
        File lFile = new File(mSU_InstallPath, mScrFileName);
        msLogger.log(Level.INFO,
                "\n\nThe ScriptFile path in the SU is ::" +
                lFile.getAbsolutePath());

        //Creating the file Reader
        FileReader lFileRdr = null;

        try {
            lFileRdr = new FileReader(lFile);

            //Instantiating the script Engine
            mEngine = ScreenScrapingSERepository.GetInstance().getScriptEngine(mEngineName);

            // Trying the create the Script Context, so that in case there are any input parameters
            // then these can be provided to the Script Engine under the ENGINE.SCOPE for
            // Script Execution.
            Bindings lBinding = createBindings(aParameter);

            //lBinding.put("additionalSchemasjar", String.format("%s\\additionalSchemas.jar", mSU_InstallPath));

            //Check of any input parameter availability
            if ((lBinding == null) || (lBinding.size() == 0)) {
                // Evaluating the script without any Bindings from the Script Engine.
                lResult = mEngine.eval(lFileRdr);
            } else {
                //Evaluating the Script by providing bindings.
                lResult = mEngine.eval(lFileRdr, lBinding);
            }

            lOutPut = createOutputSource(lResult);

        // @TODo need to convert the object in to Source...
        } catch (Exception ex) {
            msLogger.log(Level.SEVERE, ex.toString());
            ex.printStackTrace();
        } finally {
            if (lFileRdr != null) {
                try {
                    //Closing the File Reader
                    lFileRdr.close();
                } catch (IOException ex) {
                    msLogger.log(Level.INFO, "Unable to close the FileReader");
                    ex.printStackTrace();
                }
            }
        }

        return lOutPut;
    } //processScript method ends.


    /**
     * DOCUMENT ME!
     *
     * @param aSrc
     *
     * @return
     */
    public Bindings createBindings(Source aSrc) {
        Bindings lBinding = new SimpleBindings();

        try {
            System.out.println("AFTER THE JAXB CONTEXT CREATION....");

            DOMSource adomSrc = (DOMSource) aSrc;

            StringBuffer lInputXML = ScriptXMLHelper.readFromDOMSource(adomSrc);
            System.out.println("The input file is \n" + lInputXML.toString());

            Document lDoc = ScriptXMLHelper.buildDOMDocument(new StringReader(
                    ScriptXMLHelper.readFromDOMSource(adomSrc).toString()));
            Element lRootElement = lDoc.getDocumentElement();
            System.out.println("The Root Element is  " +
                    lRootElement.getTagName());

            Scriptconfig lCfg = mScrInfo.getMScriptCfg();
            List lParams = lCfg.getOperations().getOperation().getParameter();

            ArrayList<Node> lElementNodes = new ArrayList<Node>();

            if (lParams.size() <= 1) {
                lElementNodes.add(lRootElement);
            } else {
                NodeList lList = lRootElement.getChildNodes();

                for (int i = 0; i < lList.getLength(); i++) {
                    Node lChildNode = lList.item(i);
                    System.out.println("The Child NOde Name is " +
                            lChildNode.getNodeName());

                    if (lChildNode.getNodeType() == Node.ELEMENT_NODE) {
                        lElementNodes.add(lChildNode);
                        System.out.println("The child Element node is " +
                                lChildNode.getNodeName());
                    }
                } //for ends.

            }

            //Getting all the elements from the input source and finally creating the bindings 
            // for the same.
            Unmarshaller lUnMarshall = mJAXBContext.createUnmarshaller();

            Parameter lParameter = null;

            for (int i = 0; (i < lElementNodes.size()) && (lParams.size() > 0);
                    i++) {
                Node node = lElementNodes.get(i);
                Object lObj = lUnMarshall.unmarshal(node);
                lParameter = (Parameter) lParams.get(i);

                if (lObj != null) {
                    System.out.println("The object type is " + lObj.toString());
                    lBinding.put(lParameter.getName(), lObj);
                } else {
                    msLogger.log(Level.SEVERE,
                            "The  Output Object Creation Has failed...is null \n\n");
                }
            } //for  ends.

            //Creating an empty return type and adding the same to the Params based on the same. 
            Returntype lType = lCfg.getOperations().getOperation().getReturntype();

            if (lType != null) {
                String lReturnParamName = lType.getName();
                String lXSDName = lType.getXsdname();
                String lTypeName = lType.getType();
                Object lResultObj = null;

                //Assuming that the return type belongs to an xsd type and is not any basic type.
                if ((lTypeName.equals(
                        ScreenScrapingSEConstants.PARAMATER_TYPE_XSDTYPE)) &&
                        (lXSDName != null)) {
                    lResultObj = getObjFromXSD(lXSDName);

                    if (lResultObj == null) {
                        msLogger.log(Level.SEVERE,
                                "Failed to create the return Type Object \n");
                        throw new Exception();
                    }

                    msLogger.log(Level.INFO,
                            "The Output Type object created is " +
                            lResultObj.toString());
                    //Putting the result object in the bindings.
                    lBinding.put(lReturnParamName, lResultObj);
                }
            } //if (lType != null) ends.        
            else {
                msLogger.log(Level.INFO,
                        "NO RETURN TYPE FOR THE SCRIPT IS SPECIFIED....");
            }
        } catch (Exception e) {
            e.printStackTrace();

            msLogger.log(Level.SEVERE,
                    "Error in obtaining the input Parameters.." + e);
        }

        return lBinding;
    } //createBindings ends.


    /**
     * DOCUMENT ME!
     *
     * @param aResult
     *
     * @return
     */
    public Source createOutputSource(Object aResult) {
        msLogger.log(Level.INFO, "createOutputSource");

        Source lResult = null;

        try {
            //Creating a JAXB Context.
            DocumentBuilderFactory lFact = DocumentBuilderFactory.newInstance();
            lFact.setNamespaceAware(true);

            // Creating a Dococument object of the resulting output.
            Document lDoc = lFact.newDocumentBuilder().newDocument();
            //Marshalling the result back to xml.
            Marshaller lMarshaller = mJAXBContext.createMarshaller();
            //JAXBContext context = JAXBContext.newInstance(aResult.getClass());
            //Marshaller m = context.createMarshaller();
            //m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            //m.marshal(aResult, lDoc);
            lMarshaller.marshal(aResult, lDoc);

            lResult = new DOMSource(lDoc);
            msLogger.log(Level.INFO, "PRINTING THE OUTMSG....");
            msLogger.log(Level.INFO,
                    (ScriptXMLHelper.readFromDOMSource((DOMSource) lResult)).toString());
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return lResult;
    } //createOutputSource ends.


    /**
     * DOCUMENT ME!
     *
     * @param aXSDName
     *
     * @return
     */
    public Object getObjFromXSD(String aXSDName) {
        msLogger.log(Level.INFO, "OUTPUT XSD Name is " + aXSDName);

        Object lResult = null;

        try {
            //       lResult = mJAXBContext.createUnmarshaller().unmarshal(new File(mSU_InstallPath,aXSDName));
            lResult = mUrlCl.getClass(aXSDName);
        } catch (Exception e) {
            e.printStackTrace();
            msLogger.log(Level.SEVERE,
                    "Exception in Generating the output.." + e);
        }

        return lResult;
    }

    private Object wrapResult(Object aResult) {
        Object lResult = null;

        StringBuffer lBuff = new StringBuffer();
        lResult = (lBuff.append("<result>").append(aResult.toString()).append("</result>")).toString();

        return lResult;
    }

    private Bindings getBindings(String aParameter) {
        Bindings lResult = null;

        if (aParameter == null) {
            msLogger.log(Level.INFO, "No parameters are being passed to Script");
        } else {
            lResult = new SimpleBindings();
            //@ToDo The name of parameter must be read from ScriptMap.
            lResult.put("param1", aParameter);
        }

        return lResult;
    } //getBindings method


    private Bindings getBindings(Source aParameter) {
        Bindings lResult = null;

        if (aParameter == null) {
            msLogger.log(Level.INFO, "No parameters are being passed to Script");
        } else {
            lResult = new SimpleBindings();
            //@ToDo The name of parameter must be read from ScriptMap.
            lResult.put("param1", aParameter);
        }

        return lResult;
    } //getBindings method


    /**
     * DOCUMENT ME!
     *
     * @param aObj
     */
    public void printResult(Object aObj) {
        try {
            if (aObj != null) {
                msLogger.log(Level.INFO, "The object is ::" + aObj);

                Class lClass = aObj.getClass();
                Method[] lMethods = lClass.getMethods();

                for (Method method : lMethods) {
                    msLogger.log(Level.INFO,
                            "The Method Name is ::" + method.getName());

                //Invoking the methods..
                //          try
                //          {
                //            method.invoke(aObj, new Object[0]);
                //          }
                //          catch (Exception e)
                //          {
                //            e.printStackTrace();
                //            msLogger.log(Level.INFO,"Inside Blk::Exception  is \n"+e);
                //          }
                }
            } else {
                msLogger.log(Level.INFO, "Outside Blk :: THe Object is NULL....");
            }
        } catch (Exception e) {
            e.printStackTrace();
            msLogger.log(Level.INFO, "Exception  is \n" + e);
        }
    } //void printResult ends.


    private static class PackageInfoHandler extends DefaultHandler {

        private static final String FILE_ELEMENT = "file";
        private static final String XSD_NAME_ATTR = "xsdName";
        private static final String PACKAGE_NAME_ATTR = "packageName";
        /**
         * DOCUMENT ME!
         */
        /**
         * DOCUMENT ME!
         */
        /**
         * DOCUMENT ME!
         */
        /**
         * DOCUMENT ME!
         */
        ArrayList<String> mList = new ArrayList<String>();

        /**
         * DOCUMENT ME!
         *
         * @param uri DOCUMENT ME!
         * @param localName DOCUMENT ME!
         * @param qName DOCUMENT ME!
         * @param attributes DOCUMENT ME!
         *
         * @throws SAXException DOCUMENT ME!
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (qName.equals(FILE_ELEMENT)) {
                mList.add(attributes.getValue(PACKAGE_NAME_ATTR));
            }
        } //startElement ends.

    } //class PackageInfoHandler  ends.

} //class SUScriptExecutor  ends.


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
class JarFileLoader extends URLClassLoader {

    private JAXBContext mJaxbContext = null;
    private Logger msLogger = Logger.getLogger(JarFileLoader.class.getName());

    /**
     * Creates a new JarFileLoader object.
     *
     * @param aUrl DOCUMENT ME!
     * @param aParent DOCUMENT ME!
     */
    public JarFileLoader(URL[] aUrl, ClassLoader aParent) {
        super(aUrl, aParent);
    }

    /**
     * DOCUMENT ME!
     *
     * @param aClassName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    Object getClass(String aClassName) {
        Object lResult = null;

        try {
            Class lClass = this.findLoadedClass(aClassName);
            lResult = lClass.newInstance();

            createInnerObject(lClass, lResult);
        } catch (Exception e) {
            msLogger.log(Level.SEVERE,
                    "Unable to Load the Output Element Class..");
        }

        return lResult;
    }

    private void createInnerObject(Class lClass, Object lResult) {
        for (Class c : lClass.getClasses()) {
            try {
                String methodName = "set" + c.getSimpleName();
                msLogger.log(Level.INFO, methodName);
                Method method = null;
                try {
                    method = lClass.getMethod(methodName, c);
                } catch (NoSuchMethodException ex) {
                    methodName = "get" + c.getSimpleName();
                    method = lClass.getMethod(methodName);
                    List list = (List) method.invoke(lResult);
                    list.add(c.newInstance());
                    continue;
                }
                Object object = c.newInstance();
                method.invoke(lResult, object);
                createInnerObject(c, object);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
} //class JarFileLoader  ends.
