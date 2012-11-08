/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SUScriptExecutor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.process;

import org.glassfish.openesb.engine.screenscrapingse.scrconfig.Parameter;
import org.glassfish.openesb.engine.screenscrapingse.scrconfig.Returntype;
import org.glassfish.openesb.engine.screenscrapingse.scrconfig.Scriptconfig;

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
import org.glassfish.openesb.engine.screenscrapingse.I18n;

/**
 * This class is used to execute the Script files that are present as part of the SU.ZIP file.
 * This class illustrates the use case wherein, the user will not send any input xml file, but
 * just invokes a invoke method with input parameters of Script.
 *
 * @author prashanthbr
 */
public class SUScriptExecutor extends ScriptExecutorImpl {

    private static Logger msLogger = Logger.getLogger(SUScriptExecutor.class.getName());;
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

            msLogger.info("The Value of StrinBuffer Containing Packages are ::" +
                    lPackName);

            ClassLoader lClassLoader = this.getClass().getClassLoader();
            msLogger.info("The Additional Jar File Path is \n" + "file:///" +
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
            msLogger.severe(I18n.loc("SSSECORE-1030: excpetion {0}",e.getMessage()));
        
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
        msLogger.info("The FilePath name is " + lFilePath);

        if (lFilePath == null) {
            msLogger.severe(I18n.loc("SSSECORE-1031 The String pathname is null erroneous"));

            return lResult;
        } //  if (lFilePath == null) ends.
        // Just trimming to eliminate any spaces...

        lFilePath = lFilePath.trim();

        //It means that Script file is part of the SU, and needs to be picked from there
        File lFile = new File(mSU_InstallPath, mScrFileName);
        msLogger.info(
                "The ScriptFile path in the SU is ::" +
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
            msLogger.severe(I18n.loc("SSSECORE-1010: exception {0}",ex.getMessage()));
        
        } finally {
            if (lFileRdr != null) {
                try {
                    //Closing the File Reader
                    lFileRdr.close();
                } catch (IOException ex) {
                    msLogger.severe(I18n.loc("SSSECORE-1013: Unable to close the FileReader {0}",ex.getMessage()));
                 
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
        msLogger.info("The FilePath name is " + lFilePath);

        if (lFilePath == null) {
            msLogger.severe("SSSECORE-1005: The String pathname is null erroneous");

            return lOutPut;
        } //  if (lFilePath == null) ends.
        // Just trimming to eliminate any spaces...

        lFilePath = lFilePath.trim();

        //It means that Script file is part of the SU, and needs to be picked from there
        File lFile = new File(mSU_InstallPath, mScrFileName);
        msLogger.info(
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
            msLogger.severe(I18n.loc("SSSECORE-1036: exception...{0}", ex.getMessage()));
          
        } finally {
            if (lFileRdr != null) {
                try {
                    //Closing the File Reader
                    lFileRdr.close();
                } catch (IOException ex) {
                    msLogger.severe(I18n.loc("SSSEVORE-1037: Unable to close the FileReader {0}",ex.getMessage()));
                  
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
                        msLogger.info("The child Element node is " +
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
                    msLogger.info("The object type is " + lObj.toString());
                    lBinding.put(lParameter.getName(), lObj);
                } else {
                    msLogger.severe(I18n.loc("The  Output Object Creation Has failed...is null"));
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
                        msLogger.severe(I18n.loc("SSSECORE-1039: Failed to create the return Type Object "));
                        throw new Exception();
                    }

                    msLogger.info(
                            "The Output Type object created is " +
                            lResultObj.toString());
                    //Putting the result object in the bindings.
                    lBinding.put(lReturnParamName, lResultObj);
                }
            } //if (lType != null) ends.        
            else {
                msLogger.info(
                        "NO RETURN TYPE FOR THE SCRIPT IS SPECIFIED....");
            }
        } catch (Exception e) {
          

            msLogger.severe(
                    I18n.loc("SSSECORE-1040: Error in obtaining the input Parameters..{0}", e.getMessage()));
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
        msLogger.info("createOutputSource");

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
            msLogger.info("PRINTING THE OUTMSG....");
            msLogger.info(
                    (ScriptXMLHelper.readFromDOMSource((DOMSource) lResult)).toString());
        } catch (Exception ex) {
            msLogger.severe(I18n.loc("SSSECORE-1010: exception {0}",ex.getMessage()));
          
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
        msLogger.info("OUTPUT XSD Name is " + aXSDName);

        Object lResult = null;

        try {
            //       lResult = mJAXBContext.createUnmarshaller().unmarshal(new File(mSU_InstallPath,aXSDName));
            lResult = mUrlCl.getClass(aXSDName);
        } catch (Exception e) {
          
            msLogger.severe(I18n.loc(
                    "SSSECORE-1041: Exception in Generating the output..{0}", e.getMessage()));
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
            msLogger.info("No parameters are being passed to Script");
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
            msLogger.info("No parameters are being passed to Script");
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
                msLogger.info("The object is ::" + aObj);

                Class lClass = aObj.getClass();
                Method[] lMethods = lClass.getMethods();

                for (Method method : lMethods) {
                    msLogger.info(
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
                msLogger.info("Outside Blk :: THe Object is NULL....");
            }
        } catch (Exception e) {
            
            msLogger.severe(I18n.loc("SSSECORE-1044: Exception  is \n", e.getMessage()));
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
 * @version $Revision: 1.6 $
 */
class JarFileLoader extends URLClassLoader {

    private JAXBContext mJaxbContext = null;
    private static Logger msLogger = Logger.getLogger(JarFileLoader.class.getName());

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
            msLogger.severe(I18n.loc("SSSECORE-1023: Unable to Load the Output Element Class.."));
        }

        return lResult;
    }

    private void createInnerObject(Class lClass, Object lResult) {
        for (Class c : lClass.getClasses()) {
            try {
                String methodName = "set" + c.getSimpleName();
                msLogger.info(methodName);
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
