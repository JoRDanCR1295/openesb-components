/*
 * ScriptExecutorImpl.java
 *
 * Created on February 23, 2007, 12:12 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.engine.scriptse.process;

import com.sun.jbi.engine.scriptse.datatypes.*;

import org.w3c.dom.Document;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLConnection;

import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;


/**
 * DOCUMENT ME!
 *
 * @author Prashanth B.R
 */
public class ScriptExecutorImpl implements ScriptExecutor {
    private static Logger msLogger = null;
    private static final String JAR_FILE = "additionalSchemas.jar";

    //Properties file containing all the ScriptEngineFactories
    private Properties mProperties = null;
    private QName mQname = null;
    private JAXBContext mJaxbCtx = null;
    private Unmarshaller mUnMarshaller = null;
    private Marshaller mMarshaller = null;

    /** DOCUMENT ME! */
    protected ScriptEngine mEngine = null;
    private URLClassLoader mUrlCl = null;

    /** DOCUMENT ME! */
    protected String mSU_InstallPath = null;

    /**
     * Creates a new instance of ScriptExecutorImpl
     *
     * @param aSURootPath
     */
    public ScriptExecutorImpl(String aSURootPath) {
        System.out.println("Inside the ScriptExecutorImpl Constructor..");

        try {
            mSU_InstallPath = aSURootPath;

            //Getting the current class classloader.
            ClassLoader lClassLoader = this.getClass().getClassLoader();

            //@Todo Remove the hard coding here.put the Install root path of the SU
            URL[] lUrl = {new URL("file:///" + aSURootPath + JAR_FILE)};
            mUrlCl = new URLClassLoader(lUrl, lClassLoader);

            //Creating a JAXB Context.
            mJaxbCtx = JAXBContext.newInstance("com.sun.jbi.engine.scriptse.datatypes", mUrlCl);
            //Creating the Marshaller and UnMarshaller
            mUnMarshaller = mJaxbCtx.createUnmarshaller();
            mMarshaller = mJaxbCtx.createMarshaller();
            msLogger = Logger.getLogger("PrashLog");

            //      printClasses(mUrlCl);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    } //ScriptExecutorImpl() ends.

    /**
     * This is the core script processing method, which will accept a Dom source file
     * contain script data\meta data required for script execution. The result is returned as
     * another source content
     *
     * @param aInputSource
     *
     * @return Source The result of script execution is returned as Source.
     */
    public Source processScript(Source aInputSource) {
        Source lResult = null;

        // UnMarshalling the inputs xml source file and getting the  Script Object.
        Script lScript = getScript(aInputSource);

        //Getting the Engine Name.
        String lEngineName = lScript.getScriptType().getScriptEngine().getName();
        System.out.println("****The Script Engine name is " + lEngineName);
        //Executing the Script.

        //Instantiating the script Engine
        //Executing the Script.

        //Instantiating the script Engine
        mEngine = ScriptseRepository.GetInstance().getScriptEngine(lEngineName);

        if (mEngine == null) {
            msLogger.log(Level.SEVERE, "The SCRIPT ENGINE IS NOT LOADED PLESE CHECK");

            return null;
        }

        try {
            StringBuffer lBuff = ScriptXMLHelper.readFromDOMSource((DOMSource) aInputSource);
            StringReader lReader = new StringReader(lBuff.toString());
            Document lSampDoc = ScriptXMLHelper.buildDOMDocument(lReader);
            setNsLocal(lSampDoc.getNamespaceURI(), lSampDoc.getLocalName());

            msLogger.log(Level.INFO, "THE INPUT SOURCE FROM SCRIPTUTIL CLASS IS ...");
            msLogger.log(Level.INFO, lBuff.toString());

            Object lObj = executeScript(lScript);

            lResult = createOutputSource(lScript, lObj);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return lResult;
    } //Source processScript method ends.

    /**
     * DOCUMENT ME!
     *
     * @param aScript
     *
     * @return
     */
    public Object executeScript(Script aScript) {
        msLogger.log(Level.INFO, "Inside the executeScript method");

        Object lResult = null;

        // Finding out the source of the script.
        Script.ScriptSource lScriptSrc = aScript.getScriptSource();
        Script.ScriptSource.File lFile = lScriptSrc.getFile();
        Script.ScriptSource.Inlined lInlined = lScriptSrc.getInlined();
        Script.ScriptSource.Url lUrl = lScriptSrc.getUrl();

        if (lFile != null) {
            lResult = executeScriptFrmFile(lFile, aScript);
        } else if (lInlined != null) {
            lResult = executeScriptFrmInlinedTxt(lInlined, aScript);
        } else {
            lResult = executeScriptFrmUrl(lUrl, aScript);
        }

        return lResult;
    } //executeScript method ends.

    /**
     * DOCUMENT ME!
     *
     * @param aInputSource
     *
     * @return
     */
    public Script getScript(Source aInputSource) {
        msLogger.log(Level.INFO, "Inside the getScript object method");

        Script lResult = null;

        try {
            lResult = (Script) mUnMarshaller.unmarshal(aInputSource);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return lResult;
    } // getScript ends.

    private Object executeScriptFrmFile(Script.ScriptSource.File aFile, Script aScript) {
        Object lResult = null;
        String lFilePath = aFile.getFilePath();

        if (lFilePath == null) {
            msLogger.log(Level.SEVERE, "The String pathname is null\\erroneous");

            return lResult;
        } //  if (lFilePath == null) ends.
          // Just trimming to eliminate any spaces...

        lFilePath = lFilePath.trim();

        File lFile = new File(lFilePath);

        if (aFile.isPreloaded() == true) {
            //It means that Script file is part of the SU, and needs to be picked from there
            lFile = new File(mSU_InstallPath, lFilePath);
            msLogger.log(Level.INFO, "\n\nThe ScriptFile path in the SU is ::" + lFilePath);
        }

        //Creating the file
        FileReader lFileRdr = null;

        try {
            lFileRdr = new FileReader(lFile);

            // Trying the create the Script Context, so that in case there are any input parameters
            // then these can be provided to the Script Engine under the ENGINE.SCOPE for
            // Script Execution.
            Bindings lBinding = getScriptBindings(aScript);

            //Check of any input parameter availability
            if ((lBinding == null) || (lBinding.size() == 0)) {
                // Evaluating the script without any Bindings from the Script Engine.
                lResult = mEngine.eval(lFileRdr);
            } else {
                //Evaluating the Script by providing bindings.
                lResult = mEngine.eval(lFileRdr, lBinding);
            }

            //Checking if the result is null. If so, then setting the sucess message.
            // This is required for scripts which do not return anything.
            lResult = setResult(lResult);
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

        return lResult;
    } //getScriptFrmFile method ends.

    private Object setResult(Object aResult) {
        //Setting a Success message in case the script is executed successfully
        //but there is no return value.
        if (aResult == null) {
            aResult = new String("Successfully executed the script");
        }

        return aResult;
    }

    private Object executeScriptFrmUrl(Script.ScriptSource.Url aUrl, Script aScript) {
        Object lResult = null;
        FileReader lFileRdr = null;

        try {
            //Creating the URL from the url path.
            URL lUrl = new URL(aUrl.getUrlLocation());
            String lFileName = lUrl.getFile();

            //Trying to get only the file Name from a pattern like -- PrashWebApp_Sample\swing.rb
            if ((lFileName != null) && (lFileName.indexOf("/") >= 0)) {
                lFileName = lFileName.substring(lFileName.lastIndexOf("/") + 1);
            }

            URLConnection lConnection = lUrl.openConnection();
            InputStream lStream = lConnection.getInputStream();
            BufferedInputStream lBuffInStream = new BufferedInputStream(lStream);

            //Creating a file to store the downloaded contents
            File lFile = new File(lFileName);
            FileOutputStream lOutStream = new FileOutputStream(lFile);
            int lInt;

            while ((lInt = lBuffInStream.read()) != -1) {
                lOutStream.write(lInt);
            }

            //Closing all the Streams. Better to clean up what you have created.
            lOutStream.flush();
            lOutStream.close();
            lStream.close();
            lBuffInStream.close();

            //Executing the Script from the saved file.
            lFileRdr = new FileReader(lFile);

            // Trying the create the Script Context, so that in case there are any input parameters
            // then these can be provided to the Script Engine under the ENGINE.SCOPE for
            // Script Execution.
            Bindings lBinding = getScriptBindings(aScript);

            //Check of any input parameter availability
            if ((lBinding == null) || (lBinding.size() == 0)) {
                // Evaluating the script without any Bindings from the Script Engine.
                lResult = mEngine.eval(lFileRdr);
            } else {
                //Evaluating the Script by providing bindings.
                lResult = mEngine.eval(lFileRdr, lBinding);
            }

            //Checking if the result is null. If so, then setting the sucess message.
            // This is required for scripts which do not return anything.
            lResult = setResult(lResult);
        } catch (Exception ex) {
            msLogger.log(Level.SEVERE, "Failure to execute from URL :: \n" + ex.toString());
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
        } // finally ends.

        return lResult;
    } //getScriptFrmFile method ends.

    /**
     * This method is used to obtain the Script Text from the InputSource. The script would
     * be Inlined within the input source file.
     *
     * @param aInLined DOCUMENT ME!
     * @param aScript DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    private Object executeScriptFrmInlinedTxt(Script.ScriptSource.Inlined aInLined, Script aScript) {
        Object lResult = null;

        //Getting the inlined script from the Script Object.
        String lScrTxt = aInLined.getScriptText();

        try {
            // Trying the create the Script Context, so that in case there are any input parameters
            // then these can be provided to the Script Engine under the ENGINE.SCOPE for
            // Script Execution.
            Bindings lBinding = getScriptBindings(aScript);

            //Check of any input parameter availability
            if ((lBinding == null) || (lBinding.size() == 0)) {
                // Evaluating the script without any Bindings from the Script Engine.
                lResult = mEngine.eval(lScrTxt);
            } else {
                //Evaluating the Script by providing bindings.
                lResult = mEngine.eval(lScrTxt, lBinding);
            }

            //Checking if the result is null. If so, then setting the sucess message.
            // This is required for scripts which do not return anything.
            lResult = setResult(lResult);
        } catch (ScriptException ex) {
            msLogger.log(Level.SEVERE, "Failed to execute the Inlined Script..");
            ex.printStackTrace();
        }

        return lResult;
    } //getScriptFrmInlinedTxt method ends.

    /**
     * DOCUMENT ME!
     *
     * @param aScript
     *
     * @return
     */
    public Bindings getScriptBindings(Script aScript) {
        msLogger.log(Level.INFO, "Inside the ScriptContext method");

        Bindings lBinding = null;

        Script.InputParameters lInput = aScript.getInputParameters();

        if (lInput == null) {
            msLogger.log(Level.WARNING, "Input Parameters are null :: Ensure if its Ok");
        } else {
            lBinding = new SimpleBindings();

            List<Script.InputParameters.Param> lList = lInput.getParam();

            // Iterating the List to get the parameters and storing the same in the Script Context.
            for (Script.InputParameters.Param aPara : lList) {
                //Setting the input variables as Binding variable.
                lBinding.put(aPara.getName(), aPara.getValue());
            } // for (Script.InputParameters.Param aPara:lList) ends.
        } //else ends

        return lBinding;
    } //getScriptContext method ends.

    /**
     * DOCUMENT ME!
     *
     * @param aInputScript
     * @param aResult
     *
     * @return
     */
    public Source createOutputSource(Script aInputScript, Object aResult) {
        msLogger.log(Level.INFO, "createOutputSource");

        Source lResult = null;

        try {
            //Creating a JAXB Context.
            DocumentBuilderFactory lFact = DocumentBuilderFactory.newInstance();
            lFact.setNamespaceAware(true);

            Document lDoc = lFact.newDocumentBuilder().newDocument();

            //      try
            //      {
            //        //REMOVE THE CODE BELOW....
            //        Class lEngineVOClass = Class.forName("firstsu.EngineVO");
            //        Object lEngineVo = lEngineVOClass.newInstance();
            //        Method[] lEngMethods = lEngineVOClass.getDeclaredMethods();
            //        for (Method aMethod : lEngMethods)
            //        {
            //          if (aMethod.getName().equals("setName"))
            //          {
            //            aMethod.invoke(lEngineVo,"JRUBY Engine");
            //          }
            //          else if (aMethod.getName().equals("setVersion"))
            //          {
            //            aMethod.invoke(lEngineVo, "4.6");
            //          }
            //          
            //        }
            //        //Trying to dynamically create the class and
            //        Class lClass = Class.forName("firstsu.ResultVO");
            //        Object lResultObj = lClass.newInstance();
            //        Method[] lMethods = lClass.getDeclaredMethods();
            //        for (Method aMethod : lMethods)
            //        {
            //          if (aMethod.getName().equals("setResultName"))
            //          {
            //            aMethod.invoke(lResultObj,"Prashanth Result");
            //          }
            //          else if (aMethod.getName().equals("setEngineObj"))
            //          {
            //            aMethod.invoke(lResultObj, lEngineVo);
            //          }
            //          
            //        }
            //        JAXBContext lContext2 = JAXBContext.newInstance("firstsu.ObjectFactory", mUrlCl);
            //        Marshaller lMarshaller2 = lContext2.createMarshaller();
            //        JAXBElement lElement = new JAXBElement(mQname,lClass, lResult);
            //        aInputScript.getResult().getResultdata().setAny(lElement);
            //        
            //        lMarshaller2.marshal(lElement,
            //            new FileOutputStream("sample.xml"));
            //        BufferedInputStream lStream =  new BufferedInputStream(new FileInputStream("sample.xml"));
            //        DocumentBuilderFactory lFactory = DocumentBuilderFactory.newInstance();
            //        DocumentBuilder lDocumentBuilder = lFactory.newDocumentBuilder();
            //        Document lDoc1 = lDocumentBuilder.parse(lStream);
            //        
            //        Element lDocElement = lDoc1.getDocumentElement();
            //        aInputScript.getResult().getResultdata().setAny(lDocElement);
            //        
            //      }
            //      catch (Exception ex)
            //      {
            //        ex.printStackTrace();
            //      }
            //      
            //      mMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            //      mMarshaller.marshal(aInputScript, new FileOutputStream("output.xml"));
            //REMOVE THE CODE BELOW ENDS.....
            // Creating a Dococument object of the resulting output.
            JAXBElement lElement = new JAXBElement(mQname, aResult.getClass(), aResult);

            aInputScript.getResult().getResultdata().setAny(lElement);
            mMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            mMarshaller.marshal(aInputScript, lDoc);

            lResult = new DOMSource(lDoc);
            msLogger.log(Level.INFO, "PRINTING THE OUTMSG....");
            msLogger.log(
                Level.INFO, (ScriptXMLHelper.readFromDOMSource((DOMSource) lResult)).toString()
            );
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return lResult;
    } // createOutputSource method ends.

    /**
     * DOCUMENT ME!
     *
     * @param lUri
     * @param lLocal
     */
    public void setNsLocal(String lUri, String lLocal) {
        msLogger.log(Level.INFO, "\n NAMESPACE URI IS " + lUri);
        msLogger.log(Level.INFO, "\n LOCAL PART NAME IS  " + lLocal);
        mQname = new QName("http://www.sun.com/scriptSE/scriptse", "Localresult");
    }

    /**
     * DOCUMENT ME!
     *
     * @param aLoader
     */
    public void printClasses(ClassLoader aLoader) {
        /**
         * DOCUMENT ME!
         *
         * @author $author$
         * @version $Revision: 1.3 $
         */
        class PrashLoader extends ClassLoader {
            /**
             * Creates a new PrashLoader object.
             *
             * @param aLdr DOCUMENT ME!
             */
            public PrashLoader(ClassLoader aLdr) {
                super(aLdr);
            }

            /**
             * DOCUMENT ME!
             */
            public void printClasses() {
                Package[] lPackages = this.getPackages();

                for (Package elem : lPackages) {
                    System.out.println("The loader Packages are " + elem.getName());
                }
            }
        }
        ;
        new PrashLoader(aLoader).printClasses();
    } // printClasses method ends.
} //class ScriptExecutorImpl  ends.
