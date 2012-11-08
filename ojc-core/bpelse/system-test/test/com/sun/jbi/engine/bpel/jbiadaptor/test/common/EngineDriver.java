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
 * @(#)EngineDriver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import javax.jbi.JBIException;
import javax.management.MBeanServerFactory;
import javax.transaction.Transaction;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.EngineHelper;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.util.SUArtifacts;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;


/**
 *
 * @author Sun Microsystems
 */
public class EngineDriver {
	
	/** */
	public static final int RECOVERY_ID = 9999;
	
	/** */
    public static final int NO_RECOVERY_ID = 99995;
    
    /** */
    public static final int SUCCESSFUL_EXECUTION = 0;
    
    /** */
    public static final int FILE_COMPARISION_FAILED = 2;
	
    public static final String RECOVERY_TESTS_FLAG = "RECOVERY_TESTS";
    
    /* */
    private static final DocumentBuilderFactory mBuilderFactory = DocumentBuilderFactory.newInstance();
    
    /* */
    private static HashMap transactionsMap = new HashMap();
    
    /* */
    private static HashMap operationsMap = new HashMap();
    
    /* */
    private static String initialMsgExId;
    
    /* */
    protected static final String EXP_OUT_FILE_PATH = "EXP_OUT_FILE_PATH";
    
    /* */
    protected static final String ACT_OUT_FILE_PATH = "ACT_OUT_FILE_PATH";
    
    static { mBuilderFactory.setNamespaceAware(true); }

    /* */
    protected Engine mEng;
        
    /* */
    protected Properties mProps;

    public EngineDriver() throws Exception {
        //mEng = SetUpHelper.getSetUp().getEngine();
        SetUpHelper setUp = new SetUpHelper();
        mEng = setUp.getEngine();
    }

    /* 
     * Creates a new instance of EngineDriver 
     */
    private static void serialize(WSMessage msg, FileOutputStream fos) {
        try {
            OutputFormat outFormat = new OutputFormat("XML", "UTF-8", true);
            outFormat.setPreserveSpace(false);

            //outFormat.setLineSeparator("");
            XMLSerializer serializer = new XMLSerializer();
            serializer.setOutputFormat(outFormat);
            serializer.setOutputByteStream(fos);
            serializer.serialize(msg.getElement());
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /*
     * 
     */
    private static DocumentBuilder newBuilder() throws ParserConfigurationException {
        synchronized (mBuilderFactory) {
            return mBuilderFactory.newDocumentBuilder();
        }
    }
    
    /*
     * 
     */
    private static UtilityClass lookupUtilityClass(Properties props) throws Exception {

        String clazz = props.getProperty(
                        "CLASS", "com.sun.jbi.engine.bpel.jbiadaptor.test.common.UtilityClass");
        return (UtilityClass) Class.forName(clazz).newInstance();
    }
    
    /*
     * 
     */
    private static EngineDriver lookupEngineDriverClass(String propFilePath) throws Exception {
        // Load the properties file
        File propFile = new File(propFilePath);
        Properties props = new Properties();
        InputStream inStream = propFile.toURL().openStream();
        props.load(inStream);
        inStream.close();
    	
    	String clazz = props.getProperty("DRIVER_CLASS", "com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineDriver");
    	EngineDriver driver = (EngineDriver) Class.forName(clazz).newInstance();
		driver.setProperties(props);
    	return driver;
    }

    /*
     * 
     */
    private static void compareFiles(File expectedOutput, File actualRunOutput) 
    throws FileComparisionFailedException {
        try {
            FileInputStream actualRunFIS = new FileInputStream(actualRunOutput);
            FileInputStream expectedOutputFIS = new FileInputStream(expectedOutput);
            int actualRunOutputFileSize = actualRunFIS.available();
            int expectedOutputFileSize = expectedOutputFIS.available();

            byte[] actualOutputBuffer = new byte[actualRunOutputFileSize];
            byte[] expectedOutputBuffer = new byte[expectedOutputFileSize];

            actualRunFIS.read(actualOutputBuffer);
            expectedOutputFIS.read(expectedOutputBuffer);

            String actualOutputString = new String(actualOutputBuffer);
            String expectedOutputString = new String(expectedOutputBuffer);
            if (actualOutputString.equals(expectedOutputString)) {
                return;
            }
            StringTokenizer tokensForExpected = new StringTokenizer(expectedOutputString, "\n");
            StringTokenizer tokensForActualRun = new StringTokenizer(actualOutputString, "\n");
            TallySet expectedTokensSet = new TallySet(tokensForExpected);
            TallySet actualTokensSet = new TallySet(tokensForActualRun);
            if (!expectedTokensSet.compareOutput(actualTokensSet)) {
            	throw new FileComparisionFailedException("File comparision failed.");
            }
        } catch (Exception ex) {
        	throw new FileComparisionFailedException(ex);
        }
    }
    
    /*
     * 
     */
    protected static void compareTextFiles(File expectedOutput, File actualRunOutput) 
    throws FileComparisionFailedException {
        try {
            FileInputStream actualRunFIS = new FileInputStream(actualRunOutput);
            FileInputStream expectedOutputFIS = new FileInputStream(expectedOutput);
//            int actualRunOutputFileSize = actualRunFIS.available();
//            int expectedOutputFileSize = expectedOutputFIS.available();
 
            StringBuffer errorResult = new  StringBuffer ();    
            BufferedReader actualRd = new BufferedReader(new InputStreamReader (actualRunFIS));
            BufferedReader expectedRd = new BufferedReader(new InputStreamReader (expectedOutputFIS));      
            

//            byte[] actualOutputBuffer = new byte[actualRunOutputFileSize];
//            byte[] expectedOutputBuffer = new byte[expectedOutputFileSize];
            int lineNo = 1;
            String actualLine = null;
            String expectedLine = null;
            boolean failed = false;
            while (true) {
                actualLine = actualRd.readLine();
                expectedLine = expectedRd.readLine();     
                if (actualLine == null && expectedLine == null) {
                    break;
                }
                if (actualLine != null  && expectedLine != null && !actualLine.equals(expectedLine)) {
                    errorResult.append("Line ");
                    errorResult.append(lineNo);
                    errorResult.append(":\n");
                    errorResult.append("\tExpected:");
                    errorResult.append(expectedLine);
                    errorResult.append("\n");
                    errorResult.append("\tActual:");
                    errorResult.append(actualLine);    
                    errorResult.append("\n");
                    failed = true;
                } else if (actualLine == null) {
                    errorResult.append("Line ");
                    errorResult.append(lineNo);
                    errorResult.append(":\n");
                    errorResult.append("\tExpected:");
                    errorResult.append(expectedLine);
                    errorResult.append("\n");
                    errorResult.append("\tActual is null \n");   
                    failed = true;
                } else if (expectedLine == null) {
                    errorResult.append("Line ");
                    errorResult.append(lineNo);
                    errorResult.append(":\n");
                    errorResult.append("\tExpected is null \n");
                    errorResult.append("\tActual:");
                    errorResult.append(actualLine);    
                    errorResult.append("\n");         
                    failed = true;
                } 
                
                lineNo++;                
            }

            if (failed){
                throw new FileComparisionFailedException("File comparision failed." + errorResult.toString());
            }
        } catch (Exception ex) {
            throw new FileComparisionFailedException(ex);
        }
    }
    
    /**
     * 
     * @param fileName
     * @return
     * @throws Exception
     */
    public static Document getDocument(String fileName) throws Exception {

        URL ipMsgFileURL = EngineDriver.class.getResource(fileName);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        Document doc = newBuilder().parse(ipMsgFile);

        return doc;
    }

    /**
     * 
     * @param elem
     * @return
     * @throws Exception
     */
    public static String getXMLString(Element elem) throws Exception {
        DOMSource src = new DOMSource(elem);
        TransformerFactory fact = TransformerFactory.newInstance();
        Transformer transformer = fact.newTransformer();
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);

        transformer.transform(src, dest);
        String s = writer.toString();
        return s;
    }

    /**
     * 
     * @param fileName
     * @return
     * @throws Exception
     */
    public static String getXMLString(String fileName) throws Exception {

        URL ipMsgFileURL = EngineDriver.class.getResource(fileName);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        Document doc = newBuilder().parse(ipMsgFile);
        Element elem = doc.getDocumentElement();

        DOMSource src = new DOMSource(elem);
        TransformerFactory fact = TransformerFactory.newInstance();
        Transformer transformer = fact.newTransformer();
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);

        transformer.transform(src, dest);
        String s = writer.toString();

        return s;
    }
    
    /**
     * 
     * @param messageExchangeId
     * @return
     */
    public static Transaction lookupTransaction(String messageExchangeId) {
        return (Transaction)transactionsMap.get(messageExchangeId);
    }
    
    /**
     * 
     * @param messageExchangeId
     * @param transaction
     */
    public static void registerTransaction(String messageExchangeId, Transaction transaction) {
        transactionsMap.put(messageExchangeId, transaction);
    }
    
    /**
     * 
     * @param messageExchangeId
     * @param operationName
     */
    public static void registerMsgExId(String messageExchangeId, String operationName) {
    	operationsMap.put(messageExchangeId, operationName);
    }
    
    /**
     * 
     * @param messageExchangeId
     * @return
     */
    public static Object unregisterMsgExId(String messageExchangeId) {
    	return operationsMap.remove(messageExchangeId);
    }
    
    /**
     * 
     * @param messageExchangeId
     */
    public static void setInitialMsgExId(String messageExchangeId) {
    	initialMsgExId = messageExchangeId;
    }
    
    /**
     * 
     * @return
     */
    public static String getInitialMsgExId() {
    	return initialMsgExId;
    }
    
    /**
     * 
     * @param messageExchangeId
     * @return
     */
    public static String getOperationName(String messageExchangeId) {
    	Object opNameObj = operationsMap.get(messageExchangeId);
    	if (opNameObj != null) {
    		return (String) opNameObj;
    	} else {
    		return null;
    	}
    }
    
    /*
     * 
     */
    private DeploymentBindings deploy(String testName) throws Exception {
        String filePath = System.getProperty("DEPLOYED.FOLDER");
        URL url = EngineDriver.class.getResource(filePath);
        File deployDir = new File(url.toURI().getPath());

        StatusProviderHelper statusProviderHelper = null;

        try {
            statusProviderHelper = new StatusProviderHelper("BPELSE Status",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE,
                    "BpelPersistence_engine", MBeanServerFactory
                    .createMBeanServer());
            statusProviderHelper.registerMBean();
        } catch (Exception ex) {
            throw new JBIException("Failed to register status provider MBean",
                    ex);
        }

        SUArtifacts suArtifacts = EngineHelper.deploy(testName, deployDir, new TestDeploymentLookup());

        Iterator bpItr = suArtifacts.getBPs().iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            mEng.addModel(bpelProcess, testName, testName);
        }
        
        DeploymentBindings deplBindings = new DeploymentBindings();
        Set inEventModelSet = suArtifacts.getInComingEventModel().entrySet();
        Iterator inEventModelIter = inEventModelSet.iterator();
        while (inEventModelIter.hasNext()) {
            Map.Entry inEventModelMapEntry = (Map.Entry) inEventModelIter.next();
            InComingKey key = (InComingKey) inEventModelMapEntry.getKey();
            InComingEventModel model = (InComingEventModel) inEventModelMapEntry.getValue();
            deplBindings.addInComingEventModel(testName, key, model);
            mEng.addStartActivityModel(model.getBPELProcess(), 
                    model.getStartElement(), model.getOperPattern());
        }
        
        return deplBindings;


    }

    /*
     * 
     */
    private void test(Properties props, DeploymentBindings depBindings) throws Exception {
        String methodName = props.getProperty("METHOD");
        UtilityClass utils = lookupUtilityClass(props);
        Method method = utils.getClass().getMethod(methodName,
                new Class[]{Properties.class, Engine.class, DeploymentBindings.class});
        
        // associate channel
        associateChannel(props, depBindings, utils);
        
        // trigger message
        method.invoke(utils, new Object[] {props, mEng, depBindings});
    }

    /*
     * 
     */
    private void recover(Properties props, DeploymentBindings depBindings) throws Exception {
        String methodName = props.getProperty("RECOVERY_METHOD", "recover");
        UtilityClass utils = lookupUtilityClass(props);
        Method method = utils.getClass().getMethod(methodName,
                new Class[]{Properties.class, Engine.class, DeploymentBindings.class,});
        
        // associate channel
        associateChannel(props, depBindings, utils);

        // trigger recovery
        method.invoke(utils, new Object[] {props, mEng, depBindings});
    }

    /*
     * 
     */
    private void associateChannel(Properties props, DeploymentBindings depBindings, 
            UtilityClass utils) throws Exception {
        String methodName = props.getProperty("CHANNEL_METHOD"); 
        Method method = utils.getClass().getMethod(methodName,
                new Class[] {Properties.class, Engine.class, 
                DeploymentBindings.class});
        method.invoke(utils, new Object[] {props, mEng, depBindings});
    }
    
    /*
     * 
     */
    private void setProperties(Properties props) {
    	mProps = props;
    }
    
    /*
     * 
     */
    protected void preRunSetup(String[] args) throws Exception {
    	/**
         * arg0: test base file path
         * arg1: test persistence crash point: 999 indicates to recover
         * arg2: properties file path
         */
        System.setProperty("DEPLOYED.FOLDER", args[1]);
        System.setProperty("PERSIST.CRASHPOINT", args[2]);
        System.setProperty("bpelse.properties", args[3]);
        System.setProperty(RECOVERY_TESTS_FLAG, "TRUE");
        
        String outFile = mProps.getProperty("DBSTEPS");
        File propFile = new File(args[0]);
        File baseDir = propFile.getParentFile().getParentFile();

        String expOutFilePath = 
        	baseDir.getPath() + File.separator + "output" + File.separator + outFile;
        mProps.setProperty(EXP_OUT_FILE_PATH, expOutFilePath);

        String actOutFilePath = 
        	baseDir.getPath() + File.separator + "temp" + File.separator + outFile;
        mProps.setProperty(ACT_OUT_FILE_PATH, actOutFilePath);
        File actOutFile = new File(actOutFilePath);

        // if the actual output file exists, delete it.
        if (actOutFile.exists()) {
        	actOutFile.delete();
        }

        // Create the temporary files directory
        File tmpPerExecParentDir = actOutFile.getParentFile();
        if (!tmpPerExecParentDir.exists()) {
        	tmpPerExecParentDir.mkdirs();
        }

        actOutFile.createNewFile();
        //System.setProperty("PERSIST.OUTPUT", actOutFilePath);
    }
    
    /*
     * 
     */
    protected void runTest(String[] args) throws Exception {
    	
    	String testName = mProps.getProperty("TEST_NAME");
        DeploymentBindings deplBindings = deploy(testName);
        
        if (Integer.valueOf(args[2]).intValue() == RECOVERY_ID) {
            recover(mProps, deplBindings);
        } else {
            test(mProps, deplBindings);
        }
    }
    
    /*
     * 
     */
    protected void postRunVerification(String[] args) throws Exception {
    	File actOutFile = new File(mProps.getProperty(ACT_OUT_FILE_PATH));
    	File expOutFile = new File(mProps.getProperty(EXP_OUT_FILE_PATH));
    	compareFiles(expOutFile, actOutFile);
    }

    /**
     * 
     * @param args
     * @return
     */
    public static int main(String[] args) {
    	try {            
    		EngineDriver driver = lookupEngineDriverClass(args[0]);
    		driver.preRunSetup(args);
    		driver.runTest(args);
    		driver.postRunVerification(args);
    		return SUCCESSFUL_EXECUTION;
    	} catch (FileComparisionFailedException fcfe) {
    		fcfe.printStackTrace();
    		return FILE_COMPARISION_FAILED;
    	} catch (Exception ex) {
    		ex.printStackTrace();
    		return 1;
    	}
    }
    
    /**
     * 
     *
     *
     * @author Sun Microsystems
     */
    protected static class FileComparisionFailedException extends Exception {

		public FileComparisionFailedException() {
			super();
		}

		public FileComparisionFailedException(String message, Throwable cause) {
			super(message, cause);
		}

		public FileComparisionFailedException(String message) {
			super(message);
		}

		public FileComparisionFailedException(Throwable cause) {
			super(cause);
		}
    }
}
