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

package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.io.File;
import java.io.FileInputStream;
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
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDeploymentLookup;
import com.sun.jbi.engine.bpel.util.SUArtifacts;


/**
 *
 * @author Sun Microsystems
 */
public class EngineDriver {
    private static final DocumentBuilderFactory mBuilderFactory =
            DocumentBuilderFactory.newInstance();
    static { mBuilderFactory.setNamespaceAware(true); }

    static enum TestMode {PERSIST, CRASH, RECOVER};
    static final int NO_CRASH_RECOVERY_ID = -1;
    
    static final int SUCCESSFUL_EXECUTION = 0;
    static final int FILE_COMPARISION_FAILED = 2;
    public static final String RECOVERY_TESTS_FLAG = "RECOVERY_TESTS";
    public static final String PERSIST_CRASHPOINT = "PERSIST.CRASHPOINT";
    public static final String TEST_NAME = "TEST_NAME";
    public static final String TEST_ID = "TEST_ID";
    public static final String CRMP_ID = "CRMP_ID";
    
    Engine mEng;
    private static HashMap transactionsMap = new HashMap();

    public EngineDriver() throws Exception {
        System.setProperty(Engine.IS_CLUSTERED, "false");
        System.setProperty(Engine.SCALABILITY_LEVEL, "none");

        SetUpHelper setUp = new SetUpHelper();
        
        mEng = setUp.getEngine();
    }
    
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

        SUArtifacts suArtifacts = EngineHelper.deploy(testName, deployDir,
                new TestDeploymentLookup());

        Iterator bpItr = suArtifacts.getBPs().iterator();
        while (bpItr.hasNext()) {
            RBPELProcess bpelProcess = (RBPELProcess) bpItr.next();
            mEng.addModel(bpelProcess, testName, testName);
        }
        mEng.setSUState(testName, Engine.SUStates.Started);
        
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

    private void associateChannel(Properties props, DeploymentBindings depBindings, 
            UtilityClass utils) throws Exception {
        String methodName = props.getProperty("CHANNEL_METHOD"); 
        Method method = utils.getClass().getMethod(methodName,
                new Class[] {Properties.class, Engine.class, 
                DeploymentBindings.class});
        method.invoke(utils, new Object[] {props, mEng, depBindings});
    }
    
    private UtilityClass lookupUtilityClass(Properties props) throws Exception {

        String clazz = props.getProperty(
                        "CLASS", "com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.UtilityClass");
        return (UtilityClass) Class.forName(clazz).newInstance();
    }
    

    private static DocumentBuilder newBuilder() throws ParserConfigurationException {
        synchronized (mBuilderFactory) {
            return mBuilderFactory.newDocumentBuilder();
        }
    }

    public static Document getDocument(String fileName) throws Exception {

        URL ipMsgFileURL = EngineDriver.class.getResource(fileName);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        Document doc = newBuilder().parse(ipMsgFile);

        return doc;
    }

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
    
    public static Transaction lookupTransaction(String messageExchangeId) {
        return (Transaction)transactionsMap.get(messageExchangeId);
    }
    
    public static void registerTransaction(String messageExchangeId, Transaction transaction) {
        transactionsMap.put(messageExchangeId, transaction);
    }

    public static void main(String[] args) {
        try {
            /**
             * arg0: test base file path
             * arg1: test persistence crash point: 999 indicates to recover
             * arg2: properties file path
             */
            File propFilePath = new File(args[0]);
            System.setProperty("DEPLOYED.FOLDER", args[1]);
            System.setProperty(PERSIST_CRASHPOINT, args[2]);
            System.setProperty("TEST.MODE", args[3]);   
            System.setProperty(CRMP_ID, args[4]);
            
            System.setProperty("bpelse.properties", "persistenceBPELse.properties");
            System.setProperty(RECOVERY_TESTS_FLAG, "TRUE");
            
            File propFile = new File(propFilePath.toURI());
            File baseDir = propFile.getParentFile().getParentFile();

            Properties prop = new Properties();
            prop.load(propFilePath.toURI().toURL().openStream());
            String persistenceStepsFile = prop.getProperty("DBSTEPS");
            String testName = prop.getProperty(TEST_NAME);
            if (Utility.isEmpty(testName)) {
                testName = propFilePath.toURI().toURL().toString();
                System.setProperty(TEST_NAME, testName);
            }
            // set test ID could be used by as CRMP ID for tests that have reply
            String propertyFileName = propFile.getName();
            int hashId = propFilePath.toURI().toURL().toString().hashCode();
            String testIDVal = propertyFileName + "- crashpoint=" + System.getProperty(PERSIST_CRASHPOINT) + "hashId =" + hashId;
            System.setProperty(TEST_ID, testIDVal);
            
            File expectedDBStepsFile = new File(baseDir.getPath() + File.separator
                    + "output" + File.separator + persistenceStepsFile);
            String actualDBStepsFilePath = baseDir.getPath() + File.separator
                    + "temp" + File.separator + persistenceStepsFile;
            File actualDBStepsFile = new File(actualDBStepsFilePath);

            if (!args[3].equals(TestMode.RECOVER.toString())) {
                if (actualDBStepsFile.exists()) {
                    actualDBStepsFile.delete();
                }
                File tmpPerExecParentDir = actualDBStepsFile.getParentFile();
                if (!tmpPerExecParentDir.exists()) {
                    tmpPerExecParentDir.mkdirs();
                }
                actualDBStepsFile.createNewFile();
            }

            System.setProperty("PERSIST.OUTPUT", actualDBStepsFilePath);
            EngineDriver driver = new EngineDriver();

//            ObjectChangeListener listener = driver.new Listener();
//            driver.mContext.bind(TestContext.END_OF_INSTANCE_LISTENER, listener);

            DeploymentBindings deplBindings = driver.deploy(testName);
            if (args[3].equals(TestMode.RECOVER.toString())) {
                driver.recover(prop, deplBindings);
            } else {
                driver.test(prop, deplBindings);
            }

//            synchronized (driver.mInstanceCompletionWaitObj) {
//                if (!driver.mFlag) {
//                    String timeToWait = prop.getProperty("TIME_OUT");
//                    long timeOut = 0;
//                    try {
//                        Long timer = Long.parseLong(timeToWait);
//                        timeOut = timer.longValue() * 1000;
//                    } catch (Exception e) {
//                        System.out.println(e.getMessage());
//                    }
//                    driver.mInstanceCompletionWaitObj.wait(timeOut);
//                }
//            }

            if (!compareFiles(expectedDBStepsFile, actualDBStepsFile)) {
                System.out.println("file comparision failed");
                System.exit(FILE_COMPARISION_FAILED);
            }

            System.exit(SUCCESSFUL_EXECUTION);
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
    }

    private static boolean compareFiles(File expectedOutput, File actualRunOutput) {
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
                return true;
            }
            StringTokenizer tokensForExpected = new StringTokenizer(expectedOutputString, "\n");
            StringTokenizer tokensForActualRun = new StringTokenizer(actualOutputString, "\n");
            TallySet expectedTokensSet = new TallySet(tokensForExpected);
            TallySet actualTokensSet = new TallySet(tokensForActualRun);
            boolean retVal = expectedTokensSet.compareOutput(actualTokensSet);
            return retVal;
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return false;
    }
}
