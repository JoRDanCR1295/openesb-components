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

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
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
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDeploymentLookup;
import com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.TallySet;
import com.sun.jbi.engine.bpel.util.SUArtifacts;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;


/**
 *
 * @author Sun Microsystems
 */
public class EngineDriver {
    private static final DocumentBuilderFactory mBuilderFactory =
            DocumentBuilderFactory.newInstance();
    static { mBuilderFactory.setNamespaceAware(true); }

    static final int RECOVERY_ID = 9999;
    static final int NO_RECOVERY_ID = 99995;
    static final int SUCCESSFUL_EXECUTION = 0;
    static final int FILE_COMPARISION_FAILED = 2;
    static final String DEBUG_PORT_KEY = "DebugPort";

    Engine mEng;
    private static HashMap transactionsMap = new HashMap();

    public EngineDriver() throws Exception {
        //mEng = SetUpHelper.getSetUp().getEngine();
        SetUpHelper setUp = new SetUpHelper();
        mEng = setUp.getEngine();
    }

    /** Creates a new instance of EngineDriver */
    static private void serialize(WSMessage msg, FileOutputStream fos) {
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
    
    private DeploymentBindings deploy(String testName) throws Exception {
        String filePath = System.getProperty("DEPLOYED.FOLDER");
        URL url = com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.EngineDriver.class.getResource(filePath);
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

//        String clazz = props.getProperty(
//                        "CLASS", "com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor.UtilityClass");
    	String clazz = "com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor.UtilityClass";
        return (UtilityClass) Class.forName(clazz).newInstance();
    }
    

    private static DocumentBuilder newBuilder() throws ParserConfigurationException {
        synchronized (mBuilderFactory) {
            return mBuilderFactory.newDocumentBuilder();
        }
    }

    public static Document getDocument(String fileName) throws Exception {

        URL ipMsgFileURL = com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.EngineDriver.class.getResource(fileName);
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

        URL ipMsgFileURL = com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.EngineDriver.class.getResource(fileName);
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

    public static int main(String[] args) {
        try {
            /**
             * arg0: test base file path
             * arg1: test persistence crash point: 999 indicates to recover
             * arg2: properties file path
             */
            File propFilePath = new File(args[0]);
            System.setProperty("DEPLOYED.FOLDER", args[1]);
            System.setProperty("PERSIST.CRASHPOINT", args[2]);
            System.setProperty("bpelse.properties", args[3]);

            String bpelsePropFile = System.getProperty("bpelse.properties", "bpelsemonitor.properties");
            InputStream is = SetUpHelper.class.getResourceAsStream(bpelsePropFile);
            Properties engineProps = new Properties();
            engineProps.load(is);
//            String BPELDebugPort = engineProps.getProperty(DEBUG_PORT_KEY);
//            System.setProperty(DEBUG_PORT_KEY, BPELDebugPort);
            
            File propFile = new File(propFilePath.toURI());
            File baseDir = propFile.getParentFile().getParentFile();

            Properties prop = new Properties();
            prop.load(propFilePath.toURI().toURL().openStream());
            String monitorStepsFile = prop.getProperty("EventSTEPS");
            String testName = prop.getProperty("TEST_NAME");
            File expectedDBStepsFile = new File(baseDir.getPath() + File.separator
                    + "output" + File.separator + monitorStepsFile);
            String actualDBStepsFilePath = baseDir.getPath() + File.separator
                    + "temp" + File.separator + monitorStepsFile;
            File actualDBStepsFile = new File(actualDBStepsFilePath);

            if (Integer.valueOf(args[2]).intValue() != RECOVERY_ID) {
                if (actualDBStepsFile.exists()) {
                    actualDBStepsFile.delete();
                }
                File tmpPerExecParentDir = actualDBStepsFile.getParentFile();
                if (!tmpPerExecParentDir.exists()) {
                    tmpPerExecParentDir.mkdirs();
                }
                actualDBStepsFile.createNewFile();
            }

            System.setProperty("MONITOR.OUTPUT", actualDBStepsFilePath);
            EngineDriver driver = new EngineDriver();
//            DebuggerClient debugClient = new DebuggerClient(actualDBStepsFile);
            try {
//                debugClient.attach();

//            ObjectChangeListener listener = driver.new Listener();
//            driver.mContext.bind(TestContext.END_OF_INSTANCE_LISTENER, listener);

                DeploymentBindings deplBindings = driver.deploy(testName);
                if (Integer.valueOf(args[2]).intValue() == RECOVERY_ID) {
                    driver.recover(prop, deplBindings);
                } else {
                    driver.test(prop, deplBindings);
                }
                //Wait till all the events have been processed
                int toWait = Integer.parseInt(System.getProperty("ToWait", "2000"));
                Thread.sleep(toWait);
                

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
                    return FILE_COMPARISION_FAILED;
                }

                return SUCCESSFUL_EXECUTION;
            } finally {
//                debugClient.detach();
//                ((BPELEngine) driver.mEng).disableDebugger();
                
            }
        } catch (Throwable ex) {
            ex.printStackTrace();
            return 1;
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
            StringTokenizer tokensForExpected = new StringTokenizer(expectedOutputString, "\n\r");
            StringTokenizer tokensForActualRun = new StringTokenizer(actualOutputString, "\n\r");
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
