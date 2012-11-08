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
 * @(#)BaseTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URL;
import java.rmi.server.UID;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.XmlResourceProviderPoolImpl;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyNonXATxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DummyTxManagerAndDataSource;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestContext;

public abstract class BaseTestCase extends TestCase {

    public static final String SENDMESSAGECOUNT = "SendMessageCount";
    private static final String RUNDURATION = "runDuration";
    private static final String CRASHPOINT = "crashpoint";
    private static final String MESSAGESTIMING = "msgsTiming";
    
    private static final Object RECOVERYBATCHSIZE = "recoveryBatchSize";

    private static String engine_configPropFile = "engines/engine-config.properties";
    
    EngineChannelSimulatorAdaptor mChannel;
    
    protected static final String EXPECTEDCOMPLEINSTANCES = "expectedCompleteInstances";
    protected Properties mConnProp = new Properties();
    protected static InitialContext ic;
    protected Properties testProp;
    
    String enginesDirectory;
    List enginesList;
    int messages;
    
    public BaseTestCase(String testName, String enginesDirectory) {
        super(testName);
        this.enginesDirectory = "engines/" + enginesDirectory + "/";
    }
    
    /**
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        try {
            Class thisClass = getClass();
            InputStream is = thisClass.getResourceAsStream(engine_configPropFile);
            mConnProp.load(is);
            String strIsClustered = mConnProp.getProperty(Engine.IS_CLUSTERED);
            System.setProperty(Engine.IS_CLUSTERED, strIsClustered);

            String strIsScalabilityEnabled = mConnProp.getProperty(Engine.SCALABILITY_LEVEL);
            System.setProperty(Engine.SCALABILITY_LEVEL, strIsScalabilityEnabled);

            DummyTxManagerAndDataSource dummyTMAndDS = new DummyTxManagerAndDataSource(mConnProp);
            DummyNonXATxManagerAndDataSource dummyNonXATMAndDS = 
                new DummyNonXATxManagerAndDataSource(mConnProp);
            BPELSERegistry registry = BPELSERegistry.getInstance();
            registry.register(TransactionManager.class.getName(), dummyTMAndDS);
            ic = new TestContext(dummyNonXATMAndDS, dummyTMAndDS, mConnProp);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        Utility.destroyConnectionPool();
        super.tearDown();
    }

    /**
     * @param callback TODO
     * @param config
     * @param testProp
     * @throws Exception
     */
    protected List runTestCase(String deploymentConfigPropFile,
                               String testPropFileName,
                               Callback callback) throws Exception {
        Utility.doCleanupPersisteneDB();

        ConfigurationInfo config = Utility.getDeploymentConfigurations(deploymentConfigPropFile, getClass());
        this.testProp = loadTestProperties(testPropFileName, getClass());

        /* register xml resource provider pool */
        registerXmlResourceProviderPool(testProp);

        /* create engines */
        this.enginesList = constructEngines(config, testProp, callback, false);

        /* define the service connections, if any (for projects that call sub business processes) */
        createServiceConnections(config, enginesList);

        /* create message router */
        CorrelationMessageRouter router = new CorrelationMessageRouter();

        this.messages = constructMessageAndAddToRouter(config, testProp, enginesList, router);

        /* start the router (this would start sending the messages to engines) */
        router.start();

        /* wait for the engines to process the messages */
        Iterator iter = enginesList.iterator();

        while (iter.hasNext()) {
            (((EngineInfo) iter.next()).getEngine()).join();
        }
        
        return enginesList;
    }
    
    protected Engine getEngineForRecovery(String deploymentConfigPropFile,
                                         String testPropFileName,
                                         Callback callback) throws Exception {
        
        ConfigurationInfo config = Utility.getDeploymentConfigurations(deploymentConfigPropFile, getClass());
        this.testProp = loadTestProperties(testPropFileName, getClass());

        /* register xml resource provider pool */
        registerXmlResourceProviderPool(testProp);

        /* create engines and messages and add to message router */
        this.enginesList = constructEngines(config, testProp, callback, true);

        /* define the service connections, if any (for projects that call sub business processes) */
        createServiceConnections(config, enginesList);
        
        if (enginesList.size() > 1) {
            throw new RuntimeException(
                    "Multiple Engines specified for single engine recovery test. Please specify one engine only");
        }
        return ((EngineInfo) enginesList.get(0)).getEngine().getEngine();
        
    }
    

    protected void createServiceConnections(ConfigurationInfo config, List enginesList) {
        Map map = config.getMessageEndPointConnectionMap();
        
        if (map == null) {
            return;
        }
        
        for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry e = (Map.Entry) i.next();
            System.out.println(e.getKey() + ": " + e.getValue());
            EngineSimulator engine = null;
            Iterator iter = enginesList.iterator();
            while(iter.hasNext()) {
                engine = ((EngineInfo) iter.next()).getEngine();
                engine.addSubBPInvokeConnectionMap((InComingKey)e.getKey(), (InComingKey)e.getValue());
            }
        }
    }
    
    
    private Properties loadTestProperties(String testPropFile, Class classs) throws IOException {
        InputStream is = classs.getResourceAsStream(testPropFile);
        Properties testProp = new Properties();
        testProp.load(is);
        return testProp;
    }

    private List constructEngines(ConfigurationInfo config,
                                  Properties testProp,
                                  Callback callback,
                                  boolean isSingleEngineRecovery) throws Exception {
        EngineSimulator engineSimulator = null;
        List enginesList = new ArrayList();
        
        String engineId = null;
        EngineInfo engineInfo = null;
        
        DeploymentBindings deplBindings = null;
        Enumeration enums = testProp.keys();
        
        String recoveryBatchSize = (String)testProp.get(RECOVERYBATCHSIZE);
        if (recoveryBatchSize != null) {
            System.setProperty(Engine.RECOVERY_BATCH_SIZE, recoveryBatchSize);
        }

        while (enums.hasMoreElements()) {
            engineId = (String) enums.nextElement();
            if (engineId.contains("engine") && engineId.contains("messages")) {

                engineSimulator = createEngineSimulator(testProp, engineId, config, callback, isSingleEngineRecovery);
                engineSimulator.start();
                
                deplBindings = deployProject(engineSimulator, config);
                engineSimulator.setDeploymentBindings(deplBindings);

                engineInfo = new EngineInfo(engineSimulator, deplBindings, engineId);
                enginesList.add(engineInfo);
            }
        }
        return enginesList;
    }
    
    
    private EngineSimulator createEngineSimulator(Properties testProp,
                                                  String engineId,
                                                  ConfigurationInfo config,
                                                  Callback callback,
                                                  boolean isSingleEngineRecovery) {

        String engineIdRunDuration = testProp.getProperty(engineId.substring(0, engineId.indexOf(".") + 1)
                + RUNDURATION);
        int engineRunDuration = Integer.parseInt(engineIdRunDuration) * 1000;

        CrashpointInfo crashpointInfo = getCrashPointInfo(engineId, isSingleEngineRecovery);

        int engineThreadPoolSize = config.getEngineThreadPoolSize();

        mConnProp.setProperty(Engine.JUNIT_ENGINEID, engineId.substring(0, engineId.indexOf(".")));
        return new EngineSimulator(this, mConnProp, engineRunDuration, engineThreadPoolSize, ic, callback,
                crashpointInfo);
    }
    
    
    private CrashpointInfo getCrashPointInfo(String engineId, boolean isSingleEngineRecovery) {
    	String strCrashpointInfo = (String) testProp.get(engineId.substring(0, engineId.indexOf(".") + 1) + CRASHPOINT);

    	CrashpointInfo crashpointInfo = null;
        
        if (strCrashpointInfo != null && !strCrashpointInfo.equals("")) {
            StringTokenizer st = new StringTokenizer(strCrashpointInfo, ",");

            String bpelFileName = st.nextToken().trim();
            int crashpoint = Integer.valueOf(st.nextToken().trim());

            boolean isCrashEnabled = !isSingleEngineRecovery;
            crashpointInfo = new CrashpointInfo(isCrashEnabled, crashpoint, bpelFileName);
        }
        
        return crashpointInfo;
    }
    
    private Map<String, Integer> getMessagesTimingInfo(String engineId) {
    	Map<String, Integer> map = new HashMap<String, Integer>();
    	String messagesTimingInfo = (String) testProp.get(engineId.substring(0, engineId.indexOf(".") + 1) + MESSAGESTIMING);

    	int delay = -1;
    	String messageId = null;
    	String token = null;
    	
    	int index = -1;
    	
        if (messagesTimingInfo != null && !messagesTimingInfo.equals("")) {
            StringTokenizer st = new StringTokenizer(messagesTimingInfo, ",");
            
            while (st.hasMoreTokens()) {
            	token = st.nextToken().trim();
            	index = token.indexOf("=");
            	messageId = token.substring(0, index);
            	delay = Integer.valueOf(token.substring(index + 1));
            	map.put(messageId, delay);
            }
        }
    	
    	return map;
    }
    
    private int constructMessageAndAddToRouter(ConfigurationInfo config,
                                             Properties testProp,
                                             List engineInfoList,
                                             CorrelationMessageRouter router) throws Exception {
        String messageId = null;
        JBIMessageWrapper messageWrapper = null;

        int messages = 0;
        
        Iterator iter = engineInfoList.iterator();
        EngineInfo engineInfo = null;
        
        String engineId = null;
        int messageDelay = -1;
        
        while (iter.hasNext()) {
            engineInfo = (EngineInfo) iter.next();
            
            engineId = engineInfo.getEngineId();
            
            Map<String, Integer> messagesTimingInfoMap = getMessagesTimingInfo(engineId);
            
            String csvListOfMessages = (String) testProp.get(engineId);
            StringTokenizer st = new StringTokenizer(csvListOfMessages, ",");
            
            Map messageIncomingKeyMap = config.getMessageInComingKeyMap();
            
            while (st.hasMoreTokens()) {
                messageId = st.nextToken().trim();
                DeploymentBindings.InComingKey incomingKey = (DeploymentBindings.InComingKey) messageIncomingKeyMap.get(messageId);
                
                if (messagesTimingInfoMap.size() > 0) {
                	messageDelay = messagesTimingInfoMap.get(messageId);
                }
                
                messageWrapper = constructMessage(engineInfo, config, incomingKey, messageId, messageDelay);
                
                if (null != messageWrapper) {
                    router.addMessage(messageWrapper);
                    messages++;
                }
            }
            
            JBIMessageWrapper endOfMessagesMarker = constructMessage(engineInfo, null, null, null, -1);
            router.addMessage(endOfMessagesMarker);
        }
        
        return messages;
    }
    
    
    private void registerXmlResourceProviderPool(Properties testProp) {
        // get the total messages count and create same number xml resource provider pool
        // because we spawn a new thread for each request.
        Enumeration enums = testProp.keys();
        int enginesCount = 0;
        String engineId = null;
        String csvListOfMessages = null;

        while (enums.hasMoreElements()) {
            engineId = (String) enums.nextElement();
            if (engineId.contains("engine") && engineId.contains("messages")) {
                csvListOfMessages = (String) testProp.get(engineId);
                StringTokenizer st = new StringTokenizer(csvListOfMessages, ",");
                enginesCount += st.countTokens();
            }
        }
        registerXmlResourceProviderPool(enginesCount);
    }
    
    protected void verifyResults() {
        try {
            String engIds = Utility.getEngineIds(enginesList);
            int expectedCompleteInstances = Integer.parseInt(testProp.getProperty(EXPECTEDCOMPLEINSTANCES));
            Iterator iter = enginesList.iterator();
            EngineSimulator engineSimulatorThread = null;
            
            System.out.println("Total Messages Sent = " + messages + " for total final complete instances = " + expectedCompleteInstances);
            
            int doneCount = 0;
            int totalDoneCount = 0;
            int totalRunningCount = 0;
            
            String query = null;
            String engId = null;
            
            while (iter.hasNext()) {
                engineSimulatorThread = ((EngineInfo) iter.next()).getEngine();
                engId = engineSimulatorThread.getEngine().getId();
                query = "select count(*) from STATE where ENGINEID = '" + engId
                        + "' and STATUS = 'DONE'";
                doneCount = Utility.getCount(query);
            
                System.out.println("Total Instances Completed by Engine [ " + engId + " ] = "
                        + doneCount);
                totalDoneCount += doneCount;
            }
            
            query = "select count(*) from STATE where STATUS = 'RUNNING' and ENGINEID IN (" + engIds
                    + ")";
            totalRunningCount = Utility.getCount(query);
            System.out.println("Total count of Incomplete Instances = " + totalRunningCount);

            assertEquals("Test Failed..Compelted Instances less than expected : " , expectedCompleteInstances, totalDoneCount);
            assertTrue("Test Failed..Instances Still Incomplete ", totalRunningCount == 0);

        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
     
    protected void verifyResultsSingleEngRecovery(int expectedRunningInstances, int expectedCompleteInstances) {
        
        try {
            int doneCount = 0;
            int totalRunningCount = 0;
            
            String query = null;
            
            query = "select count(*) from STATE where STATUS = 'DONE'";
            doneCount = Utility.getCount(query);

            query = "select count(*) from STATE where STATUS = 'RUNNING'";
            totalRunningCount = Utility.getCount(query);
            
            System.out.println("Count of Completed Instances = " + doneCount);
            System.out.println("Count of Incomplete Instances = " + totalRunningCount);

            assertEquals("Test Failed..Incomplete Instances does not match the expected : " , expectedRunningInstances, totalRunningCount);
            assertEquals("Test Failed..Compelted Instances does not match the expected : " , expectedCompleteInstances, doneCount);

        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
    
    private JBIMessageWrapper constructMessage(EngineInfo engineinfo,
                                               ConfigurationInfo config,
                                               InComingKey key, 
                                               String messageId,
                                               int messageDelay) throws Exception {
        EngineSimulator engine = engineinfo.getEngine();
        if (config == null) {
            return new JBIMessageWrapper(engine, null, null, null, -1);
        }
        DeploymentBindings deplBindings = engineinfo.getDeplBindings();
        InComingEventModel incomingEvntModel = deplBindings.getInComingEventModel(key);
        JBIMessageImpl jbiMsg = constructJBIMessage(incomingEvntModel, config, messageId);
        JBIMessageWrapper message = new JBIMessageWrapper(engine, jbiMsg, incomingEvntModel,
                new UID().toString(), messageDelay);
        return message;
    }

    private DeploymentBindings deployProject(EngineSimulator engine, ConfigurationInfo config) {
        engine.setTestEnvironment(config);
        
        URL url = getClass().getResource(config.getDeploymentDir());
        File deployDir = new File(url.getFile());
        DeploymentBindings deplBindings = null;
        try {
            deplBindings = engine.deploy(deployDir);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("Exception thrown during deployment " + e.getMessage());
        }
        return deplBindings;
    }
    
    private JBIMessageImpl constructJBIMessage(InComingEventModel model,
                                               ConfigurationInfo config, String messageId) throws Exception {
        String testFolder = config.getTestFolder();
        MessageInfo messageInfo = (MessageInfo) config.getMessageToInputFileMap().get(messageId);
        
        String inputFileName = messageInfo.getMessageFile();
        String ipMsgQName = messageInfo.getMessageQName();
        
        String inputXML = getXMLString(testFolder, inputFileName);
        System.out.println("constructed message [" + inputXML + "]");
        Document doc = getDocument(testFolder, inputFileName);

        QName ipMsgType = QName.valueOf(ipMsgQName);

        Message wsdlMessage = model.getBPELProcess().getWSDLMessage(ipMsgType);

        JBIMessageImpl jbiMsg = new JBIMessageImpl(doc, wsdlMessage);
        return jbiMsg;
    }

    private Document getDocument(String testFolder, String fileName) throws Exception {
        String ipMsgFilePath = testFolder + fileName;
        URL ipMsgFileURL = getClass().getResource(ipMsgFilePath);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        Document doc = docFactory.newDocumentBuilder().parse(ipMsgFile);

        return doc;
    }

    private String getXMLString(String testFolder, String fileName) throws Exception {

        String ipMsgFilePath = testFolder + fileName;
        URL ipMsgFileURL = getClass().getResource(ipMsgFilePath);
        File ipMsgFile = new File(ipMsgFileURL.toURI());
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        Document doc = docFactory.newDocumentBuilder().parse(ipMsgFile);
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
    
    private void registerXmlResourceProviderPool(int size) {
        // Register a XmlResourceProviderPool with the Registry. We use the name of
        // the interface XmlResourceProviderPool to register the pool.
        BPELSERegistry registry = BPELSERegistry.getInstance();

        XmlResourceProviderPoolImpl xmlResProviderPool;
        try {
            xmlResProviderPool = new XmlResourceProviderPoolImpl(size);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        registry.register(XmlResourceProviderPool.class.getName(), xmlResProviderPool);
    }
}

class MessageInfo {
    String messageQName;
    String messageFile;
    
    public MessageInfo(String messageQName, String messageFile) {
        this.messageQName = messageQName;
        this.messageFile = messageFile;
    }
    public String getMessageFile() {
        return messageFile;
    }

    public String getMessageQName() {
        return messageQName;
    }
}

class JBIMessageWrapper {

    JBIMessageImpl mJbiMsg;
    InComingEventModel mModel;
    EngineSimulator mEngine;
    String mMessageId;
    int mMessageDelay;
    
    public JBIMessageWrapper(EngineSimulator engine, JBIMessageImpl jbiMsg, InComingEventModel model, String messageId, int messageDelay) {
        this.mEngine = engine;
        this.mJbiMsg = jbiMsg;
        this.mModel = model;
        this.mMessageId = messageId;
        this.mMessageDelay = messageDelay;
    }

    public String getMessageId() {
        return mMessageId;
    }

    public JBIMessageImpl getJBIMessage() {
        return mJbiMsg;
    }

    public InComingEventModel getInComingEventModel() {
        return mModel;
    }

    public EngineSimulator getEngine() {
        return mEngine;
    }
    
    public int getMessageDelay() {
    	return mMessageDelay;
    }
}

class ConfigurationInfo {
    String testFolder;
    String deploymentDir;
    String outputFileName;
    int engineThreadPoolSize;

    Map messageInComingKeyMap;
    Map messageEndPointConnectionMap;
    Map messageToInputFileMap;
    Map outputKeyToOutputMessageMap;

    public String getOutputFileName() {
        return outputFileName;
    }

    public void setOutputFileName(String outputFileName) {
        this.outputFileName = outputFileName;
    }

    public String getDeploymentDir() {
        return deploymentDir;
    }

    public void setDeploymentDir(String deploymentDir) {
        this.deploymentDir = deploymentDir;
    }

    public String getTestFolder() {
        return testFolder;
    }

    public void setTestFolder(String testFolder) {
        this.testFolder = testFolder;
    }

    public Map getMessageInComingKeyMap() {
        return messageInComingKeyMap;
    }

    public void setMessageInComingKeyMap(Map messageInComingKeyMap) {
        this.messageInComingKeyMap = messageInComingKeyMap;
    }

    public Map getMessageEndPointConnectionMap() {
        return messageEndPointConnectionMap;
    }

    public void setMessageEndPointConnectionMap(Map messageEndPointConnectionMap) {
        this.messageEndPointConnectionMap = messageEndPointConnectionMap;
    }

    public int getEngineThreadPoolSize() {
        return engineThreadPoolSize;
    }

    public void setEngineThreadPoolSize(int engineThreadPoolSize) {
        this.engineThreadPoolSize = engineThreadPoolSize;
    }

    public Map getMessageToInputFileMap() {
        return messageToInputFileMap;
    }

    public void setMessageToInputFileMap(Map messageToInputFileMap) {
        this.messageToInputFileMap = messageToInputFileMap;
    }
    
    public Map getOutputKeyToOutputMessageMap() {
        return outputKeyToOutputMessageMap;
    }
    
    public void setOutputKeyToOutputMessageMap(Map outputKeyToOutputMessageMap) {
        this.outputKeyToOutputMessageMap = outputKeyToOutputMessageMap;
    }
}
