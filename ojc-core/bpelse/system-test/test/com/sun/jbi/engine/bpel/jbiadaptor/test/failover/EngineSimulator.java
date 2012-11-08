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
 * @(#)EngineSimulatorForCorrelation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import com.sun.jbi.engine.bpel.core.bpel.engine.EPReferenceComposer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import java.io.File;
import java.io.StringWriter;
import java.net.URL;
import java.rmi.server.UID;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServerFactory;
import javax.naming.InitialContext;
import javax.transaction.NotSupportedException;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import junit.framework.TestCase;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.EngineHelper;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Channel;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.impl.BPELSEException;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDeploymentLookup;
import com.sun.jbi.engine.bpel.jbiadaptor.test.scalability.BPELMemoryMonitorTestImpl;
import com.sun.jbi.engine.bpel.jbiadaptor.test.scalability.DebuggerSimulator;
import com.sun.jbi.engine.bpel.util.SUArtifacts;

public class EngineSimulator extends Thread {

    public static final String PERSISTENCEENABLED = "PersistenceEnabled";

    protected static final String CRASH = "crash";
    protected static final String ENGINERUNDURATION = "EngineRunDuration";

    
    protected Channel mChannel;
    protected MessageContainer mContainer = null;
    protected InComingEventModel mModel = null;
    boolean isRunning = true;
    boolean isStarted = false;
    Engine mEng;
    String mtestFolder;
    String mFileName;
    boolean mIsPersistEnabled = false;
    int mEngineRunDuration = 60 * 1000;
    int mEngineThreadPoolSize = 1;
    
    private Callback mCallback = null;
    private BaseTestCase mTestCase;
    
    protected static Map mTransactionsMap = Collections.synchronizedMap(new HashMap());

    Properties mConnProp = null;
    InitialContext mInitialContext;
    LinkedList mMessageContainerList = new LinkedList();
    DeploymentBindings mDeplBindings;
    
    Map mOutputKeyToOutputMessageMap;

    // To map the message exchange id to business process
    Map mMsgExchangeToBPELProcessMap = Collections.synchronizedMap(new HashMap());
    
    // This maps the service connections i.e it maps the consumes and provides entries
    // in the jbi.xml as per the process definition. If a main business process invokes
    // a sub-business process, this map will link the two endpoints (invoke from parent
    // business process to receive of sub- bp)
    Map mSubBPInvokeConnectionMap =  Collections.synchronizedMap(new HashMap());
    
    // this map the message exchange id to incoming event model of the business process
    Map mSubBPInvokesMsgExchangeToIncomingEventModelMap = Collections.synchronizedMap(new HashMap());
    
    // this map is used to keep the message ex id to crmp id map and is needed while constructing the 
    // status message for two-way invoke from bp to engine channel (or sub-process, as the case may be)
    // The reason why this map is required is that for status message api on engine channel is passed
    // only the message exchange id. Earlier design was sending the crmp id for two-way invokes only and 
    // was used for sub-bp recovery. But the scalability design need this to be passed for one-way invokes
    // and also for the status messages.
    Map mMsgExIdToCRMPIDMap = Collections.synchronizedMap(new HashMap<String, String>());
    
    CrashpointInfo mCrashpointInfo;
    
    public EngineSimulator(BaseTestCase testCase, Properties connProp, int engineRunDuration, int engineThreadPoolSize,
            InitialContext ic, Callback callback, CrashpointInfo crashpointInfo) {
        this.mTestCase = testCase;
        this.mConnProp = connProp;
        this.mIsPersistEnabled = Boolean.parseBoolean(connProp.getProperty(PERSISTENCEENABLED));
        this.mEngineThreadPoolSize = engineThreadPoolSize;
        this.mEngineRunDuration = engineRunDuration;
        this.mInitialContext = ic;
        this.mEng = new EngineImpl(connProp, ic);
        this.mEng.preStart();

        if (crashpointInfo != null) {
            setDebugger(crashpointInfo);
        }

        this.mCrashpointInfo = crashpointInfo;
        this.mCallback = callback;
        this.mEng.setBpelMemoryMonitor(new BPELMemoryMonitorTestImpl());
        associateEngineChannel();
    }

    /**
     * attach debugger
     * 
     * @param crashpointInfo
     */
    private void setDebugger(CrashpointInfo crashpointInfo) {
        DebuggerSimulator debugger = new DebuggerSimulator(mEng, crashpointInfo);
        try {
            String strIsDebugEnabled = mConnProp.getProperty(Engine.DEBUG_ENABLED, "false");
            boolean isDebugEnabled = strIsDebugEnabled == "false" ? false : true;

            if (isDebugEnabled) {
                ((EngineImpl)mEng).setDebugger(debugger);
                debugger.setRemoteDebugger(debugger);
            }

        } catch (Exception e) {
            System.out.print("Exception in atttaching the debugger");
            e.printStackTrace();
            System.exit(1);
        }

    }

    /**
     * Set test environment.
     * 
     * @param config
     */
    public void setTestEnvironment(ConfigurationInfo config) {
        
        this.mtestFolder = config.getTestFolder();
        this.mOutputKeyToOutputMessageMap = config.getOutputKeyToOutputMessageMap(); 
    }
    
    /**
     * Deploy the artifacts
     * 
     * @param deployDir
     * @return
     * @throws JBIException
     * @throws BPELSEException
     */
    public DeploymentBindings deploy(File deployDir) throws JBIException, BPELSEException {

        StatusProviderHelper statusProviderHelper = null;

        try {
            statusProviderHelper = new StatusProviderHelper("BPELSE Status",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE, "Bpel_engine",
                    MBeanServerFactory.createMBeanServer());
            statusProviderHelper.registerMBean();
        } catch (Exception ex) {
            throw new JBIException("Failed to register status provider MBean", ex);
        }

        String testName = "Assign1Test_test1";
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
    
    public Engine getEngine() {
        return mEng;
    }
    
    /**
     * sets the deployment bindings
     * 
     * @param depBindings
     */
    public void setDeploymentBindings(DeploymentBindings depBindings) {
        this.mDeplBindings = depBindings;
    }
    
    /**
     * Create parent bp endpoint connection to the sub-business process
     * 
     * @param parentKey
     * @param subBpKey
     */
    public void addSubBPInvokeConnectionMap(InComingKey parentKey, InComingKey subBpKey) {
        mSubBPInvokeConnectionMap.put(parentKey, subBpKey);
    }
    
    
    /**
     * Associate Engine channel. This will act as intercepter for all the communication
     * from the engine.
     */
    private void associateEngineChannel() {
        
        mChannel = new Channel() {
			public void reply(MessageContainer msgContainer) {
                String msgExId = msgContainer.getId();
                InComingEventModel model = (InComingEventModel) mSubBPInvokesMsgExchangeToIncomingEventModelMap.get(msgExId);
                if (model != null) {
                    //System.out.println("Reply called on two way invoke to sub-bp..");
                    // this indicate this is reply on two way invoke to a sub-bp. 
                    handleSubBPInvokeReply(msgContainer, model);
                } else {
                    //System.out.println("One way Reply ..");
                    String sIsClustered = System.getProperty(Engine.IS_CLUSTERED);
                    boolean isClustered = sIsClustered != null && sIsClustered.equals("true") ? true : false;

                    if (isClustered) {
                        // this indicate one way reply; this is not supported for cluster 
                        throw new RuntimeException("Exception: Not supported");
                    } 
                    // it is non cluster, echo the message back.
                    RBPELProcess process = (RBPELProcess) mMsgExchangeToBPELProcessMap.get(msgExId);
                    sendDoneStatus(msgContainer, msgExId, process);
                }
            }

            public Object invoke(MessageContainer msgContainer,
                                 RuntimePartnerLink partnerLink,
                                 QName operation1,
                                 boolean oneWay,
                                 RBPELProcess process) {
                
            	String operation = operation1.getLocalPart();
                String msgExId = new UID().toString();

                mMsgExIdToCRMPIDMap.put(msgExId, msgContainer.getCRMPInvokeId());
                
                if (oneWay) {
                    //System.out.println("One way Invoke called on engineChannel..");
                    handleOneWayInvoke(msgContainer, partnerLink, operation, process, msgExId);
                } else {
                    //System.out.println("Two way Invoke called on engineChannel.. ");

                    RuntimePartnerLink.InternalEPR epr = (RuntimePartnerLink.InternalEPR)partnerLink.getServiceRef();

                    InComingKey key = mDeplBindings.createInComingBindingsKey(epr.getService(), epr.getEndPoint(), operation);
                    InComingKey subBPIncomingModelKey = (InComingKey) mSubBPInvokeConnectionMap.get(key);
                    
                    if (subBPIncomingModelKey != null) {
                        System.out.println("Forwarding the request to sub bp.. ");
                        InComingEventModel model = mDeplBindings.getInComingEventModel(subBPIncomingModelKey);
                        mMsgExchangeToBPELProcessMap.put(msgExId, process);
                        
                        handleTwoWaySubBPInvoke(msgContainer, model, msgExId);
                    } else {
                        // we are intercepting a two way invoke (which is not to sub-bp)
                        // we will echo the same message back to the originating process
                        handleTwoWayInvoke(msgContainer, msgExId, process);
                    }
                }
                return msgExId;
            }

            public void sendFault(QName faultName, MessageContainer msgContainer) {
                // TODO Auto-generated method stub
            }

            public void sendRequestError(String msgExchangeId, Exception error) {
                //System.out.println("Error for Request - Error Details : " + error.getMessage());
            	handleInOnlyRequestStatus(msgExchangeId, false);

            }

            public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
                //System.out.println("Done Status for In-Only Request called .. ");
            	handleInOnlyRequestStatus(msgExchangeId, true);
            }

			public void sendResponseDoneStatus(String msgExchangeId) {
                //System.out.println("Done Status for In-Only Request called .. ");
                String crmpId = (String)mMsgExIdToCRMPIDMap.get(msgExchangeId);
                
                MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(msgExchangeId, crmpId, null);
                // This is used only for one way invokes. The reply unit will ignore it
                InComingEventModel model = (InComingEventModel) mSubBPInvokesMsgExchangeToIncomingEventModelMap.get(msgExchangeId);

                // this response done status could be for two way invoke done after the response is received
                // the process doing two way invoke. The done sent here is useful only in the context of
                // sub-bp invoke. If there is no sub-bp i.e. this engine simulator is intercepting the 
                // two way invoke, this done can be dropped.
                if (model != null) {
                    ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                            model.getBPELProcess(), Event.DONE, msgExchangeId);
                    mEng.process(event, statusContainer);
                }
            }

            public void sendResponseErrorStatus(String msgExchangeId, Exception ex) {
                System.out.println("Error for Request - Error Details : " + ex.getMessage());
                completeTransaction(msgExchangeId, false);
            }

            /**
             * @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
             */
            public DocumentFragment getExternalEndPoint(PartnerLink partnerLink) {
                return null;
            }

            public void sendKPIMEx(QName interfaceName, String operation,
                    String doc) throws MessagingException {
                // no need to implement for Testdriver
            }

            public void installServiceQualities(String suName, String suPath)
                    throws DeploymentException {
                // no need to implement for Testdriver
            }

            public void uninstallServiceQualities(String suName) {
                // no need to implement for Testdriver
            }

            public EPReferenceComposer getEPRComposer() {
                // TODO Auto-generated method stub
                return null;
            }

            public DocumentFragment getEndpointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
                // TODO Auto-generated method stub
                return null;
            }

            public ServiceEndpoint resolveEndpointReference(DocumentFragment docFrag) {
                // TODO Auto-generated method stub
                return null;
            }
        };
        
        mEng.setOutChannel(mChannel);
    }
    
    /**
     * complete transaction
     * 
     * @param msgExchangeId
     * @param isDone
     */
    private void completeTransaction(String msgExchangeId, boolean isDone) {
        // got the done from the engine for the in-only message, commit the transaction.
        Transaction transaction = (Transaction) mTransactionsMap.get(msgExchangeId);
        
        if (transaction == null) {
            // null means no transaction is defined, no need to do anything.
            return;
        }
        
        try {
            if (isDone) {
                transaction.commit();
            } else {
                transaction.rollback();
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        
    }

    /**
     * compare the result with expected output, also send done or error status
     * 
     * @param container
     * @param process
     * @param msgExId
     */
    private void handleOneWayInvoke(MessageContainer container,
                                           RuntimePartnerLink partnerLink,
                                           String operation,
                                           RBPELProcess process,
                                           String msgExId) {
        try {
            verifyOutput(container, partnerLink, operation);
        } catch (Exception e1) {
            e1.printStackTrace();
            sendErrorStatus(e1, msgExId, process, container.getCRMPInvokeId());
        }
        
        boolean sendStatus = true;
        
        if (mCallback != null) {
            sendStatus = mCallback.oneWayInvokeCallback(this, container, process, msgExId);
        }
        
        // TODO the crash case need to be handled. the crash test cases 
        // need to implement the callback interface and intercept the callback 
        // and reply to send the status or not. The way this framework is built that
        // after some configurable period the engine would be crashed and all the
        // running instances will be picked by the other live engine. Hence 
        // not sending the status would prevent the instance from completing.
        //if (!isCrashEnabled) {
        if (sendStatus) {
            sendDoneStatus(container, msgExId, process);
        }
    }
    
    /**
     * handle two way invoke
     * 
     * @param msgContainer
     * @param msgExId
     * @param process
     */
    private void handleTwoWayInvoke(MessageContainer msgContainer, String msgExId, RBPELProcess process) {
        boolean sendResponse = true;
        
        if (mCallback != null) {
            sendResponse = mCallback.twoWayInvokeCallback(this, msgContainer, process, msgExId);
        }
        
        if (sendResponse) {
            sendResponseMessage(msgContainer, process, msgExId);
        }
    }

    
    /**
     * Construct a message container for sub-bp, put a msg exchange id and 
     * incoming event model for the sub-bp IMA and send it to the engine.
     * 
     * @param msgContainer
     * @param model
     * @param megExId
     */
    private void handleTwoWaySubBPInvoke(MessageContainer msgContainer, InComingEventModel model, String msgExId) {
        boolean sendResponse = true;
        
        if (mCallback != null) {
            sendResponse = mCallback.twoWaySubBPInvokeCallback(this, msgContainer, model, msgExId);
        }
        
        if (sendResponse) {
            sendSubBPRequestMessage(msgContainer, model, msgExId);
        }
    }

    
    /**
     * Send the reply of the sub business process back to parent bp.
     * 
     * @param container
     * @param model
     * @throws SystemException 
     */
    private void handleSubBPInvokeReply(MessageContainer container,
                                        InComingEventModel model) {
        if (mCallback != null) {
            mCallback.replyCallback(this, container, model);
        }
        
        String meId = container.getId();
        
        Transaction tx = (Transaction)mTransactionsMap.get(meId);
        container.setTransaction(tx);
        RBPELProcess parentBussProcess = (RBPELProcess) mMsgExchangeToBPELProcessMap.get(meId);
        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(parentBussProcess,
                Event.REPLY_FAULT, meId);
        mEng.process(event, container);
    }
    
    /**
     * Complete the transaction and provide callback to the junit, if the callback object
     * is not null.
     * 
     * @param msgExchangeId
     * @param isDone
     */
    private void handleInOnlyRequestStatus(String msgExchangeId, boolean isDone) {
        
        if (mCallback != null) {
        	mCallback.sendInOnlyRequestStatusCallback(this, msgExchangeId);
        }
        
        completeTransaction(msgExchangeId, isDone);
	}
    
    public void run() {
        Set engineAliveTaskSet = startEngineAliveTasks();
        Set sendClientThreadSet = new HashSet();

        // wait to for the message creation
        synchronized (mMessageContainerList) {
            try {
                mMessageContainerList.wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        try {
            Thread t = null;

            int i = 0;
            while (true) {
                MessageContainerWrapper containerWrapper = null;

                synchronized (mMessageContainerList) {
                    if (!mMessageContainerList.isEmpty()) {
                        containerWrapper = (MessageContainerWrapper) mMessageContainerList.removeFirst();
                        // this means no more messages to process
                        if (containerWrapper.getMessageContainer() == null) {
                            // this indicate that there are more messaegs coming from router for
                            // this engine.
                            break;
                        }
                    } else {
                        try {
                            mMessageContainerList.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                        continue;
                    }
                }

                t = new MessageSender(containerWrapper);
                
                // need to specify name so that during crash by the debugger the thread can be identified
                // as junit thread the engine. Also the thread name should be unique as the dummy transaction 
                // manager uses thread name for storing transaction.
                
                String threadName = t.getName();
                t.setName("BPELSE_JUNIT_THREAD_" + threadName);
                
                t.start();

                sendClientThreadSet.add(t);
            }

            Iterator iter = sendClientThreadSet.iterator();

            while (iter.hasNext()) {
                t = (Thread) iter.next();
                t.join();
            }
            
            if (mCrashpointInfo != null && mCrashpointInfo.isCrashEnabled()) {
                System.out.println("Crashing the Engine : " + mEng.getId());
            } else {
                // TODO need to revisit the following, is the following sleep really required.
                sleep(mEngineRunDuration);
                System.out.println("Shutting down the Engine : " + mEng.getId());
            }

            killEngineAliveTasks(engineAliveTaskSet);
            
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Start engine alive tasks
     * 
     * @return
     */
    private Set startEngineAliveTasks() {

        if (!isClustered()) {
            return null;
        }
        
        // Create a timer task for updating the engine heart-beat EngineThreadPoolSize
        Set engineAliveTaskThreads = new HashSet();

        if (mIsPersistEnabled) {
            EngineAliveTask engineAliveTask = null;
            for (int i = 0; i < mEngineThreadPoolSize; i++) {
                engineAliveTask = new EngineAliveTask();

                engineAliveTask.setName("ENGINE_ALIVE_THREAD_" + engineAliveTask.getName());
                engineAliveTaskThreads.add(engineAliveTask);
                engineAliveTask.start();
            }
        }
        return engineAliveTaskThreads;
    }

    /**
     * @param engineAliveTaskSet
     * @throws InterruptedException
     */
    private void killEngineAliveTasks(Set engineAliveTaskSet) throws InterruptedException {
        if (!isClustered()) {
            return;
        }
        
        Iterator iter2 = engineAliveTaskSet.iterator();
        EngineAliveTask engineAliveTask = null;
        while(iter2.hasNext()) {
            engineAliveTask = (EngineAliveTask) iter2.next();
            // kill the engine alive thread
            if (engineAliveTask != null) {
                engineAliveTask.killIt();
                engineAliveTask.join();
            }
        }
    }
    
    /**
     * @return
     */
    private boolean isClustered() {
        String sIsClustered = System.getProperty(Engine.IS_CLUSTERED);
        return sIsClustered != null && sIsClustered.equals("true") ? true : false;
    }
    
    /**
     * send message
     * 
     * @param message
     * @throws Exception
     */
    public void sendMessage(JBIMessageWrapper message) throws Exception {

        MessageContainerWrapper containerWrapper = null;
        if (message.getJBIMessage() == null) {
            // this indicate end of messages for this engine.
            containerWrapper = new MessageContainerWrapper(null, null, -1);
        } else {

            MessageContainer messageContainer = MessageContainerFactory.createMessage(
                    message.getMessageId(), message.getJBIMessage(), null, null);

            final InComingEventModel model = message.getInComingEventModel();

            containerWrapper = new MessageContainerWrapper(messageContainer, model, message.getMessageDelay());
            mMsgExchangeToBPELProcessMap.put(message.getMessageId(), model.getBPELProcess());
        }

        synchronized (mMessageContainerList) {
            mMessageContainerList.addLast(containerWrapper);
            mMessageContainerList.notify();
        }
    }

    /**
     * send done status
     * 
     * @param container
     * @param msgExId
     * @param process
     */
    public void sendDoneStatus(MessageContainer container, String msgExId, RBPELProcess process) {
        MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(msgExId, container.getCRMPInvokeId(),
                null);
        // This is used only for one way invokes. The reply unit will ignore it
        statusContainer.setTransaction(container.getTransaction());
        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                process, Event.DONE, statusContainer.getId());
        mEng.process(event, statusContainer);
    }
    
    
    /**
     * Send error status
     * 
     * @param e
     * @param msgExId
     * @param process
     * @param crmpId
     */
    public void sendErrorStatus(Exception e, String msgExId, RBPELProcess process, String crmpId) {
        MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(msgExId, e.getMessage(), crmpId, null);
        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process, Event.ERROR,
                statusContainer.getId());
        mEng.process(event, statusContainer);
        
        TestCase.fail(e.getMessage());
    }
    
    /**
     * Send response message
     * 
     * @param msgContainer
     * @param process
     * @param msgExId
     */
    public void sendResponseMessage(MessageContainer msgContainer, RBPELProcess process, String msgExId) {
        Transaction transaction = msgContainer.getTransaction();
        String crmpInvokeId = msgContainer.getCRMPInvokeId();

        // creating a new message container using the same context and msgId, crmpId and transaction
        MessageContainer messageContainer = MessageContainerFactory.createMessage(msgExId, msgContainer.getContent(),
                crmpInvokeId, transaction);

        ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process, Event.RESPONSE_ERROR,
                messageContainer.getId());
        mEng.process(event, messageContainer);
    }
    
    
    /**
     * Send sub process request message
     * 
     * @param msgContainer
     * @param model
     * @param msgExId
     */
    public void sendSubBPRequestMessage(MessageContainer msgContainer, InComingEventModel model, String msgExId) {
        Transaction transaction = msgContainer.getTransaction();
        String crmpInvokeId = msgContainer.getCRMPInvokeId();
        MessageContainer messageContainer = MessageContainerFactory.createMessage(msgExId,
                msgContainer.getContent(), crmpInvokeId, transaction);
        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
        mSubBPInvokesMsgExchangeToIncomingEventModelMap.put(msgExId, model);
        
        // although we dont need to keep the transaction in this map, becuase
        // for sub-bp invoke, engine simulator is passthrough, but since the 
        // ReplyUnitImpl code does not set the transaction on the response container,
        // we need to look this up in the map to get the transaction object to pass to the 
        // invoking process (parent process)
        mTransactionsMap.put(msgExId, transaction);
        mEng.process(event, messageContainer);
    }
    
    /**
     * verify the received message
     * 
     * @param container
     * @param partnerLink
     * @param operation
     * @throws Exception
     */
    private void verifyOutput(MessageContainer container, RuntimePartnerLink partnerLink, String operation) throws Exception {
        JBIMessageImpl jbiMsg = (JBIMessageImpl) container.getContent();
        String receivedXML = getXMLString(jbiMsg.getElement());
        System.out.println("received message [" + receivedXML + "]");

        RuntimePartnerLink.InternalEPR epr = (RuntimePartnerLink.InternalEPR)partnerLink.getServiceRef();
        InComingKey key = mDeplBindings.createInComingBindingsKey(epr.getService(), epr.getEndPoint(), operation);
        
        String outputFileName = (String)mOutputKeyToOutputMessageMap.get(key);
        
        String expectedOutputXML = getXMLString(outputFileName);
        
        System.out.println("Expected output: [" + expectedOutputXML + "]");

        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        docFactory.setNamespaceAware(true);
        XMLUnit.setTestDocumentBuilderFactory(docFactory);
        XMLUnit.setControlDocumentBuilderFactory(docFactory);
        XMLUnit.setIgnoreWhitespace(true);

        Diff diff = XMLUnit.compare(expectedOutputXML, receivedXML);
        TestCase.assertTrue(diff.similar());
    }

    
    /**
     * @param fileName
     * @return
     * @throws Exception
     */
    private String getXMLString(String fileName) throws Exception {

        String ipMsgFilePath = mtestFolder + fileName;
        URL ipMsgFileURL = mTestCase.getClass().getResource(ipMsgFilePath);
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

    /**
     * @param elem
     * @return
     * @throws Exception
     */
    private String getXMLString(Element elem) throws Exception {
        DOMSource src = new DOMSource(elem);
        TransformerFactory fact = TransformerFactory.newInstance();
        Transformer transformer = fact.newTransformer();
        StringWriter writer = new StringWriter();
        StreamResult dest = new StreamResult(writer);

        transformer.transform(src, dest);
        String s = writer.toString();
        return s;
    }

    class MessageSender extends Thread {

        MessageContainerWrapper containerWrapper;

        public MessageSender(MessageContainerWrapper container) {
            this.containerWrapper = container;
        }

        private void associateTransaction(MessageContainer messageContainer) throws NotSupportedException, SystemException {
            TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(
                    TransactionManager.class.getName());
            tm.begin();
            final Transaction transaction = tm.getTransaction();
            
            // when you are initiator of transaction, as in the case wherein the message is sent by 
            // the junit, you need to save this transaction, as you need to commit (or rollback) it later when
            // status message done (error) happens
            mTransactionsMap.put(messageContainer.getId(), transaction);
            
            messageContainer.setTransaction(transaction);
            tm.suspend();
        }
        
        public void run() {
            try {
            	
            	int messageDelay = containerWrapper.getMessageDelay();
            	
            	if (messageDelay != -1) {
            		sleep(messageDelay * 1000);
            	}
            	
                InComingEventKeyImpl event = new InComingEventKeyImpl(
                        containerWrapper.getInComingEventModel(), Event.REQUEST);
                
                MessageContainer messageContainer = containerWrapper.getMessageContainer();
                associateTransaction(messageContainer);
                mEng.process(event, messageContainer);
                
            } catch (Exception e) {
                if (Thread.currentThread().isInterrupted() && e.getMessage().equals("JUNIT_RELATED_CRASH_SIMULATION")) {
                    //System.out.println("Thread Terminated .. simulating crash..");
                } else {
                    e.printStackTrace();
                }
            }
        }
    }
    
    class EngineAliveTask extends Thread {

        boolean isEngineAliveRunning = true;

        public void killIt() {
            isEngineAliveRunning = false;
        }

        public void run() {

            while (isEngineAliveRunning) {

                try {
                    long nextHeartbeatUpdateTime = mEng.getNextScheduledTime();
                    long sleepFor = nextHeartbeatUpdateTime - System.currentTimeMillis();

                    if (sleepFor > 0) {
                        sleep(sleepFor);
                    }
                    System.out.println(new java.util.Date().toString() + " Updating heartbeat for engine : " + mEng.getId());
                    mEng.process();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}

class MessageContainerWrapper {
    
    MessageContainer mContainer;
    InComingEventModel mModel;
    int mMessageDelay;

    public MessageContainerWrapper(MessageContainer container, InComingEventModel model, int messageDelay) {
        this.mContainer = container;
        this.mModel = model;
        this.mMessageDelay = messageDelay;
    }

    public MessageContainer getMessageContainer() {
        return mContainer;
    }

    public InComingEventModel getInComingEventModel() {
        return mModel;
    }

    public String toString() {
        return mModel.getStartElement().toString();
    }
    
    public int getMessageDelay() {
    	return mMessageDelay;
    }
}
