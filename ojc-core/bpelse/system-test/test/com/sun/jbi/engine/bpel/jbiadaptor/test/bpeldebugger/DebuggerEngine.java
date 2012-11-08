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
 * @(#)DebuggerEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.management.MBeanServerFactory;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELProcessManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventModelImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.RequestReplyKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.DebuggerEngineHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.MasterTestProperties;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestProperties;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * @author Sun Microsystems
 */
public class DebuggerEngine implements Runnable {
	String mEngId;

	Engine mEng;

	Properties mConnProp;

	StateManager mStateMgr;

	boolean started = false;

	static HashMap mModelMap = new HashMap();

	File msgFile;

	File outputFile;

	File tmpOutputFile;

	/** PARTNER_MYROLE constant */
	public static final String PARTNER_MYROLE = "myRole";

	/** PARTNER_PARTNERROLE constant */
	public static final String PARTNER_PARTNERROLE = "partnerRole";

	private static Logger mLogger = Logger
			.getLogger("com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger.DebuggerEngineTest");

	private static final String testPropertyFile = System.getProperty("test.properties", "bpdebugger/test.properties");

	private File baseDir;

	private MasterTestProperties masterTestProps;

	private boolean debugDetail = false;

    private DeploymentBindings mDeploymentBindings = new DeploymentBindings ();


	void setUp() throws Exception {
        SetUpHelper setUp = new SetUpHelper();
        mEng = setUp.getEngine();
		mEngId = mEng.getId();
		mStateMgr = mEng.getStateManager();

	}

	void tearDown() throws Exception {
		// remove BPELs and inserted instance data??
		System.out.println("Engine tearing down ...");
		mEng = null;
		mEngId = null;
		mStateMgr =null;
	}

	/** Assumes that the FirstTest class hasn't failed.
	 * @throws Exception
	 */
	public void deployBP() throws Exception {
		URL url = getClass().getResource(testPropertyFile);
		File testProperties =  new File(url.toURI());
		baseDir = testProperties.getParentFile();
		masterTestProps = new MasterTestProperties ();
		masterTestProps.load(testProperties);


		BPELInterpreter mInter = mEng.getInterpreter();
        mEng.setDebugEnabled(true);
//        mInter.enableDebugger();
//        mInter.disableDebugger();
//        mInter.enableDebugger();

        StatusProviderHelper statusProviderHelper = null;

		try {
			statusProviderHelper = new StatusProviderHelper("BPELSE Status",
					StatusProviderMBean.COMPONENT_TYPE_ENGINE,
					"BpelDebugger_engine", MBeanServerFactory
							.createMBeanServer());
			if (mLogger.isLoggable(Level.INFO)) {
				mLogger.info("Registered Status Provider MBean for "
						+ "BpelDebugger_engine");
			}
			statusProviderHelper.registerMBean();
		} catch (Exception ex) {
			mLogger.log(Level.WARNING,
					"Failed to register status provider MBean", ex);
			throw new JBIException("Failed to register status provider MBean",
					ex);
		}

		Collection startedBPELIds = new ArrayList();

		DebuggerEngineHelper.deploy(mEng, masterTestProps, mDeploymentBindings,
				startedBPELIds, tmpOutputFile, mModelMap);
        ((BPELEngine) mEng).enableDebugger();
        ((BPELEngine) mEng).disableDebugger();
        ((BPELEngine) mEng).enableDebugger();


		started = true;
	}


	public static class ServiceRef {
		private BPELProcessManager manager;
		private QName inType;

		public ServiceRef(BPELProcessManager p, QName type) {
			manager = p;
			inType = type;
		}

		public BPELProcessManager getBPELProcessManager() {
			return manager;
		}

		public QName getInType() {
			return inType;
		}
	}

	public static class RefKey {
		private String operation;

		private QName serviceName;

		public RefKey(String op, QName service) {
			operation = op;
			serviceName = service;
		}

		public int hashCode() {
			return operation.hashCode() + serviceName.hashCode();
		}

		public boolean equals(Object obj) {
			if (!(obj instanceof RefKey)) {
				return false;
			}

			if (!((RefKey) obj).operation.equals(this.operation)) {
				return false;
			}

			if (!((RefKey) obj).serviceName.equals(this.serviceName)) {
				return false;
			}

			return true;
		}
	}
    public static class EngineRequestProcesser extends TimerTask {
        private MessageContainer container;
        private InComingEventModel model;
        private Engine engine;

        public EngineRequestProcesser (MessageContainer container, InComingEventModel model, Engine engine ) {
            this.container = container;
            this.model = model;
            this.engine = engine;

        }

        public void run() {
            try {
                InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
                engine.process(event, container);
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public static class EngineResponseProcessor extends TimerTask {
        private MessageContainer container;
        private RBPELProcess bp;
        private InComingEventModel model;
        private Engine engine;

        public EngineResponseProcessor (MessageContainer container, RBPELProcess bp, Engine engine ) {
            this.container = container;
            this.bp = bp;
            this.engine = engine;

        }

        public void run() {
            try {
                InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(bp,
                        Event.REPLY_FAULT, container.getId());
                engine.process(event, container);
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public static class EngineProcessDoneProcessor extends TimerTask {
        private MessageContainer container;
        private RBPELProcess bp;
        private InComingEventModel model;
        private Engine engine;

        public EngineProcessDoneProcessor (MessageContainer container, RBPELProcess bp, Engine engine ) {
            this.container = container;
            this.bp = bp;
            this.engine = engine;

        }

        public void run() {
            try {
                ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                        bp, Event.DONE, container.getId()); 
                engine.process(event, container);
                //engine.processDone(container, bp);
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    public static class EngineProcessErrorProcessor extends TimerTask {
        private MessageContainer container;
        private RBPELProcess bp;
        private InComingEventModel model;
        private Engine engine;

        public EngineProcessErrorProcessor (MessageContainer container, RBPELProcess bp, Engine engine ) {
            this.container = container;
            this.bp = bp;
            this.engine = engine;

        }

        public void run() {
            try {
                InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(bp, Event.ERROR,
                        container.getId());

                engine.process(event, container);
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }


	public static class TestBPELProcessManagerImpl extends BPELProcessManagerImpl {
		private Engine mEngine;
		private TestProperties mTestProperties;
		private File mBaseDir;
		private int mReplyno;
        private DeploymentBindings mDepBindings;
		boolean start = true;
        boolean toReply=false;
        WSMessage replymsg = null;
        MessageContainer con = null;
        private RBPELProcess childProcess;
//        private MessageContainer childContainer;

		public TestBPELProcessManagerImpl(RBPELProcess process, Engine engine, TestProperties testProperties,
				File baseDir, DeploymentBindings depBindings) {
			super(process, engine, null, null);
			mEngine = engine;
			mTestProperties = testProperties;
			mBaseDir = baseDir;
			mReplyno = 0;
            mDepBindings = depBindings;

		}

        /**
         * @see BPELProcessManager#sendReply(com.sun.jbi.engine.bpel.core.bpel.model.meta.impl.RReplyImpl,
         *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
         */
        public void sendReply(MessageContainer content, QName faultName) {
            if ((faultName == null) || "".equals(faultName)) { //$NON-NLS-1$

                // TODO fix the following implementation access of EngineImpl.
               toReply = true;
               replymsg = (WSMessage) content.getContent();
               con = content;
            } else {
                // TODO fix the following implementation access of EngineImpl.
                return;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param event DOCUMENT ME!
         * @param frame DOCUMENT ME!
         */
        public void addToPendingQueueWaitingForDone(InComingEventKeyImpl event, ICallFrame callFrame) {

            RBPELProcess process = callFrame.getProcess();
            if (mTestProperties.testOutputFile != null) {

                // if (start) {
                // ResponseInComingEventKeyImpl newevent = new ResponseInComingEventKeyImpl(
                // process, Event.DONE, con.getId());
                // super.addToPendingQueueWaitingForDone(newevent, callFrame);
                // start = false;
                // }
                if (mReplyno < mTestProperties.testOutputFile.size()) {
                    ResponseInComingEventKeyImpl newevent = new ResponseInComingEventKeyImpl(
                            process, Event.DONE, con.getId());
                    super.addToPendingQueueWaitingForDone(newevent, callFrame);


                    MessageContainer statusMsgCon = MessageContainerFactory.createDoneStatus(con.getId(), null, null);

                    try {
                        new Timer().schedule(new EngineProcessDoneProcessor(statusMsgCon, process,
                                mEngine), 2);
                        // new Thread (new EngineResponseProcessor(container, ((ServiceRef)
                        // mModelMap.get(key)).getProcess(), mEngine)).run();
                    } catch (Exception e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }

                    System.out.println("<<<writing reply : " + mReplyno);

                    WSMessage output = replymsg;
                    WSMessage dom = (WSMessage) output;
                    System.out.println("Writing out");

                    try {
                        File parent = new File(mBaseDir.getAbsolutePath() + File.separator
                                + "output");

                        if (!parent.exists()) {
                            parent.mkdirs();
                        }
                        File outputFile = new File(parent, (String) (mTestProperties.testOutputFile
                                .get(mReplyno)));
                        if (!outputFile.exists()) {
                            outputFile.createNewFile();
                        }

                        FileOutputStream fos = new FileOutputStream(outputFile);
                        DebuggerEngineHelper.serialize(dom, fos);
                        fos.close();
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(2);
                    }
                    mReplyno++;

                }
            } else {
                System.out.println("<<<Sending reply : " + mReplyno);
                WSMessage output = replymsg;

                MessageContainer container = MessageContainerFactory.createMessage(con.getId(),
                        output, null, null);

                RefKey key = new RefKey(mTestProperties.replyOperation, new QName(
                        mTestProperties.replyServiceNameSpace, mTestProperties.replyServiceName));
                InComingEventModel model = DebuggerEngine.getInComingEventModel(key);

                ResponseInComingEventKeyImpl pendingevent = new ResponseInComingEventKeyImpl(
                        process, Event.DONE, con.getId());
                super.addToPendingQueueWaitingForDone(pendingevent, callFrame);

                try {
                    new Timer().schedule(new EngineResponseProcessor(container,
                            ((ServiceRef) mModelMap.get(key)).getBPELProcessManager().getBPELProcess(), mEngine), 2);
                    // new Thread (new EngineResponseProcessor(container, ((ServiceRef)
                    // mModelMap.get(key)).getProcess(), mEngine)).run();
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }


//		public void sendReply(RReplyImpl reply, ICallFrame callFrame) {
//			RBPELProcess process = callFrame.getProcess();
//            if (mTestProperties.testOutputFile != null) {
//
//			if (start)  {
//					ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
//					process, Event.DONE, mTestProperties.testId);
//				super.addToPendingQueueWaitingForDone(event, callFrame);
//				start = false;
//			}
//
//
//			if (mReplyno < mTestProperties.testOutputFile.size()) {
//
//			WSMessage output = callFrame.getVariableData(((RReply) reply)
//					.getRVariable().getName()).getWSMessage();
//			WSMessage dom = (WSMessage) output;
//			System.out.println("Writing out");
//
//			try {
//				File parent = new File (mBaseDir.getAbsolutePath() + File.separator + "output");
//
//
//				if (!parent.exists()) {
//					parent.mkdirs();
//				}
//				File outputFile = new File (parent, (String) (mTestProperties.testOutputFile.get(mReplyno)));
//				if (!outputFile.exists()) {
//					outputFile.createNewFile();
//				}
//
//				FileOutputStream fos = new FileOutputStream(outputFile);
//				DebuggerEngineHelper.serialize(dom, fos);
//				fos.close();
//			} catch (Exception ex) {
//				ex.printStackTrace();
//				System.exit(2);
//			}
//				mReplyno ++;
//			}
//            }
//            else  {
//
//            WSMessage output = callFrame.getVariableData(((RReply) reply)
//                    .getRVariable().getName()).getWSMessage();
//
//            MessageContainer container = MessageContainerFactory.createMessage(
//                    mTestProperties.testId, output, null);
//
//
//            RefKey key = new RefKey(mTestProperties.replyOperation, new QName(mTestProperties.replyServiceNameSpace , mTestProperties.replyServiceName));
//            InComingEventModel model = DebuggerEngine.getInComingEventModel(key);
//
//            ResponseInComingEventKeyImpl pendingevent = new ResponseInComingEventKeyImpl(
//                    process, Event.DONE, mTestProperties.testId);
//            super.addToPendingQueueWaitingForDone(pendingevent, callFrame);
//
//
//            try {
//                new Timer ().schedule(new EngineResponseProcessor(container, ((ServiceRef) mModelMap.get(key)).getProcess(), mEngine), 2);
////                new Thread (new EngineResponseProcessor(container, ((ServiceRef) mModelMap.get(key)).getProcess(), mEngine)).run();
//            } catch (Exception e) {
//                // TODO Auto-generated catch block
//                e.printStackTrace();
//            }
//            }
//
//
//		}

        /**
         * @see BPELProcessManager#sendInOnlyRequestDoneStatus(Object)
         */
        public void sendDoneStatusOfReceive(Object messageExchangeKey) {
            MessageContainer statusMsgCon = MessageContainerFactory.createDoneStatus(
                   (String) messageExchangeKey, null, null);

            // TODO fix the following implementation access of EngineImpl.
//            ((EngineImpl) mEngine).getChannel().sendStatus(statusMsgCon, null);
        }


        /**
         * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#sendResponseDoneStatus(java.lang.Object)
         */
        public void sendDoneStatus(Object messageExchangeKey) {
            MessageContainer statusMsgCon = MessageContainerFactory.createDoneStatus(
                    (String)messageExchangeKey, null, null);


            try {
                new Timer ().schedule(new EngineProcessDoneProcessor(statusMsgCon, childProcess, mEngine), 2);
//                new Thread (new EngineResponseProcessor(container, ((ServiceRef) mModelMap.get(key)).getProcess(), mEngine)).run();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        /**
         * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#sendResponseErrorStatus(java.lang.Object,
         *      java.lang.Exception)
         */
        public void sendErrorStatus(Object messageExchangeKey, Exception ex) {
            MessageContainer statusMsgCon = MessageContainerFactory.createErrorStatus(
                    (String)messageExchangeKey, ex.getMessage(), null, null);
            try {
                new Timer ().schedule(new EngineProcessErrorProcessor(statusMsgCon, childProcess, mEngine), 2);
//                new Thread (new EngineResponseProcessor(container, ((ServiceRef) mModelMap.get(key)).getProcess(), mEngine)).run();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }


        /**
         * @see BPELProcessManager#sendErrorStatusOfReceive(Object, java.lang.Exception)
         */
        public void sendErrorStatusOfReceive(Object messageExchangeKey, Exception ex) {
            MessageContainer statusMsgCon = MessageContainerFactory
                    .createErrorStatus((String)messageExchangeKey, ex.getMessage(),
                            null, null);

            // TODO fix the following implementation access of EngineImpl.
//            ((EngineImpl) mEngine).getChannel().sendStatus(statusMsgCon, ex);
        }


        /**
         * @see BPELProcessManager#sendFault(com.sun.jbi.engine.bpel.core.bpel.engine.impl.RequestReplyKeyImpl,
         *      com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
         */
        public void sendFault(RequestReplyKeyImpl key, MessageContainer msgContainer) {
//                mRequestReplyMap.remove(key);
            WSMessage dom = (WSMessage) msgContainer.getContent();

            if (mTestProperties.testOutputFile != null) {

                // if (start) {
                // ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                // process, Event.DONE, mTestProperties.testId);
                // super.addToPendingQueueWaitingForDone(event, callFrame);
                // start = false;
                // }
                //
                //
                if (mReplyno < mTestProperties.testOutputFile.size()) {

                    System.out.println("Writing out");

                    try {
                        File parent = new File(mBaseDir.getAbsolutePath() + File.separator
                                + "output");

                        if (!parent.exists()) {
                            parent.mkdirs();
                        }
                        File outputFile = new File(parent, (String) (mTestProperties.testOutputFile
                                .get(mReplyno)));
                        if (!outputFile.exists()) {
                            outputFile.createNewFile();
                        }

                        FileOutputStream fos = new FileOutputStream(outputFile);
                        DebuggerEngineHelper.serialize(dom, fos);
                        fos.close();
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(2);
                    }
                    mReplyno = mTestProperties.testOutputFile.size();
                }
            } else {

                MessageContainer container = MessageContainerFactory.createMessage(
                        msgContainer.getId(), dom, null, null);

                RefKey refKey = new RefKey(mTestProperties.replyOperation, new QName(
                        mTestProperties.replyServiceNameSpace, mTestProperties.replyServiceName));
                InComingEventModel model = DebuggerEngine.getInComingEventModel(refKey);

                // ResponseInComingEventKeyImpl pendingevent = new ResponseInComingEventKeyImpl(
                // getBPELProcess(), Event.DONE, mTestProperties.testId);
                // super.addToPendingQueueWaitingForDone(pendingevent, null);
                //

                try {
                    new Timer().schedule(new EngineResponseProcessor(container,
                            ((ServiceRef) mModelMap.get(refKey)).getBPELProcessManager().getBPELProcess(), mEngine), 2);
                    // new Thread (new EngineResponseProcessor(container, ((ServiceRef)
                    // mModelMap.get(key)).getProcess(), mEngine)).run();
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        }

        /**
         * invoke
         *
         * @param pLink Invoke activity
         * @param con input value
         * @param oneWay when true it is oneway
         * @param callFrame callframe
         * @return message exchange key
         */
//        public Object invoke(RuntimePartnerLink pLink, String operation, MessageContainer con, boolean oneWay, RBPELProcess process) {
//            WSMessage content = null;
//            RVariable inputContainer = (RVariable) pLink.getInputBPELVariable();
//
//            if (inputContainer != null) {
//                content = (WSMessage) con.getContent();
//            }
//            Object me = new Object ();
//
//            String meId = new Random ().toString();
//
//            MessageContainer container = MessageContainerFactory.createMessage(
//                    meId, content, con.getCRMPInvokeId(), null);
//
////            ResponseInComingEventKeyImpl event = null;
////            RBPELProcess process = callFrame.getProcess();
////
////            if (oneWay) {
////                event = new ResponseInComingEventKeyImpl(
////                        process, Event.DONE,  mTestProperties.testId
////                    );
////            } else {
////                event = new ResponseInComingEventKeyImpl(
////                        process, Event.REPLY_FAULT,  mTestProperties.testId
////                    );
////            }
////
////            if (oneWay) {
////                synchronized (super.mDonePendingInstances) {
////                    MessageContainer obj = receiveDone(event);
////
////                    if (obj != null) {
////                        return obj;
////                    }
////
////                    super.addToPendingQueueWaitingForDone(event, callFrame);
////                }
////            } else {
////                synchronized (super.mResponsePendingInstances) {
////                    MessageContainer obj = receiveResponse(event);
////
////                    if (obj != null) {
////                        return obj;
////                    }
////
////                    super.addToPendingQueueWaitingForReply(event, callFrame);
////                }
////            }
//            //Invoking the partnerLink
//            PartnerLink partenLink = pLink.getBPELPartnerLink();
//            String partenRole = partenLink.getPartnerRole();
//            PortType pt = partenLink.getBPELPartnerLinkType().getRole(partenRole).getWSDLPortType();
//            WSDLDocument wsdlDoc = (WSDLDocument) pt.getOwnerDocument();
//            RefKey key = new RefKey(pLink.getOperation(), DebuggerEngineHelper.findService(wsdlDoc.getDocumentDefinitions(), pt));
//            InComingEventModel model = DebuggerEngine.getInComingEventModel(key);
//            childProcess = ((ServiceRef) mModelMap.get(key)).getBPELProcessManager().getBPELProcess();
//
//
//            try {
//                new Timer ().schedule(new EngineRequestProcesser(container, model, mEngine), 2);
//
////                new Thread (new EngineRequestProcesser(container, model, mEngine)).run();
//            } catch (Exception e) {
//                // TODO Auto-generated catch block
//                e.printStackTrace();
//            }
//            return meId;
//
//        }


//        /**
//         * @see BPELProcessManager#invoke(com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InvokeUnitImpl,
//         *      com.sun.jbi.engine.bpel.core.bpms.bpel.runtime.ICallFrame)
//         */
//        public MessageContainer invoke(InvokeUnitImpl invokeUnit, ICallFrame callFrame) {
//            Invoke invoke = (Invoke) invokeUnit.getStaticModelActivity();
//            boolean oneWay = true;
//
//            if (invoke.getOutputVariable() != null) {
//                oneWay = false;
//            }
//
//            WSMessage content = null;
//            RVariable inputContainer = (RVariable) invoke.getInputBPELVariable();
//
//            if (inputContainer != null) {
//                content = callFrame.getVariableData(inputContainer.getName()).getWSMessage();
//            }
//
//            String relMesgId = Utility.getCustRelMesgId(callFrame, invokeUnit);
//
//            MessageContainer container = MessageContainerFactory.createMessage(
//                    mTestProperties.testId, content, relMesgId);
//
//            ResponseInComingEventKeyImpl event = null;
//            RBPELProcess process = callFrame.getProcess();
//
//            if (oneWay) {
//                event = new ResponseInComingEventKeyImpl(
//                        process, Event.DONE,  mTestProperties.testId
//                    );
//            } else {
//                event = new ResponseInComingEventKeyImpl(
//                        process, Event.REPLY_FAULT,  mTestProperties.testId
//                    );
//            }
//
//            if (oneWay) {
//                synchronized (super.mDonePendingInstances) {
//                    MessageContainer obj = receiveDone(event);
//
//                    if (obj != null) {
//                        return obj;
//                    }
//
//                    super.addToPendingQueueWaitingForDone(event, callFrame);
//                }
//            } else {
//                synchronized (super.mResponsePendingInstances) {
//                    MessageContainer obj = receiveResponse(event);
//
//                    if (obj != null) {
//                        return obj;
//                    }
//
//                    super.addToPendingQueueWaitingForReply(event, callFrame);
//                }
//            }
//            //Invoking the partnerLink
//            PartnerLink partenLink = invoke.getBPELPartnerLink();
//            String ns = partenLink.getBPELPartnerLinkType().getOwnerDocument().getTargetNamespace();
//            InvokeServiceReference servRef = mDepBindings.getInvokeServiceReference(partenLink);
//            RefKey key = new RefKey(invoke.getOperation(), new QName(ns,servRef.getService().getLocalName()));
//            InComingEventModel model = DebuggerEngine.getInComingEventModel(key);
//            childProcess = ((ServiceRef) mModelMap.get(key)).getProcess();
//
//
//            try {
//                new Timer ().schedule(new EngineRequestProcesser(container, model, mEngine), 2);
//
////                new Thread (new EngineRequestProcesser(container, model, mEngine)).run();
//            } catch (Exception e) {
//                // TODO Auto-generated catch block
//                e.printStackTrace();
//            }
//            return null;
//        }


		public void checkDone () {
			if (mReplyno == mTestProperties.testOutputFile.size()) {
				MessageContainer  con = MessageContainerFactory.createDoneStatus(mTestProperties.testId, null, null);
	            //mEngine.processDone(con, this.getBPELProcess());
                ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                        getBPELProcess(), Event.DONE, con.getId()); 
                mEngine.process(event, con);

	            mReplyno = 0;
	            start = true;
	            return;
			}

		}

	}

	public void run() {
		// TODO Auto-generated method stub
		try {
			setUp();
			deployBP();
                } catch (Throwable t) {
                    throw new RuntimeException(t);
                }

		//

	}



	public boolean sendMessage() {
		boolean result = true;
		Set testsEntrySet = masterTestProps.sends.entrySet();
		File baseFile = masterTestProps.baseDir;
		for (Iterator it = testsEntrySet.iterator(); it.hasNext();) {
			Entry testEntry = (Entry) it.next();
			TestProperties testProperties = (TestProperties) testEntry.getValue();
			List testOperations = testProperties.testOperations;
			List testMsgNams = testProperties.testMsgNams;
			List testInputFiles = testProperties.testInputFiles;
			List testParts = testProperties.testParts;
			List testOutputFiles = testProperties.testOutputFile;

			String testName = testProperties.name;
			String testId = testProperties.testId;
			String serviceNameSpace = testProperties.serviceNameSpace;
			String serviceName = testProperties.serviceName;
			String wsdlFile = testProperties.wsdlFile;
            long testWaitInterval = testProperties.testWaitInterval;
            long testWaitTimes = testProperties.testWaitTimes;
			debugDetail = testProperties.testDebugDetail;
			int testMsgInterval = testProperties.testMsgInterval;

			for (int msgno = 0; msgno < testOperations.size(); msgno ++) {
                System.out.println("Sending message : " + msgno + " " + testInputFiles.get(msgno));
				RefKey key = new RefKey((String) testOperations.get(msgno), new QName(serviceNameSpace,
				serviceName));
				ServiceRef sref = (ServiceRef) mModelMap.get(key);
				RBPELProcess bp = sref.getBPELProcessManager().getBPELProcess();

				File inputFile = new File (baseFile + File.separator + testName + File.separator + "input" + File.separator + testInputFiles.get(msgno));
				InputStream is = null;
				try {
					is = new FileInputStream(inputFile);
				} catch (FileNotFoundException e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				}
				ByteArrayOutputStream os = new ByteArrayOutputStream();

				try {
					int c;

					while ((c = is.read()) != -1) {
						os.write(c);
					}

					is.close();
					os.close();
				} catch (IOException e) {
					e.printStackTrace();
				}

				String xmlString = new String(os.toByteArray());

				Document elem = DOMHelper.createDOMDoc(xmlString);

				File wsdl = new File (baseFile + File.separator + testName + File.separator + wsdlFile);
				
				WSDLReader wsdlReader = WSDL4JExt.newWSDLReader(null);
				Definition defn = null;
                try {
                    defn = wsdlReader.readWSDL(wsdl.getAbsolutePath());
                } catch (WSDLException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }

				QName qName = new QName(defn.getTargetNamespace(), (String) testMsgNams.get(msgno));

				WSMessage domMsg = new JBIMessageImpl(elem, defn.getMessage(qName));
//				domMsg.setPart((String) testParts.get(msgno), null, elem, /*evalCtx*/null);
				MessageContainer container = MessageContainerFactory.createMessage(
						testId + msgno, domMsg, null, null);
				Iterator actIt = bp.getStartElements().iterator();
				while (actIt.hasNext()) {
					RActivity act = (RActivity) actIt.next();
					if (!(act instanceof RStartElement)) {
						continue;
					}

					RStartElement sa = (RStartElement) act;
					if (key.operation.equals(sa.getWSDLOperation().getName())) {
						BPELProcessManager procMgr = sref.getBPELProcessManager();
						String pattern = procMgr.getOperationPattern(sa);
						InComingEventModel model = new InComingEventModelImpl(bp, sa,
								pattern);
						try {
					        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
							mEng.process(event, container);
                            if (testWaitInterval > 0) {
                                if (testWaitTimes == 0)
                                    testWaitTimes = 1;
                                for (int i=0; i< testWaitTimes; i++) {
                                    Thread.sleep (testWaitInterval);
                                    mEng.process();
                                }
                            }
//                            Thread.sleep (flowToWait);
//                            mEng.process();
//							((TestBPELProcessManagerImpl) procMgr).checkDone();
							//					procMgr.process();
						} catch (Exception e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
				if (testMsgInterval > 0) {
					try {
						Thread.sleep(testMsgInterval);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		for (Iterator outfileIter = testOutputFiles.iterator(); outfileIter.hasNext();) {

		File tmpOutputFile = new File (baseFile + File.separator + testName + File.separator + "output" + File.separator + (String) outfileIter.next());

		int i = 0;

		while (!tmpOutputFile.exists() && i < 10) {
					try {
						Thread.sleep(100);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					i++;
				}
				if (!tmpOutputFile.exists()) {
					result = false;
				}
			}
		}
		return result;

	}

	public boolean isDebugDetail() {
		// TODO Auto-generated method stub
		return debugDetail;
	}

    public MasterTestProperties getMasterProps () {
        return masterTestProps;
    }

    public static InComingEventModel getInComingEventModel (RefKey serviceRefKey) {
        ServiceRef sref = (ServiceRef) mModelMap.get(serviceRefKey);
        RBPELProcess bp = sref.getBPELProcessManager().getBPELProcess();
        Iterator actIt = bp.getStartElements().iterator();
        InComingEventModel model= null;
        while (actIt.hasNext()) {
            RActivity act = (RActivity) actIt.next();
            if (!(act instanceof RStartElement)) {
                continue;
            }

            RStartElement sa = (RStartElement) act;
            if (serviceRefKey.operation.equals(sa.getWSDLOperation().getName())) {
                BPELProcessManager procMgr = sref.getBPELProcessManager();
                String pattern = procMgr.getOperationPattern(sa);
                model = new InComingEventModelImpl(bp, sa,
                        pattern);
                break;
            }
        }
        return model;

    }



}
