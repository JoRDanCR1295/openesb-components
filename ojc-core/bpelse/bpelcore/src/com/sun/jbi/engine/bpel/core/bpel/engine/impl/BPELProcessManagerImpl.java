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
 * @(#)BPELProcessManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.io.Reader;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.w3c.dom.DocumentFragment;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.util.NDC;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.debug.DefaultDebugger;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.PerformanceManager;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsConfigurationHelper;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsFilter;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventPersister;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardFaultException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.TimerActUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.XSDVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InactivityReason;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.MonitorManager;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.XSDVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.NonExpiredOnAlarmInstanceInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;
import java.util.StringTokenizer;

/**
 * BPEL process manager implementation
 * 
 * @author Sun Microsystems
 */
public class BPELProcessManagerImpl implements BPELProcessManager {

    private static Logger LOGGER = Logger.getLogger(BPELProcessManagerImpl.class.getName());
    private static final String YES = "yes";
    private static final String NO = "no";
    private RBPELProcess mBPELProcess;
    private Engine mEngine;
    private PickManagerImpl mPickMgr;
    private CorrelationManagerImpl mCorrMgr;
    private ScalabilityManager mMemoryManager;
    private PerformanceManager mPerformanceManager;
    private String mSAName;
    private String mSUName;
    private Integer mWaitingRequestLifeSpan;
    /** read-to-run queue */
    private ReadyToRunQueue mReadyToRunQueue = null;
    private Map mEventRequestMap = new HashMap();

    /* used by clustering to store out of order requests. i.e
     * if the correlated message arrives before the message that
     * create the instance
     */
    private Map mEventRequestMapForCorr = new HashMap();
    private RequestLifeSpanHelper mReqLifeSpanHelper;
    private Hashtable mEventResponseMap = new Hashtable();
    private Hashtable mEventDoneMap = new Hashtable();
    private Hashtable mStartActivityModelMap = new Hashtable();
    private boolean mIsBPAtomic;
    private boolean mGenerateEvents;
    private boolean mPersistenceOptOut;
    private boolean mPersistenceOff;
    private boolean mIgnoreMissingFromData;
    private EventsFilter mEventsFilter = null;
    // TODO We probably can add different pendingInstances, based on different
    // states they are pending. Think about it!
    protected Map mRequestPendingInstances = new HashMap();
    protected Hashtable<InComingEventKeyImpl, BusinessProcessInstanceThread> mResponsePendingInstances = new Hashtable<InComingEventKeyImpl, BusinessProcessInstanceThread>();
    private List<ResponseInComingEventKeyImpl> expiredInvokeResponse = Collections.synchronizedList(new ArrayList<ResponseInComingEventKeyImpl>());
    protected Hashtable mDonePendingInstances = new Hashtable();
    private List mAllInstanceIds = Collections.synchronizedList(new ArrayList());
    private List mAllInstances = Collections.synchronizedList(new ArrayList());
    // With persistence enabled, this hold the running instances for on alarm
    private List nonExpiredOnAlarmRunningInstances = Collections.synchronizedList(new ArrayList());
    /** Lock object for mRequestPendingInstances and mEventRequestMap */
    //final Object mRPI_ER_Lock = new Object();
    Object mProcessLevelLock = new Object();
    /** lock object for flow based creat or correlate branches */
    //private final Object mCREATE_OR_CORRELATE_Lock = new Object();
    private final Object mCreateInstanceLock = new Object();
    /**
     * During recovery the MainBP CRMP requests that arrive before the SubBP instaces are created will be put into this map
     * The key is bpId + PartnerLink + operation ( in the future bpel message id may have to be included).
     * This map and its access apis are not thread safe.
     */
    private Map mCRMPReqsForRecoveringInsts = new HashMap();
    /**Map holds the CRMPLookup object that is populated when the reply activity is processed */
    protected Map mCRMPReplyMap = new HashMap();
    /** 
     * Indicates maxInstances for this process 
     *  default value of -1 indicates no restrictions 
     *  on instance creation. 
     *  value of 1 indicates that the instances for this
     *  bpel process have to be processed serially.
     */
    private int mMaxInstances = -1;
    /**
     * The list that maintains the events in order that
     * could not be processed by the engine, when 
     * maxInstances was reached.
     */
    private LinkedList<WaitingMaxedObj> mWaitingMaxedInstances;
    /**
     * Used only for the engine maxInstances/throttling mode
     * where this flag is used to determine if all the running
     * instances in the DB for this process has finished processing
     * before the maxInstances/throttling can be applied to new 
     * inbound messages are processed, when the engine has recovered.
     */
    private boolean mIsRecoveryComplete = true;
    private Map<EndpointInfo, ThrottlingConfigInfo> mThrottleConfigMap =
            Collections.synchronizedMap(new HashMap<EndpointInfo, ThrottlingConfigInfo>());
    private XslCache mXslCache = new XslCache();
    private Map<String, Object> mAppVarsCache = new HashMap();
    /**
     * Represents the 'atomicTxType' attribute of the process tag
     * This attribute is used only in conjunction with the 'atomic' attribute and 
     * is ignored without it. 
     * Note: Two attributes exist to ensure backward compatibility of existing bpel process
     * definitions.
     * Currently supported types are 
     * 
     * 1. Supports: If the inbound message exchange as an associated transaction
     * context then the bpel atomic mode will execute with the client transaction, 
     * if the inbound message exchange does not have an associated transaction context
     * then the bpel atomic mode will not create a transaction.
     * This is the default type supported even if the attribute 'atomicTxType' is not
     * specified but the 'atomic' attribute is defined. 
     * 
     * 2. Required: If the inbound message exchange as an associated transaction
     * context then the bpel atomic mode will execute with the client transaction and
     * behave exactly as the Supports type. 
     * if the inbound message exchange does not have an associated transaction context
     * then the bpel atomic mode will create a new transaction to execute the atomic 
     * mode. 
     */
    private String mAtomicTxType = null;
    /**
     * It is switch at the process level disable Trace/Logging configured at
     * activity level. This helps to switch-off the business logging from BP
     */
    private boolean mIsLoggingEnabled;
    /**
     * NDC context name and value pairs. List size is always even. These pairs
     * are set as context for Trace/Logging.
     */
    private List<String> mExtraNDC;

    /**
     * Creates a new BPELProcessManagerImpl object.
     *
     * @param process runtime bpel process
     * @param engine engine
     */
    public BPELProcessManagerImpl(RBPELProcess process, Engine engine, String saName, String suName) {
        mBPELProcess = process;
        mEngine = engine;
        mReadyToRunQueue = new ReadyToRunQueue(this);
        mPickMgr = new PickManagerImpl(this);
        mCorrMgr = new CorrelationManagerImpl(process, engine);
        mReqLifeSpanHelper = new RequestLifeSpanHelper();
        mPerformanceManager = new PerformanceManager();

        mIsBPAtomic = (YES.equals(process.getAtomic())) ? true : false;
        mPersistenceOptOut = (YES.equals(process.getPersistenceOptOut())) ? true : false;
        mPersistenceOff = ((mIsBPAtomic) || (mPersistenceOptOut)) ? true : false;
        if ((!mPersistenceOff) && (mEngine.isPersistenceEnabled())) {
            // we assume that there are running instances of
            // this process in the DB. Optimization instead
            // of using a DB query to find out if there are
            // instances.
            mIsRecoveryComplete = false;
            // create a scalability manager for given BP if its persistence is
            // enabled.
            mMemoryManager = new ScalabilityManager(this, mEngine, mReadyToRunQueue);
        }
        mGenerateEvents = (NO.equals(process.getGenerateEvents())) ? false : true;
        mEventsFilter = EventsConfigurationHelper.createEventsFilter(process);

        String waitingRequestLifeSpan = process.getWaitingRequestLifeSpan();
        if (waitingRequestLifeSpan == null) {
            mWaitingRequestLifeSpan = new Integer(-1);
        } else {
            try {
                mWaitingRequestLifeSpan = Integer.parseInt(waitingRequestLifeSpan);
            } catch (NumberFormatException nfe) {
                // Non fatal exception. Print a warning reporting the exception
                // and that the engine's default
                // waiting request life span will be used.
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6023: Exception parsing the waiting request life span " + "attribute for bpel process {0}. Engine configuration of {1} seconds will be used instead.", process.getName(), engine.getWaitingRequestLifeSpan()));
            }
        }

        // Extension element ignoreAllMissingFromData
        mIgnoreMissingFromData = (YES.equals(process.getIgnoreMissingFromData())) ? true : false;

        mSAName = (saName != null) ? saName : "unavailable";
        mSUName = (suName != null) ? suName : "unavailable";

        // empty list for waiting maxed instance objects.
        mWaitingMaxedInstances = new LinkedList<WaitingMaxedObj>();

        if (mIsBPAtomic) {
            mAtomicTxType = process.getAtomicTxType();
        }

        mIsLoggingEnabled = (NO.equals(process.getEnableLogging())) ? false : true;
        mExtraNDC = parseNDC(process.getExtraNDC());

        if (engine.isMonitorEnabled()) {
            // get the instance counts and sync the monitior console. 
            Map<String, Integer> sMap = getMonitorInstanceSyncCount(process.getBPELId().toString());
            mPerformanceManager.syncInstanceCountAtStart(sMap);

        }
    }

    private List<String> parseNDC(String extraNDC) {
        List<String> retVal = new ArrayList();
        if (extraNDC == null) {
            return retVal;
        }
        StringTokenizer ndcTokenizer = new StringTokenizer(extraNDC, ",");
        while (ndcTokenizer.hasMoreTokens()) {
            String pairNDC = ndcTokenizer.nextToken();
            int indexOfColon = pairNDC.indexOf(':');
            retVal.add(pairNDC.substring(0, indexOfColon));
            retVal.add(pairNDC.substring(indexOfColon + 1));
        }
        return retVal;
    }

    /**
     * check if incoming event is duplicate
     *
     * @param event incoming event key
     * @param contents message container
     * @return boolean: if messagecontainer is duplicated, returns true; otherwise, returns false
     */
    public boolean isDuplicate(InComingEventKeyImpl event, MessageContainer contents) {
        // TODO custom Reliable Messaging protocol
        return false;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getEventFilter()
     */
    public EventsFilter getEventsFilter() {
        return mEventsFilter;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#setMaxInstances(int)
     */
    public void setMaxInstances(int count) {
        // at present we only support serial processing mode
        // where count == 1.
        if (count == 1) {
            mMaxInstances = 1;
        }
    }

    private void validateIMA(InComingEventKeyImpl event,
            MessageContainer contents) {
        InComingEventModel model = event.getEventModel();
        int createFlag = model.getStartElement().getStartType();

        if (createFlag == Engine.RECEIVE_TYPE_CREATE_ONLY || createFlag == Engine.RECEIVE_TYPE_CORRELATE_ONLY || createFlag == Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE) {
            //Valid IMA, do nothing
        } else if (createFlag == Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {
            throw new RuntimeException(I18n.loc("BPCOR-6016: IMA(Receive, Pick-OnMessage) activities other than that creates an instance should use correlations with initiate attribute no or its equal."));
        } else {
            throw new RuntimeException(I18n.loc("BPCOR-6017: IMA(Receive, Pick-OnMessage) activity {0} invalid type",
                    createFlag));
        }
    }

    /**
     * @see BPELProcessManager#handleEvent(com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void handleEvent(InComingEventKeyImpl event, MessageContainer contents) {

        if (event.getType() == Event.REQUEST) {
            validateIMA(event, contents);
            handleRequest(event, contents);
        } else if ((event.getType() == Event.REPLY_FAULT) || (event.getType() == Event.RESPONSE_ERROR)) {

            BusinessProcessInstanceThread bpit = null;
            String instanceId = null;

            synchronized (mProcessLevelLock) {

                if (expiredInvokeResponse.remove(event) && contents.isStatusError() == false) {
                    sendResponseErrorStatus(contents.getId(),
                            new Exception("The invoke was terminated. Response no longer valid"));
                    return;
                }

                mEventResponseMap.put(event, contents);

                // Find the instance off the mPendingQueue
                bpit = (BusinessProcessInstanceThread) mResponsePendingInstances.remove(event);
                if (bpit != null) {
                    ICallFrame frame = bpit.getCallFrame();
                    frame.getProcessInstance().removeFromInactiveList(frame, false);
                    mReadyToRunQueue.addBPI(bpit);
                } else {
                    if (isPersistenceEnabled() && mMemoryManager.areInstancesScalPassivted()) {
                        // If bpit is null, this means one of two things
                        // 1. Timing: The response came and picked by another thread
                        // before the instance was put on mResponsePendingInstances map . 
                        // 2. The instance was scalability passivated. For this case
                        // check the database to see if there is instance id defined for
                        // the message exchange id on the incoming response.
                        instanceId = mMemoryManager.getScalabilityPassInstanceId((ResponseInComingEventKeyImpl) event, contents);
                    }
                }
            }

            /*
             * NOTE: The activation should be done outside of the processlevel
             * lock as this call will involve recovery and will otherwise
             * serialize all the instances (for activation) during recovery
             * time (as noticed // while running for ScalabilityTest project
             * with 1 MB message size.
             */
            if (instanceId != null) {
                mMemoryManager.activateScalabilityPassInstance(instanceId, InactivityReason.PENDING_RESPONSE);
            }

        } else if ((event.getType() == Event.DONE) || (event.getType() == Event.ERROR)) {

            ICallFrame frame = null;
            String instanceId = null;

            synchronized (mProcessLevelLock) {
                mEventDoneMap.put(event, contents);
                // Find the instance off the mPendingQueue

                frame = (ICallFrame) mDonePendingInstances.remove(event);

                if (frame == null) {
                    // If we didn't find a call frame it could be executed by
                    // thread executing one way invoke or Reply
                    // OR
                    if (isPersistenceEnabled() && mMemoryManager.areInstancesScalPassivted()) {
                        // the instance could be scalability passivated. Check the database
                        // for presence of instance id for the outstanding msg ex and recover
                        // the instance.
                        instanceId = mMemoryManager.getScalabilityPassInstanceId((ResponseInComingEventKeyImpl) event, contents);
                    }

                } else {
                    frame.getProcessInstance().removeFromInactiveList(frame, false);
                    BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, frame);
                    bpit.setType(BusinessProcessInstanceThread.WAITING_FOR_DONE);
                    bpit.setMessageExchangeKey(contents.getId());

                    mReadyToRunQueue.addBPI(bpit);
                }
            }

            if (instanceId != null) {
                mMemoryManager.activateScalabilityPassInstance(instanceId, InactivityReason.PENDING_STATUS);
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getBPELProcess()
     */
    public RBPELProcess getBPELProcess() {
        return mBPELProcess;
    }

    /**
     * @see BPELProcessManager#addStartActivityModel(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement,
     *      java.lang.String)
     */
    public void addStartActivityModel(RStartElement sa, String operPattern) {
        mStartActivityModelMap.put(sa, operPattern);
    }

    /**
     * @see BPELProcessManager#getOperationPattern(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement)
     */
    public String getOperationPattern(RStartElement sa) {
        return (String) mStartActivityModelMap.get(sa);
    }

    private void addRequest(InComingEventKeyImpl event, MessageContainer contents) {
        synchronized (mProcessLevelLock) {
            Utility.addToListInMap(event, mEventRequestMap, contents);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getEventResponseMap()
     */
    public Map getEventResponseMap() {
        return mEventResponseMap;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getEventDoneMap()
     */
    public Map getEventDoneMap() {
        return mEventDoneMap;
    }

    public Object getProcessLevelLock() {
        return mProcessLevelLock;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#pickRequestOrPutInPendingQueue(com.sun.bpel.model.Pick,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl[], long,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public Object[] pickRequestOrPutInPendingQueue(Pick pick, InComingEventKeyImpl[] events, long deadline, ICallFrame frame) {

        Object[] returnValues = new Object[2];
        MessageContainer obj = null;

        synchronized (mProcessLevelLock) {
            for (int i = 0; i < events.length; i++) {
                obj = (MessageContainer) Utility.removeFromListInMap(events[i], mEventRequestMap);

                if (obj != null) {
                    returnValues[0] = new MessageContainerForPickImpl(events[i], obj);
                    returnValues[1] = false;
                    mReqLifeSpanHelper.removeMessage(events[i]);
                    return returnValues;
                }
            }

            if (isPersistenceEnabled() && mEngine.isClustered()) {
                // This handles the case wherein the correlated message arrives before the
                // message that creates the instance. check if the correlated message is already
                // waiting
                for (int i = 0; i < events.length; i++) {
                    obj = receiveRequestForCorrWaitingEvents(events[i]);

                    if (obj != null) {
                        returnValues[0] = new MessageContainerForPickImpl(events[i], obj);
                        returnValues[1] = false;
                        mReqLifeSpanHelper.removeMessage(events[i]);
                        return returnValues;
                    }
                }
            } else {
                if (frame.getProgramCounter().getContext().getFaultHandlingContext().isBeingTerminated()) {
                    returnValues[0] = null;
                    returnValues[1] = true;
                    return returnValues;
                } else {
                    // returns a timer based BPIT based on the deadline value.
                    BusinessProcessInstanceThread bpit = mPickMgr.addToPendingQueue(this, mEngine,
                            frame, deadline, events);
                    if (bpit != null) {
                        mReadyToRunQueue.addBPI(bpit);
                    }
                }
            }
        }

        returnValues[0] = null;
        returnValues[1] = false;
        return returnValues;
    }

    /**
     * @see BPELProcessManager#pickRequestWithOutWaiting(InComingEventKeyImpl[],
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public MessageContainerForPick pickRequestWithOutWaiting(InComingEventKeyImpl[] events,
            ICallFrame frame) {

        synchronized (mProcessLevelLock) {
            for (int i = 0; i < events.length; i++) {
                // MessageContainer obj = (MessageContainer) mEventRequestMap.remove(events[i]);
                MessageContainer obj = (MessageContainer) Utility.removeFromListInMap(events[i],
                        mEventRequestMap);

                if (obj != null) {
                    MessageContainerForPick ret = new MessageContainerForPickImpl(events[i], obj);

                    return ret;
                }
            }
        }

        return null;
    }

    /**
     * @see BPELProcessManager#addToPickRequestPendingInstances(InComingEventKeyImpl[],
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void addToPickRequestPendingInstances(InComingEventKeyImpl[] events,
            long deadline,
            ICallFrame frame) {

        synchronized (mProcessLevelLock) {
            mPickMgr.addToPendingQueue(this, mEngine, frame, deadline, events);
        }
    }

    /**
     * @see BPELProcessManager#receiveRequestOrPutInPendingQueue(
     * com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl,
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public Object[] receiveRequestOrPutInPendingQueue(InComingEventKeyImpl event, ICallFrame frame) {

        Object[] returnValues = new Object[2];

        synchronized (mProcessLevelLock) {
            MessageContainer obj = (MessageContainer) Utility.removeFromListInMap(event, mEventRequestMap);

            if (obj != null) {
                returnValues[0] = obj;
                returnValues[1] = false;
                mReqLifeSpanHelper.removeMessage(event);
                return returnValues;
            }

            if (isPersistenceEnabled() && mEngine.isClustered()) {

                // This handles the case wherein the correlated message arrives before the
                // message that creates the instance. check if the correlated message is already
                // waiting
                obj = receiveRequestForCorrWaitingEvents(event);

                if (obj != null) {
                    returnValues[0] = obj;
                    returnValues[1] = false;
                    mReqLifeSpanHelper.removeMessage(event);
                    return returnValues;
                }
            } else {
                if (frame.getProgramCounter().getContext().getFaultHandlingContext().isBeingTerminated()) {
                    returnValues[0] = null;
                    returnValues[1] = true;
                    return returnValues;
                } else {
                    frame.getProcessInstance().addToInactiveList(frame, InactivityReason.PENDING_REQUEST);
                    mRequestPendingInstances.put(event, frame);
                }
            }
        }

        returnValues[0] = null;
        returnValues[1] = false;
        return returnValues;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#checkRequest(
     * com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl,
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public boolean checkRequest(InComingEventKeyImpl[] events) {
        InComingEventKeyImpl event = null;

        for (int i = 0; i < events.length; i++) {
            event = (InComingEventKeyImpl) events[i];

            // check event request map
            synchronized (mProcessLevelLock) {
                if (mEventRequestMap.containsKey(event)) {
                    return true;
                }

                // did not find the request in regular event request map,
                // it could be in the out of order
                if (mEventRequestMapForCorr.containsKey(event)) {
                    return true;
                }
            }
        }

        return false;
    }

    /*
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#addToCorrelatedWaitingEventsMap(com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void addToCorrelatedWaitingEventsMap(InComingEventKeyImpl event, MessageContainer contents) {
        synchronized (mProcessLevelLock) {
            Utility.addToListInMap(event, mEventRequestMapForCorr, contents);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getCorrelatedWaitingEvents()
     */
    public List getCorrelatedWaitingEvents() {
        List eventList = new ArrayList();

        synchronized (mProcessLevelLock) {
            Iterator iter = mEventRequestMapForCorr.keySet().iterator();
            while (iter.hasNext()) {
                CorrelatingSAInComingEventKeyImpl event = (CorrelatingSAInComingEventKeyImpl) iter.next();
                eventList.add(event);
            }
        }

        return eventList;
    }

    /**
     * @param event
     * @return
     */
    public MessageContainer receiveRequestForCorrWaitingEvents(InComingEventKeyImpl event) {
        MessageContainer obj = null;

        synchronized (mEventRequestMapForCorr) {
            obj = (MessageContainer) Utility.removeFromListInMap(event, mEventRequestMapForCorr);
        }

        return obj;
    }

    /**
     * @see BPELProcessManager#receiveResponse(com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl)
     */
    public MessageContainer receiveResponse(InComingEventKeyImpl event) {
        MessageContainer retVal = null;
        retVal = (MessageContainer) mEventResponseMap.remove(event);
        return retVal;
    }

    /**
     * @see BPELProcessManager#receiveDone(com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl)
     */
    public MessageContainer receiveDone(InComingEventKeyImpl event) {
        MessageContainer retVal = null;
        retVal = (MessageContainer) mEventDoneMap.remove(event);
        return retVal;
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     * @param bpit DOCUMENT ME!
     */
    public void addToPendingQueueWaitingForReply(InComingEventKeyImpl event, BusinessProcessInstanceThread bpit) {
        ICallFrame frame = bpit.getCallFrame();
        frame.getProcessInstance().addToInactiveList(frame, InactivityReason.PENDING_RESPONSE);
        mResponsePendingInstances.put(event, bpit);
    }

    /**
     * DOCUMENT ME!
     *
     * @param event DOCUMENT ME!
     * @param frame DOCUMENT ME!
     */
    public void addToPendingQueueWaitingForDone(InComingEventKeyImpl event, ICallFrame frame) {
        frame.getProcessInstance().addToInactiveList(frame, InactivityReason.PENDING_STATUS);
        mDonePendingInstances.put(event, frame);
    }

    /**
     * @see BPELProcessManager#sendReply(com.sun.jbi.engine.bpel.core.bpel.model.meta.impl.RReplyImpl,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void sendReply(MessageContainer content, QName faultName) {
        if (faultName == null) {
            // TODO fix the following implementation access of EngineImpl.
            ((EngineImpl) mEngine).getChannel().reply(content);
        } else {
            // TODO fix the following implementation access of EngineImpl.
            ((EngineImpl) mEngine).getChannel().sendFault(faultName, content);
        }
    }

    /**
     * @see BPELProcessManager#sendInOnlyRequestDoneStatus(Object)
     */
    public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
        ((EngineImpl) mEngine).getChannel().sendInOnlyRequestDoneStatus(msgExchangeId);
    }

    /**
     * @see BPELProcessManager#invoke(com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.InvokeUnitImpl,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public Object invoke(RuntimePartnerLink pLink, QName operation, MessageContainer con, boolean oneWay, RBPELProcess process) {
        // TODO fix the following implementation access of EngineImpl.
        return ((EngineImpl) mEngine).getChannel().invoke(con, pLink,
                operation, oneWay, process);
    }

    /**
     * @see BPELProcessManager#sendFault(com.sun.jbi.engine.bpel.core.bpel.engine.impl.RequestReplyKeyImpl,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void sendFault(QName faultName, MessageContainer msgContainer) {
        // mRequestReplyMap.remove(key);
        ((EngineImpl) mEngine).getChannel().sendFault(faultName, msgContainer);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#sendRequestError(java.lang.String,
     *      java.lang.Exception)
     */
    public void sendRequestError(String msgExchangeId, Exception error) {
        ((EngineImpl) mEngine).getChannel().sendRequestError(msgExchangeId, error);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#sendResponseDoneStatus(java.lang.Object)
     */
    public void sendResponseDoneStatus(String msgExchangeId) {
        ((EngineImpl) mEngine).getChannel().sendResponseDoneStatus(msgExchangeId);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#sendResponseErrorStatus(java.lang.Object,
     *      java.lang.Exception)
     */
    public void sendResponseErrorStatus(String messageExchangeKey, Exception ex) {
        ((EngineImpl) mEngine).getChannel().sendResponseErrorStatus(messageExchangeKey, ex);
    }

    /**
     * terminates BPEL process instance
     * @param bpInstanceId BPEL instance ID
     */
    public void terminate(BPELProcessInstance processInstance) {

        String bpInstanceId = processInstance.getId();
        Iterator itr = null;
        Map.Entry entry = null;

        synchronized (mProcessLevelLock) {
            itr = mRequestPendingInstances.entrySet().iterator();

            while (itr.hasNext()) {
                entry = (Map.Entry) itr.next();
                ICallFrame callframe = (ICallFrame) entry.getValue();

                if (callframe.getBPId().equals(bpInstanceId)) {
                    itr.remove();
                }
            }

            itr = mResponsePendingInstances.entrySet().iterator();

            while (itr.hasNext()) {
                entry = (Map.Entry) itr.next();

                ICallFrame callframe = ((BusinessProcessInstanceThread) entry.getValue()).getCallFrame();
                if (callframe.getBPId().equals(bpInstanceId)) {
                    itr.remove();
                }
            }

            itr = mDonePendingInstances.entrySet().iterator();

            while (itr.hasNext()) {
                entry = (Map.Entry) itr.next();

                ICallFrame callframe = (ICallFrame) entry.getValue();

                if (callframe.getBPId().equals(bpInstanceId)) {
                    itr.remove();
                }
            }
        }

        instanceComplete(processInstance);
        // if the istance has faulted then the Terminate event need not generated for
        // the monitoring manager. The getMonitorMgr().postInstanceFaultedEvent() would 
        // have been called prior to this.
        if (!((FaultHandlingContext) processInstance).hasFaulted()) {
            processInstance.getMonitorMgr().postInstanceTerminatEvent();
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getNextScheduledTime()
     */
    public long getNextScheduledTime() {
        long nextScheduledTime = mReadyToRunQueue.getNextScheduledTime();
        long nextExpiryTime;
        synchronized (mProcessLevelLock) {
            nextExpiryTime = mReqLifeSpanHelper.getNextExpiryTime();
        }
        if ((nextExpiryTime > 0) && ((nextScheduledTime == 0) || (nextExpiryTime < nextScheduledTime))) {
            nextScheduledTime = nextExpiryTime;
        }

        return nextScheduledTime;
    }

    /**
     * @see BPELProcessManager#addToReadyToRunQueueForFlow(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void addToReadyToRunQueue(ICallFrame parentCallFrame) {
        BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, parentCallFrame);
        bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
        mReadyToRunQueue.addBPI(bpit);
        mEngine.notifyThreads();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#addToReadyToRunQueueForFlow(java.util.List)
     */
    public void addToReadyToRunQueueForFlow(List childCallFrames) {
        for (int i = 0, size = childCallFrames.size(); i < size; i++) {
            BusinessProcessInstanceThread bpit =
                    new BusinessProcessInstanceThread(this, mEngine, (ICallFrame) childCallFrames.get(i));
            bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
            mReadyToRunQueue.addBPI(bpit);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#addToReadyToRunQueueWithTimeout(long,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void addToReadyToRunQueueWithTimeout(long deadline, ICallFrame callFrame) {
        BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, callFrame);
        if (deadline == 0) {
            bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
        } else {
            bpit.setType(BusinessProcessInstanceThread.TIMEOUT);
            bpit.setTimeout(deadline);
        }

        mReadyToRunQueue.addBPI(bpit);
    }

    /**
     * recover the instance
     * @throws Exception 
     */
    public void recoverInstance(String instanceId) throws Exception {
        List list = new ArrayList();
        list.add(instanceId);
        recover(list);

    }

    /*
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#setRecoveryComplete()
     */
    public void setRecoveryComplete() {
        synchronized (mProcessLevelLock) {
            mIsRecoveryComplete = true;
            if (mMaxInstances > 0) {
                //schedule waiting new inbound events.
                checkAndScheduleWaitingInstances();
            }
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#recover(java.util.List)
     */
    public void recover(List bpInstanceIds) throws Exception {
        if (!isPersistenceEnabled()) {
            return;
        }

        mEngine.getStateManager().deleteAllScalabilityPasvtdVars(bpInstanceIds);

        String bpInstanceId = null;
        Collection cfs = null;
        List callFrames = new ArrayList();

        for (int j = 0, Idsize = bpInstanceIds.size(); j < Idsize; j++) {
            try {
                bpInstanceId = (String) bpInstanceIds.get(j);
                cfs = mEngine.getStateManager().getCallframe(this,
                        bpInstanceId, mEngine);

                if (cfs != null) {
                    callFrames.addAll(cfs);
                }
            } catch (RuntimeException e) {
                LOGGER.log(Level.WARNING,
                        I18n.loc("BPCOR-6018: Exception occurred while recovering BPEL instance, with ID: {0}", bpInstanceId), e);
            }
        }

        BusinessProcessInstanceThread bpit = null;
        ICallFrame frame = null;

        for (int i = 0, size = callFrames.size(); i < size; i++) {
            frame = (ICallFrame) callFrames.get(i);

            if ((frame == null) || (frame.getProgramCounter() == null)) {
                continue;
            }

            bpit = new BusinessProcessInstanceThread(this, mEngine, frame);

            if (frame.getProgramCounter() instanceof TimerActUnit) {
                bpit.setType(BusinessProcessInstanceThread.TIMEOUT);
            } else {
                bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
            }

            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPCOR-3004: Adding Business process instance thread (callframe) to " +
                        "ready to run queue for recovery - instance id {0}, callframe id {1}, activity id {2}",
                        frame.getProcessInstance().getId(), frame.getId(),
                        frame.getProgramCounter().getStaticModelActivity().getUniqueId()));
            }


            mReadyToRunQueue.addBPI(bpit);
        }

        // if in maxInstances/throttling mode check and schedule any waiting
        // inbound events
        if (mMaxInstances > 0) {
            synchronized (mProcessLevelLock) {
                mIsRecoveryComplete = true;
                checkAndScheduleWaitingInstances();
            }
        }

    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#recover(java.util.List)
     */
    public BPELProcessManager recreateInstance(String bpInstanceId) throws Exception {
        if (!isPersistenceEnabled()) {
            return null;
        }
        List bpInstanceIds = new ArrayList(1);
        bpInstanceIds.add(bpInstanceId);
        mEngine.getStateManager().deleteAllScalabilityPasvtdVars(bpInstanceIds);

        Collection cfs = null;
        List callFrames = new ArrayList();

        try {
            cfs = mEngine.getStateManager().getCallframe(this, bpInstanceId, mEngine);

            if (cfs != null) {
                callFrames.addAll(cfs);
            } else {
                return null;
            }
        } catch (RuntimeException e) {
            LOGGER.log(Level.WARNING,
                    I18n.loc("BPCOR-6018: Exception occurred while recovering BPEL instance, with ID: {0}", bpInstanceId), e);
            throw e;
        }

        BusinessProcessInstanceThread bpit = null;
        ICallFrame frame = null;


        for (int i = 0, size = callFrames.size(); i < size; i++) {
            frame = (ICallFrame) callFrames.get(i);

            if ((frame == null) || (frame.getProgramCounter() == null)) {
                continue;
            }
//           QName qName =  frame.getProcess().getBPELId();
//           
//           if (!qName .equals(getBPELProcess().getBPELId ())) {
//               manager = mEngine.getBPELProcessManager(qName);
//           }               

            bpit = new BusinessProcessInstanceThread(this, mEngine, frame);

            if (frame.getProgramCounter() instanceof TimerActUnit) {
                bpit.setType(BusinessProcessInstanceThread.TIMEOUT);
            } else {
                bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
            }

            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPCOR-3004: Adding Business process instance thread (callframe) to " +
                        "ready to run queue for recovery - instance id {0}, callframe id {1}, activity id {2}",
                        frame.getProcessInstance().getId(), frame.getId(),
                        frame.getProgramCounter().getStaticModelActivity().getUniqueId()));
            }

            addToReadyToRunQueue(bpit);
        }

        return this;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#process()
     */
    public void process() {
        BusinessProcessInstanceThread bpi = mReadyToRunQueue.getReadyInstance();

        while ((bpi != null) && (mEngine.getState() == Engine.RUNNING)) {
            NDC ndc = null;
            try {
                ndc = NDC.enter("Service Assembly Name", mSAName,
                        "BPEL Process Name", getBPELProcess().getName(),
                        "Process Instance Id", bpi.getProcessInstanceId());
                bpi.execute();
            } finally {
                if (ndc != null) {
                    // exit ndc
                    ndc.exit();
                }
            }
            bpi = mReadyToRunQueue.getReadyInstance();
        }
    }

    /**
     * @see BPELProcessManager#createMessage(com.sun.wsdl.model.common.model.QName)
     */
    public WSMessage createMessage(QName messageName) {
        return new JBIMessageImpl(mBPELProcess.getWSDLMessage(messageName));
    }

    public XSDVariable createXSDVariable(QName name) {
        return new XSDVariableImpl(name);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getCorrMgr()
     */
    public CorrelationManager getCorrMgr() {
        return mCorrMgr;
    }

    public PerformanceManager getPerformanceManger() {
        return mPerformanceManager;
    }

    /**
     * Called when a running instance has finished its execution,
     * enables the process manager to clean up internal data store.
     *
     * SerialProcessing: when engine is in maxInstances/throttling mode
     * the completion of a running instance amounts to one more available
     * execution thread and hence if any events are waiting for execution
     * the first in the head of the list (in incoming order) is picked up
     * and enqueued for execution.
     *
     * @param bp BpelProcessInstance : instances that is completed
     */
    public void instanceComplete(BPELProcessInstance bp) {
        synchronized (mProcessLevelLock) {
            cleanUp(bp);
            bp.instanceComplete();

            if (mEngine.isDebugEnabled() && ((BPELEngine) mEngine).getDebugger() != null) {
                ((DefaultDebugger) ((BPELEngine) mEngine).getDebugger()).processInstanceDied(bp);
            }

            // this method gets called even if the instance 'bp' has faulted or is being terminated.
            // there do not call the monitoring event mechanism with the CompleteEvent for such
            // cases. Extra monitoring persistence hits are avoided in this case.
            if (!(((BPELProcessInstanceImpl) bp).isBeingTerminated())) {
                bp.getMonitorMgr().postInstanceCompleteEvent();
            }


            if (mMaxInstances > 0) {
                if (!mIsRecoveryComplete) {
                    return;
                }

                if (mWaitingMaxedInstances.size() > 0) {
                    WaitingMaxedObj obj = mWaitingMaxedInstances.remove();
                    createInstance(obj.getEvent(), obj.getContents(), obj.getCorrIds(), obj.isJoinFlag());
                }
            }
        }
    }

    public void cleanUp(BPELProcessInstance bp) {
        ((CorrelationManagerImpl) getCorrMgr()).cleanUp(bp);
        mAllInstances.remove(bp);
        mAllInstanceIds.remove(bp.getId());
        BPELHelper.removeUID(bp.getId());
    }

    public void pickMgrCleanUp(ICallFrame callframe) {
        mPickMgr.cleanUp(callframe);
    }

    /*
     * If the engine is in Serial(maxInstances/throttle) mode this method is
     * used to determine if the running instances have not yet reached
     * the maximum instances, then schedule the waiting instances
     * for execution in order of arrival up to the maximum instances
     * permissible.
     */
    private void checkAndScheduleWaitingInstances() {
        // schedule the waiting maxedInstances for execution.
        int waitingInstances = mWaitingMaxedInstances.size();
        if (waitingInstances > 0) {
            int runningInstances = mAllInstances.size();
            int countAvailable = mMaxInstances - runningInstances;
            if (countAvailable > 0) {
                int loopCount = 0;
                if (waitingInstances <= countAvailable) {
                    loopCount = waitingInstances;
                } else {
                    loopCount = countAvailable;
                }
                // schedule the waiting instances.
                for (; loopCount > 0; loopCount--) {
                    WaitingMaxedObj obj = mWaitingMaxedInstances.remove();
                    createInstance(obj.getEvent(), obj.getContents(), obj.getCorrIds(), obj.isJoinFlag());

                }
            }
        }
    }

    private boolean handleRequest(InComingEventKeyImpl event, MessageContainer contents) {
        // check if this event is meant to be for CRMP processing
        if (handleCRMPRequest(event, contents)) {
            return true;
        }

        boolean handled = false;
        InComingEventModel model = event.getEventModel();
        int createFlag = model.getStartElement().getStartType();

        if (createFlag == Engine.RECEIVE_TYPE_CREATE_ONLY) {

            handled = createInstance(event, contents, null, false);
        } else if (createFlag == Engine.RECEIVE_TYPE_CORRELATE_ONLY) {
            List corrIds = mCorrMgr.getCorrelateOnlyIDs(model.getStartElement(),
                    (WSMessage) contents.getContent());
            handled = correlateInstance(event, contents, corrIds);

        } else if (createFlag == Engine.RECEIVE_TYPE_CREATE_OR_CORRELATE) {
            handled = handleCreateOrCorrelate(event, model, contents);
        } else if (createFlag == Engine.RECEIVE_TYPE_NO_COR_JOIN_ONLY) {
            // TODO not yet supported.
            handled = false;
        } else {
            handled = false;
        }

        return handled;
    }

    /**
     * The design aspects for CRMP based recovery are as follows.
     * when engine comes to executing reply:
     * 1) if we can find a live messageExchange waiting for response
     *    a) we send the response
     *    b) wait for done
     *    c) then persist the reply activity as done
     * 2) if there is no active message Exchange waiting for response (happens in the case of recovery
     * 	  or similar situations like failover, clustering)
     * 	  a) we persist the response in CRMP table
     * 	  b) we persist the reply activity as done
     * 	  c) we don't wait for done and continue with the BPEL execution
     *
     * RECOVERY SCENARIOS
     * CASE 1: Crashed occured before inbound request was processed by SubBP
     * In this case the there would be no entry in the CRMP table for this crmpInvokeId and hence
     * a new instance would be created.
     *
     * CASE 2: Crashed after the SubBP processed the Receive activity but not the Reply activity
     * In this case there would be an entry in the CRMP table. If this method accquired the
     * CRMP lock before the reply activity then it would put the request either in the
     * instance request map (if the subBP instance is available/recovered) else in the
     * processManager/instance level map.
     * If this method accquires the lock after the Reply activity has processed it, then the CRMPLookup
     * object would be available. The reply would be send for this Message Exchange. Since the process
     * would have continued after the reply activity, the DONE STATUS would be of no importance but is logged
     * and the message exchange is deemed to have completed.
     *
     * CASE 3: Crashed after the SubBP send reply but the MainBP did not receive the response.
     * In this case the reply would not have been persisted (as reply is only persisted after the STATUS
     * is received for the message exchange), hence the recovery scenario would be the same as CASE2.
     *
     * CASE 4: Crashed after the 'DONE' status is send by the MainBP but SubBP did not receive it.
     * Since the SubBP has not persisted the reply activity(only persisted after the STATUS is received),
     * it would recover from the reply, persist the response object to the CRMP table, complete the reply
     * and proceed to next activity.
     *
     * CASE5: Here after recovery from CASE2, reply activity got the lock first, did not find request,
     * persisted the response object, added the in-memory CRMPLookup, completed reply and continued.
     * The crash happened before the MainBP could re-send the crmp request. On recovery the MainBP would
     * resend the crmp request and in this case since there is no subBP recovery to put the in-memory
     * CRMPLookup object, the response object would have to be got from the DB. (this would in all
     * probability be a very rare recovery case senario)
     *
     * IMP: After recovery the reply activty would continue irrespective of the Status for the InOut ME,
     * this is the design.
     *
     * @param event Inbound event
     * @param contents MessageContainer associated with the Request
     * @return boolean true if this request was meant and processed for CRMP,
     * false if the request is meant for non-crmp and CRMP instance creation cases.
     * @throws Exception
     */
    private boolean handleCRMPRequest(InComingEventKeyImpl event,
            MessageContainer contents) {
        String crmpInvokeId = contents.getCRMPInvokeId();
        if (isPersistenceEnabled() && crmpInvokeId != null) {
            long replyVarId = -1;
            Object lookup = null;

            synchronized (mProcessLevelLock) {
                CRMPDBO crmpDbo = null;
                try {
                    crmpDbo = mEngine.getEngStateMgr().getCRMPDBOForCRMPInvokeID(crmpInvokeId, false);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
                if (crmpDbo == null) {
                    return false; // Request is meant to create a new Instance
                }
                String bpId = crmpDbo.getBPId();
                String partnerLink = crmpDbo.getPartnerLink();
                String operation = crmpDbo.getOperation();
                String msgExch = crmpDbo.getBpelMessageExchange();
                replyVarId = crmpDbo.getReplyVariableId();

                String key = bpId + partnerLink + operation; //future may need to include bpel message exchange id.
                lookup = mCRMPReplyMap.get(key);
                if (replyVarId == -1) { //CASE2 (lock accquired before reply activity)
                    RStartElement elem = mBPELProcess.getStartElement(
                            partnerLink, operation, msgExch);
                    if (elem == null) {
                        throw new RuntimeException(I18n.loc("BPCOR-6019: CRMP failed, there is no start activity " +
                                "for this request: {0}", key));
                    }
                    if (!event.getEventModel().getOperPattern().equals(
                            Engine.IN_ONLY)) {
                        Context inst = getInstanceForBpId(bpId);
                        if (inst != null) {
                            // add the duplicate request to the context request
                            // map.
                            inst.addRequest(elem, contents);
                        } else {
                            addCRMPReqForRecoveringInsts(key, contents);
                        }
                    }
                    return true;
                }
            }
            //CASE2 (lock acquired after reply activity).
            RuntimeVariable rVar = null;
            String meId = contents.getId();
            if (lookup != null) {
                // The reply activity was already executed and hence the in-memory CRMPLookup is available
                rVar = ((CRMPLookup) lookup).getMVariable();
                ICallFrame frame = ((CRMPLookup) lookup).getMFrame();
                // need to process status
                // send the response for the request
                WSMessage output = rVar.getWSMessage();
                if (output == null) {
                    throw new StandardFaultException(
                            I18n.loc("BPCOR-6020: Cannot respond with uninitialized variable: {0}",
                            rVar.getVariableDef().getName()));
                }

                MessageContainer con = MessageContainerFactory.createMessage(
                        meId, output, null, null);
                sendReply(con, null);
                return processStatusForCRMP(frame, meId);
            } else { //CASE 5

                MessageContainer con = getMessageContainerForCRMP(crmpInvokeId, meId, replyVarId);
                sendReply(con, null);
                return true;
            }
        }
        return false; // continue with normal processing
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getResponsesForClusteredSubBPInvokes()
     */
    public void getResponsesForClusteredSubBPInvokes() {
        BPITForClusteredInvoke bpitForClustedInvoke = null;

        synchronized (mProcessLevelLock) {
            Iterator iter = mResponsePendingInstances.keySet().iterator();

            while (iter.hasNext()) {
                InComingEventKeyImpl event = (InComingEventKeyImpl) iter.next();
                bpitForClustedInvoke = (BPITForClusteredInvoke) mResponsePendingInstances.get(event);
                boolean receivedResponse = bpitForClustedInvoke.getInvokeCRMPResponse();
                if (receivedResponse) {
                    mResponsePendingInstances.remove(event);
                    ICallFrame frame = bpitForClustedInvoke.getCallFrame();
                    frame.getProcessInstance().removeFromInactiveList(frame, false);
                    mReadyToRunQueue.addBPI(bpitForClustedInvoke);
                }
            }
        }
    }

    public MessageContainer getMessageContainerForCRMP(String crmpInvokeId, String meId, long replyVarId) {
        QueryRespCRMPDBO respDBO;
        try {
            respDBO = mEngine.getEngStateMgr().getResponseObjForCrmpInvokeId(crmpInvokeId);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        /* The response object cannot be null for this case. The subBP instance should have updated
         * the crmp table. There is unexpected condition for now we will throw a StandardFault for this case.
         */
        if (respDBO == null) {
            throw new StandardFaultException(
                    I18n.loc("BPCOR-6021: CRMP failure, there is no response object to send the reply: {0}",
                    crmpInvokeId));
        }
        Reader reader = respDBO.getResponseObj();
        RVariable var = mBPELProcess.getVariable(replyVarId);
        WSMessage output = Utility.getWSMessage(var, reader);
        if (output == null) {
            throw new StandardFaultException(I18n.loc("BPCOR-6020: Cannot respond with uninitialized variable: {0}",
                    var.getName()));
        }
        MessageContainer con = MessageContainerFactory.createMessage(
                meId, output, null, null);
        return con;
    }

    /**
     * The STATUS processing for the CRMP Recovery cases 2 where the calling method acquired the CRMP lock
     * after the reply activity had finished processing. In these cases the STATUS returned from the MainBP
     * after sending the reply would have no bearing on the process (as per design) is logged here.
     * In the future we may have to design for error status??
     * @param frame
     * @param msgExchangeId
     * @return
     */
    private boolean processStatusForCRMP(ICallFrame frame, Object msgExchangeId) {
        ResponseInComingEventKeyImpl rEvent = new ResponseInComingEventKeyImpl(
                frame.getProcess(), Event.DONE, msgExchangeId);
        Map eventDoneMap = getEventDoneMap();
        MessageContainer content = null;
        synchronized (eventDoneMap) {
            content = (MessageContainer) eventDoneMap.remove(rEvent);
        }
        if (content != null) {
            // the done status was received for the CRMP recovery case log it and return.
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE,
                        I18n.loc("BPCOR-3005: CRMP recovery: Status was received for the message exchange id: {0}",
                        msgExchangeId));
            }
        }
        return true;

    }

    /*
     * Refer to BPELProcessManager.removeCRMPReqForRecoveringInsts
     * Api not thread safe, its the duty of the caller to ensure thread safety
     */
    public MessageContainer removeCRMPReqForRecoveringInsts(String key) {
        MessageContainer container = (MessageContainer) mCRMPReqsForRecoveringInsts.remove(key);
        if (container != null) {
            return container;
        }
        return null;
    }

    /*
     * Refer to BPELProcessManager.addCRMPReqForRecoveringInsts
     * Api not thread safe, its the duty of the caller to ensure thread safety
     */
    public void addCRMPReqForRecoveringInsts(String key, MessageContainer req) throws BPELRuntimeException {
        mCRMPReqsForRecoveringInsts.put(key, req);
    }

    /*
     * Refer to BPELProcessManager.getCRMPMonitor
     */
    public Object getCRMPMonitor() {
        return mProcessLevelLock;
    }

    /*
     * This is used only from the Reply activity and where is part of a sychronized block update
     * hence if used otherwise, synchronization would have to be considered.
     *
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#putReplyInCRMPReplyMap(java.lang.String,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.impl.CRMPLookup)
     */
    public void putReplyInCRMPReplyMap(String key, CRMPLookup value) {
        mCRMPReplyMap.put(key, value);
    }

    private ICallFrame getPendingInstance(InComingEventKeyImpl event, MessageContainer contents) {

        ICallFrame frame = null;
        String scalPassInstanceId = null;

        synchronized (mProcessLevelLock) {
            frame = (ICallFrame) mRequestPendingInstances.remove(event);

            if (frame != null) {
                // NOTE: Don't combine the following call with next call
                // to pick manager as, if the frame is found in pick queues
                // it will be removed from the in active list in that call.
                frame.getProcessInstance().removeFromInactiveList(frame, false);
            } else {
                frame = mPickMgr.getPendingInstance(event);
            }

            if (frame != null) {
                /*
                 * If a thread is waiting for this event, put the event in the queue
                 * so that the instance can pick it up.
                 */
                Utility.addToListInMap(event, mEventRequestMap, contents);
                return frame;
            }

            /*
             * If frame is null and persistence is enabled, this could happen for one of
             * the following situations
             * For engine running on cluster:
             *  -- the instance was passivated; The instance need to be activated again
             *
             *  For single engine
             *  -- the instance was scalability passivated.
             */

            if (frame == null) {
                if (isPersistenceEnabled()) {

                    StateManager mgr = mEngine.getStateManager();

                    if (mEngine.isClustered()) {

                        List<InComingEventKeyImpl> eventsList = new ArrayList<InComingEventKeyImpl>();
                        eventsList.add(event);

                        List<String> instances = mgr.activateInstancesAndUpdateOwnership(mEngine.getId(), eventsList);

                        if (instances.size() > 1) {
                            LOGGER.log(Level.SEVERE,
                                    "Multiple instances found. Persistence database is corrupted");
                        } else if (instances.size() == 1) {
                            String instanceId = instances.get(0);

                            LOGGER.log(Level.INFO, I18n.loc("BPCOR-5014: Engine running in Cluster Mode, " +
                                    " activating the instance with id {0}", instanceId));


                            /*
                             * When the instance is not null, This means -
                             * The instance was passivated when the execution reached correlated receive.
                             * The instance will be activated (recovered)
                             */
                            Utility.addToListInMap(event, mEventRequestMap, contents);
                            try {
                                recoverInstance(instanceId);
                            } catch (Exception ex) {
                                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6022: Failed During Recovery of correlated " +
                                        "instance in multiple engine"), ex);
                            }
                        } else {
                            /*
                             * case 2.
                             * The frame is null and instance is not found in the database.
                             * This is out of order message. The request that would create the instance can arrive
                             * on other engine. We don't want to store this (out of order) message in the mEventRequestMap
                             * because the failover thread then would need to periodically scan the mEventRequestMap
                             * and check if the instance for this request is available in the database (at some point
                             * the other engine would passivate the instance). Keeping the out or order request on
                             * mEventRequestMap could have performance implication on normal execution.Hence created
                             * new map mEventRequestMapForCorr for keeping such requests.
                             */
                            addToCorrelatedWaitingEventsMap(event, contents);
                            mReqLifeSpanHelper.addMessage(event, getLifeSpan());
                        }

                    } else {
                        /*
                         * The instance could be scalability passivated. Check the
                         * database. NOTE: It is conscious decision not to keep
                         * correlation values in the memory as the foot print for
                         * the correlation value will be large, as a result we need
                         * to make a database call only when there are passivated
                         * instances as indicated by the size of
                         * mScalabltyPassvtdInstsSet
                         */
                        if (mMemoryManager.areInstancesScalPassivted()) {
                            scalPassInstanceId = mMemoryManager.getScalabilityPassivatedInstance(((CorrelatingSAInComingEventKeyImpl) event).getCorrIds());
                        }
                        /*
                         * This is single engine case, following conditions apply:
                         * Case 1: The instance was scalability passivated, if yes,
                         * at this point it will be recovered. Case 2: This was out
                         * of order correlated message, in which this message will
                         * be put on event request map and wherever the execution
                         * reaches IMA execution for this event, this message will
                         * be picked up. The behavior is similar to non-persistence
                         * mode.
                         */
                        Utility.addToListInMap(event, mEventRequestMap, contents);
                        mReqLifeSpanHelper.addMessage(event, getLifeSpan());
                    }
                } else {
                    Utility.addToListInMap(event, mEventRequestMap, contents);
                    mReqLifeSpanHelper.addMessage(event, getLifeSpan());
                }
            }
        }

        if (scalPassInstanceId != null) {
            mMemoryManager.activateScalabilityPassInstance(scalPassInstanceId, InactivityReason.PENDING_REQUEST);
        }

        return null;
    }

    /**
     * Remove business process instance wrapper from ready to run queue
     *
     * @param bpit business process instance wrapper to be removed
     * @return true if this contains bpit and successfully removed it
     */
    public boolean removeFromReadyToRunQueue(BusinessProcessInstanceThread bpit) {
        return mReadyToRunQueue.remove(bpit);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#createBPInstance(com.sun.jbi.engine.bpel.core.bpel.model.meta.RBPELProcess,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.Engine, java.lang.String)
     */
    public BPELProcessInstance createBPInstance(String bpInstanceId) {
        BPELProcessInstanceImpl instance = null;
        try {
            instance = new BPELProcessInstanceImpl(this, mEngine, bpInstanceId);
            mAllInstanceIds.add(instance.getId());
            mAllInstances.add(instance);
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("Instance Id {0} is created, {1} BP instances are in memory",
                        instance.getId(),
                        mAllInstanceIds.size()));
            }

            if (mEngine.isDebugEnabled() && ((BPELEngine) mEngine).getDebugger() != null) {
                ((DefaultDebugger) ((BPELEngine) mEngine).getDebugger()).processInstanceStarted(instance);
            }
            // Post start events only when the instance is a new instance
            if (bpInstanceId == null) {
                instance.getMonitorMgr().postInstanceStartEvent();
            } else {
                // this is to update the in-memory instance count in the 
                // monitoring console for the recovered instance.
                instance.getMonitorMgr().postInstanceRecoveredEvent();
            }
        } catch (RuntimeException e) {
            BPELTraceManager.getInstance().alertUnableToCreateInstance(
                    mEngine.getId(), mBPELProcess.getBPELId().toString(), e);
            throw e;
        }
        return instance;
    }

    private long getLifeSpan() {
        if (mWaitingRequestLifeSpan.intValue() < 0) {
            // Devnote: Always get the value from the engine. Do not store the value from the engine locally, since
            // the engine value can be updated dynamically at runtime.
            return mEngine.getWaitingRequestLifeSpan();
        }

        return mWaitingRequestLifeSpan.longValue();
    }

    private boolean createInstance(InComingEventKeyImpl event,
            MessageContainer contents,
            List corrIds,
            boolean joinFlag) {
        boolean handled = true;
        BPELProcessInstance instance;
        /* if in maxInstances/throttle mode check to see if this
         * new request can create an instance, otherwise maintain
         * it in a list in order of arrival to processed when
         * maxInstance threshold has been lowered by the completion
         * of running instances.
         */
        if (mMaxInstances > 0) {
            synchronized (mProcessLevelLock) {
                if (!mIsRecoveryComplete || (mAllInstances.size() >= mMaxInstances)) {
                    mWaitingMaxedInstances.add(new WaitingMaxedObj(event, contents, corrIds, joinFlag));
                    return handled;
                }
                // have to be called in the same sync block as another thread could
                // create an instance.
                instance = (BPELProcessInstance) createBPInstance(null);
            }
        } else {
            instance = (BPELProcessInstance) createBPInstance(null);
        }

        ICallFrame frame = mEngine.getInterpreter().createCallFrame(mBPELProcess, instance, (ActivityUnit) instance, null);
        instance.addCallFrame(frame);
        BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, frame);
        bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
        bpit.setMessageExchangeKey(contents.getId());

        InComingEventKeyImpl eventWrp = null;

        if (joinFlag) {
            mCorrMgr.associateInstance(corrIds, frame.getProcessInstance());
            eventWrp = new CorrelatingSAInComingEventKeyImpl(event, corrIds);
        } else {
            eventWrp = new SAInComingEventKeyImpl(event, frame.getBPId());
        }

        addRequest(eventWrp, contents);

        /*
         * RStartElement act = event.getEventModel().getStartElement(); RequestReplyKeyImpl key =
         * new RequestReplyKeyImpl( act.getRPartner(), act.getRPortType(),
         * act.getWSDLOperation().getName(), null, act.getMessageExchange(), frame.getBPId() );
         * mRequestReplyMap.put(key, contents.getId());
         */
        mReadyToRunQueue.addBPI(bpit);

        return handled;
    }

    private boolean correlateInstance(InComingEventKeyImpl event,
            MessageContainer contents,
            List corrIds) {
        boolean handled = false;

        InComingEventKeyImpl key = new CorrelatingSAInComingEventKeyImpl(event, corrIds);

        // is thread waiting for this incoming message?
        // If no thread is waiting the event is put on the queue.
        ICallFrame frame = getPendingInstance(key, contents);

        if (frame != null) {
            // put the bpit in the readyToRunQueue
            BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, frame);
            bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
            bpit.setMessageExchangeKey(contents.getId());

            mReadyToRunQueue.addBPI(bpit);
            handled = false;
        }
        /*
         * RStartElement act = event.getEventModel().getStartElement(); RequestReplyKeyImpl
         * reqReplykey = new RequestReplyKeyImpl( act.getRPartner(), act.getRPortType(),
         * act.getWSDLOperation().getName(), null, act.getMessageExchange(), frame.getBPId() );
         * mRequestReplyMap.put(reqReplykey, contents.getId());
         */
        return handled;
    }

    private boolean handleCreateOrCorrelate(InComingEventKeyImpl event,
            InComingEventModel model,
            MessageContainer contents) {
        boolean handled = true;

        // Only considers correlations with initiate flag = "join" and the correlations
        // that are common to all the create_or_correlate startelements.
        List corrIds = mCorrMgr.getCommonJoinOnlyIDs(model.getStartElement(),
                (WSMessage) contents.getContent());

        BPELProcessInstance bpi = mCorrMgr.getInstance(corrIds);

        if (bpi == null) {

            synchronized (mProcessLevelLock) {
                bpi = mCorrMgr.getInstance(corrIds);

                if (bpi == null) {
                    // create a new instance
                    handled = createInstance(event, contents, corrIds, true);
                }
            }
        }

        if (bpi != null) {
            // NOTE: don't put this "if" as an "else" to the above (bpi == null) condition.
            // due to the check-lock-check pattern used, It is like this.
            // correlate with an existing instance
            handled = correlateInstance(event, contents, corrIds);
        }

        /**
         * Above code could also be written as following. If the following is easy to understand, we
         * should use that code instead of the above. if (bpi != null) { // correlate with an
         * existing instance handled = correlateInstance(event, contents, bpi, corrIds); } else {
         * synchronized (mCREATE_OR_CORRELATE_Lock) { bpi = mCorrMgr.getInstance(corrIds); if (bpi ==
         * null) { // create a new instance corrIds =
         * mCorrMgr.getCreateOrCorrelateIDs(model.getStartElement(), (WSMessage)
         * contents.getContent(), true); handled = createInstance(event, contents, corrIds); }} if
         * (bpi != null) { // correlate with an existing instance handled = correlateInstance(event,
         * contents, bpi, corrIds); }}
         */
        return handled;
    }

    public List getInstances() {
        return mAllInstanceIds;
    }

    public List getNonExpiredOnAlarmRunningInstances() {
        return nonExpiredOnAlarmRunningInstances;
    }

    public void addNonExpiredOnAlarmRunningInstances(NonExpiredOnAlarmInstanceInfo instanceInfo) {
        nonExpiredOnAlarmRunningInstances.add(instanceInfo);
    }

    public List getProcessInstances() {
        return mAllInstances;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getMonitorManager(java.lang.String)
     */
    public MonitorManager getMonitorManager(String instanceId) {
        // first check the memory.
        MonitorManager monitorManager = getProcessMonitor(instanceId);

        if (monitorManager == null && isPersistenceEnabled()) {
            // instance not found in memory. 
            // the instance might have been scalability passivated, activate the instance
            // if it is of waiting type (defined with wait), activate it.
            try {
                boolean isActivated = mMemoryManager.activateInstanceForMonitor(instanceId);
                if (isActivated) {
                    monitorManager = getProcessMonitor(instanceId);
                }

            } catch (Exception e) {
                e.printStackTrace();
                throw new RuntimeException(
                        "Exception happened while recovery the Scalability Passivated instance with ID : " + instanceId);
            }
        }
        return monitorManager;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getProcessInstances(java.lang.String)
     */
    public Collection<BPELProcessInstance> getProcessInstances(String bpelId) {

        if (isPersistenceEnabled()) {
            mMemoryManager.activateWaitingInstancesForMonitor();
        }

        if (isPersistenceEnabled() && mMemoryManager.areInstancesScalPassivted()) {
            mMemoryManager.activateInstancesForMonitor(bpelId);
            // at this point all the scalability passivated (waiting and status/response pending)
            // instance should be recovered and will be available in the mAllInstances list,
            // return this list.
        }

        return mAllInstances;
    }

    private MonitorManager getProcessMonitor(String instanceId) {
        BPELProcessInstance processInstance = null;
        Iterator iter = mAllInstances.iterator();
        while (iter.hasNext()) {
            processInstance = (BPELProcessInstance) iter.next();
            if (processInstance.getId().equals(instanceId)) {
                return processInstance.getMonitorMgr();
            }
        }
        return null;
    }

    public void addToReadyToRunQueue(BusinessProcessInstanceThread bpit) {
        mReadyToRunQueue.addBPI(bpit);
    }

    public int activateClusteringPassInstance(String instanceId) {
        StateManager mgr = mEngine.getStateManager();
        // Although, we don't need to pass the engine id, but due
        // to persistence framework limitations (we don't want to create
        // another DB Object), the engineid is passed to use the same
        // update query for activating and passivating instance
        // and also updating the instance ownership by another engine.

        int updateCount = mgr.activateInstance(instanceId, mEngine.getId());

        if (updateCount == 1) {
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPCOR-3006: Instance Id {0} * Activated * by Engine {1}",
                        instanceId, mEngine.getId()));
            }
        }
        return updateCount;
    }

    private Context getInstanceForBpId(String bpId) {
        synchronized (mProcessLevelLock) {
            Iterator iter = mAllInstances.iterator();
            while (iter.hasNext()) {
                BPELProcessInstanceImpl inst = (BPELProcessInstanceImpl) iter.next();
                if (bpId.equals(inst.getId())) {
                    return inst;
                }
            }
        }
        return null;
    }

    private static class MessageContainerForPickImpl implements MessageContainerForPick {

        /** incoming event key */
        InComingEventKeyImpl mEvent;
        /** message container */
        MessageContainer mMesgCont;

        /**
         * Creates a new MessageContainerForPickImpl object.
         *
         * @param event incoming event key
         * @param msgCont message container
         */
        MessageContainerForPickImpl(InComingEventKeyImpl event, MessageContainer msgCont) {
            mEvent = event;
            mMesgCont = msgCont;
        }

        /**
         * @see BPELProcessManager.MessageContainerForPick#getInComingEventKey()
         */
        public InComingEventKeyImpl getInComingEventKey() {
            return mEvent;
        }

        /**
         * @see BPELProcessManager.MessageContainerForPick#getMessageContainer()
         */
        public MessageContainer getMessageContainer() {
            return mMesgCont;
        }
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#cleanUpEventHandlerCallFrames(java.util.Set, java.util.Set)
     */
    public void cleanUpEventHandlerCallFrames(Set<ICallFrame> eventCallFrames, Set<ICallFrame> alarmCallFrames) {
        synchronized (mProcessLevelLock) {
            // Remove the onEvent call frames from the pending queue.
            mRequestPendingInstances.values().removeAll(eventCallFrames);
            // Remove onAlarm call frames from the
            mReadyToRunQueue.expireTimeoutBpit(alarmCallFrames);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#activateWaitingCallFrames(java.util.Collection)
     */
    public void activateWaitingCallFrames(Collection callFrames) {

        ArrayList<ICallFrame> callFramesToRemove = new ArrayList<ICallFrame>();

        synchronized (mProcessLevelLock) {
            Iterator iter = callFrames.iterator();
            while (iter.hasNext()) {
                ICallFrame callFrame = (ICallFrame) iter.next();
                //remove the callframe from the pending queue
                if (mRequestPendingInstances.values().remove(callFrame) || (mPickMgr.instancePending(callFrame) && (mPickMgr.removePendingInstanceAndCheckIfTimerFired(callFrame) == false))) {
                    callFramesToRemove.add(callFrame);
                    BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, callFrame);
                    bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
                    mReadyToRunQueue.addBPI(bpit);
                }
            }

            callFrames.removeAll(callFramesToRemove);
            callFramesToRemove.clear();

            iter = callFrames.iterator();
            while (iter.hasNext()) {
                ICallFrame callFrame = (ICallFrame) iter.next();
                if (mDonePendingInstances.values().remove(callFrame)) {
                    callFramesToRemove.add(callFrame);
                    BusinessProcessInstanceThread bpit = new BusinessProcessInstanceThread(this, mEngine, callFrame);
                    bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
                    mReadyToRunQueue.addBPI(bpit);
                }
            }

            callFrames.removeAll(callFramesToRemove);
            callFramesToRemove.clear();

            ArrayList foundBpitList = new ArrayList();

            iter = mResponsePendingInstances.keySet().iterator();
            while (iter.hasNext()) {
                ResponseInComingEventKeyImpl event = (ResponseInComingEventKeyImpl) iter.next();
                BusinessProcessInstanceThread bpit = (BusinessProcessInstanceThread) mResponsePendingInstances.get(event);
                ;
                boolean callFrameFound = callFrames.remove(bpit.getCallFrame());
                if (callFrameFound) {
                    foundBpitList.add(bpit);
                    addExpiredInvokeResponse(event);
                    mReadyToRunQueue.addBPI(bpit);
                }
            }

            mResponsePendingInstances.values().removeAll(foundBpitList);
        }

        mReadyToRunQueue.expireTimeoutBpit(callFrames);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#addExpiredInvokeResponse(
     * com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl)
     */
    public void addExpiredInvokeResponse(ResponseInComingEventKeyImpl event) {
        expiredInvokeResponse.add(event);
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#isPersistenceEnabled()
     */
    public boolean isPersistenceEnabled() {
        return ((!mPersistenceOff) && (mEngine.isPersistenceEnabled()));
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#ignoreMissingFromData()
     */
    public boolean ignoreMissingFromData() {
        return mIgnoreMissingFromData;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#isBPAtomic()
     */
    public boolean isBPAtomic() {
        return mIsBPAtomic;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#persistenceOptOut()
     */
    public boolean persistenceOptOut() {
        return mPersistenceOptOut;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getGenerateEventsFlag()
     */
    public boolean getGenerateEventsFlag() {
        return mGenerateEvents;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#setGenerateEventsFlag(boolean)
     */
    public void setGenerateEventsFlag(boolean eventsFlag) {
        mGenerateEvents = eventsFlag;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#purgeWaitingRequests()
     */
    public void purgeWaitingRequests() {
        synchronized (mProcessLevelLock) {
            List<InComingEventKeyImpl> expiredReqList = mReqLifeSpanHelper.purgeWaitingRequests();
            if (expiredReqList != null) {
                for (InComingEventKeyImpl event : expiredReqList) {
                    MessageContainer msgContainer;
                    msgContainer = (MessageContainer) Utility.removeFromListInMap(event, mEventRequestMap);
                    if (msgContainer == null) {
                        msgContainer = receiveRequestForCorrWaitingEvents(event);
                    }
                    Exception msgTimedOutExc = new Exception(I18n.loc("BPCOR-6119: Request timed out. There was " +
                            "no business process instance which was able to process this request in the " +
                            "configured time of {0} seconds.", getLifeSpan()));
                    sendRequestError(msgContainer.getId(), msgTimedOutExc);
                }
            }
        }
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#doPhase1ScalabilitySolution(long)
     */
    public void doPhase1ScalabilitySolution(long memRelTimeCriterion) {
        if (!isPersistenceEnabled()) {
            return;
        }
        mMemoryManager.doPhase1ScalabilitySolution(memRelTimeCriterion);
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#doPhase2ScalabilitySolution(long)
     */
    public void doPhase2ScalabilitySolution(long instPassivationTimeCriterion) {
        if (!isPersistenceEnabled()) {
            return;
        }
        mMemoryManager.doPhase2ScalabilitySolution(instPassivationTimeCriterion);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
     */
    public DocumentFragment getExternalEndPoint(PartnerLink partnerLink) {
        return ((EngineImpl) mEngine).getChannel().getExternalEndPoint(partnerLink);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getExternalEndPoint(com.sun.bpel.model.PartnerLink)
     */
    public DocumentFragment getEndPointReference(PartnerLinkScope plScope, PartnerLink partnerLink, boolean isMyRole) {
        return ((EngineImpl) mEngine).getChannel().getEndpointReference(plScope, partnerLink, isMyRole);
    }

    public class WaitingMaxedObj {

        InComingEventKeyImpl mEvent;
        MessageContainer mContents;
        List mCorrIds;
        boolean mJoinFlag;

        public WaitingMaxedObj(InComingEventKeyImpl event, MessageContainer contents,
                List corrIds, boolean joinFlag) {
            mEvent = event;
            mContents = contents;
            mCorrIds = corrIds;
            mJoinFlag = joinFlag;
        }

        public InComingEventKeyImpl getEvent() {
            return mEvent;
        }

        public MessageContainer getContents() {
            return mContents;
        }

        public List getCorrIds() {
            return mCorrIds;
        }

        public boolean isJoinFlag() {
            return mJoinFlag;
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager
     * 	#addThrottlingBPIT(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, com.sun.jbi.common.descriptor.EndpointInfo, int)
     */

    @Deprecated
    public void addThrottlingBPIT(ICallFrame frame, EndpointInfo endpointInfo,
            int throttleCount) {
        synchronized (mProcessLevelLock) {

            ThrottlingConfigInfo configInfo = null;

            boolean hasEntry = mThrottleConfigMap.containsKey(endpointInfo);
            if (!hasEntry) {
                configInfo = new ThrottlingConfigInfo(endpointInfo, throttleCount, mBPELProcess.getBPELId());
                mThrottleConfigMap.put(endpointInfo, configInfo);
            }

            BPITForThrottling bpit = new BPITForThrottling(this,
                    mEngine, frame, endpointInfo, throttleCount);

            bpit.setType(BusinessProcessInstanceThread.NON_TIMEOUT);

            addToReadyToRunQueue(bpit);
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager
     * 	#addTransactionSyncBPIT(com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance , com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void addTransactionSyncBPIT(ICallFrame frame, TxPropagationObject txPropObj, int transactionstatus) {
        BPITForTransactionSync bpitTxn = new BPITForTransactionSync(this, this.mEngine, frame, txPropObj, transactionstatus);
        bpitTxn.setType(BusinessProcessInstanceThread.NON_TIMEOUT);
        this.addToReadyToRunQueue(bpitTxn);
        mEngine.notifyThreads();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager
     * 	#decrementThrottlingCount(com.sun.jbi.common.descriptor.EndpointInfo)
     */
    @Deprecated
    public void decrementThrottlingCount(EndpointInfo info) {
        synchronized (mProcessLevelLock) {
            ThrottlingConfigInfo configInfo =
                    mThrottleConfigMap.get(info);
            configInfo.decrementProcessingCount();
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager
     * 	#canConsumeService(com.sun.jbi.common.descriptor.EndpointInfo)
     */
    @Deprecated
    public boolean canConsumeService(EndpointInfo info) {
        synchronized (mProcessLevelLock) {
            ThrottlingConfigInfo configInfo = mThrottleConfigMap.get(info);
            if (configInfo.isReady()) {
                configInfo.incrementProcessingCount();
                return true;
            }
            return false;
        }
    }

    public String getServiceAssemblyName() {
        return mSAName;
    }

    public String getServiceUnitName() {
        return mSUName;
    }

    public Map getResponsePendingInstances() {
        return mResponsePendingInstances;
    }

    public Map getPickRequestPendingInstances() {
        return mPickMgr.getPickRequestPendingInstances();
    }

    public Map getDonePendingInstances() {
        return mDonePendingInstances;
    }

    public Map getRequestPendingInstances() {
        return mRequestPendingInstances;
    }

    public ScalabilityManager getMemoryMgr() {
        return mMemoryManager;
    }

    public XslCache getXslCache() {
        return mXslCache;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#setXsltCache(java.util.Map) */
    public void setXslCache(XslCache xslCache) {
        mXslCache = (xslCache == null) ? new XslCache() : xslCache;
    }

    public Map<String, Object> getApplicationVariables() {
        return mAppVarsCache;
    }

    public void setApplicationVariables(Map<String, Object> appVars) {
        mAppVarsCache = appVars == null ? new HashMap<String, Object>() : appVars;
    }

    /*
     *
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#setAtomicTxType(String type)
     */
    public void setAtomicTxType(String type) {
        this.mAtomicTxType = type;
    }

    /*
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager#getAtomicTxType()
     */
    public String getAtomicTxType() {
        return mAtomicTxType;
    }

    public List<String> getExtraNDC() {
        return mExtraNDC;
    }

    public boolean isLoggingEnabled() {
        return mIsLoggingEnabled;
    }

    public boolean isReadyToRecover() {
        return mEngine.isSUStarted(mSUName);
    }

    /**
     * Method that will return a map of the total count for the various states of the 
     * instances in the monitoring instance table. 
     * This method is called when the engine initializes the <code>BPELProcessManager</code>
     * for a BPEL process. This method is called only if the engine property of <code>MonitorEnabled</code>
     * is enabled. This method is fail-safe in that it will not hamper the loading of the 
     * BPEL process if there an exception in retreving the values from the monitoring DB, but 
     * will log the event as a WARNING.  
     * @param processQName, the process id (QName) of the bpel process.
     * @return Map<String, Integer>, ex {{RUNNING, 10}, {COMPLETED, 100}...}
     * @throws Exception
     */
    private Map<String, Integer> getMonitorInstanceSyncCount(String processQName) {
        Map<String, Integer> sMap = new HashMap<String, Integer>();
        ResultSet rs = null;
        AbstractDBConnection dbConn = null;
        try {
            dbConn = mEngine.getDBConnectionFactory().createNonXAConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            List<String> queryVals = new ArrayList<String>();
            queryVals.add(processQName);
            rs = dbConn.get(BPELEventPersister.MONITOR_INSTANCE_COUNT_SYNC_QUERY, queryVals);

            while (rs.next()) {
                String status = rs.getString(1);
                int count = rs.getInt(2);
                sMap.put(status, new Integer(count));
            }

        } catch (Exception ex) {
            //throw new RuntimeException("Monitor sync count DB failure: " + ex);
            LOGGER.log(Level.WARNING,
                    I18n.loc("BPCOR-7144: fetch of instance count to synch monitor console failed"), ex);

        } finally {
            if (rs != null) {
                try {
                    java.sql.Statement stmt = rs.getStatement();
                    // closing the statement should close its current resultset object.
                    stmt.close();
                } catch (SQLException sqlEx) {
                    LOGGER.log(Level.WARNING,
                            I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), sqlEx);

                }
            }
            if (dbConn != null) {
                try {
                    // closing the wrapper will set the initial value of the setAutoCommit Field.
                    dbConn.close();
                } catch (SQLException sqlEx) {
                    LOGGER.log(Level.WARNING,
                            I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), sqlEx);
                }
            }
        }
        return sMap;
    }
}
