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
 * @(#)EngineImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.ri.JXPathContextReferenceImpl;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionConfiguration;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.debug.DebugServer;
import com.sun.jbi.engine.bpel.core.bpel.debug.DefaultDebugger;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Channel;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.ThreadManager;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventManager;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsConfigurationHelper;
import com.sun.jbi.engine.bpel.core.bpel.event.ProcessEventsConfig;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELConfigurationException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.MonitorManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.NonExpiredOnAlarmInstanceInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation.STATUS;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.EngineStateManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELXPathContextFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.xpath.dom.BPELSEDOMPointerFactory;
import java.util.Collections;

/**
 * Engine implementation
 * 
 * @author Sun Microsystems
 */
public class EngineImpl implements Engine, BPELEngine {

	private static final Logger LOGGER = Logger.getLogger(EngineImpl.class.getName());

    private static final String TRUE = "true"; //$NON-NLS-1$

    private BPELInterpreter mInterpreter;

    private Channel mChannel;

    private String mId;

    private StateManager mStateMgr;

    private EngineStateManager mEngStateMgr;
    
    private ThreadManager threadMgr;

    private boolean mReliabilityEnabled = true;

    private boolean mPersistEnabled;
    
    private boolean mMonitorEnabled;
    
    private boolean mKPIEnabled;
    
    private boolean mVariableMonitorEnabled;

    private long mHeartbeatUpdateConfigTime = -1;
    
    private boolean mScalabilityInProgress = false;
    
    private boolean mDebugEnabled;
    
    private long mWaitingRequestLifeSpan;

    private String mDebugPort = "0"; //$NON-NLS-1$

    private TransformEngine mTransformEngine = TransformEngine.XSLT_1_0;

    private Hashtable mBPELProcessIDMap = new Hashtable();

    private Hashtable mBPELProcessManagerMap = new Hashtable();
    
    private Map<QName, ProcessEventsConfig> mBPELProcessEventsConfigMap = null;

    /** A lock for changing the engine state * */
    private Integer mStatusLock = new Integer(0);

    private int mState = RUNNING;

    private String mLocation = null;

    private boolean mNewAddedDep = false;

    private final Object newDeploymentLock = new Object();

    private Properties mProperties;

    private InitialContext mInitialContext;

    private boolean mIsClustered = false;
    
    private DebugServer mDebugServer = null;
    private BPELDebugger mDebugger = null;

    private DeploymentLookup mDeploymentLookup = null;

    private Map<String, Object> mApplicationVariables;

    private boolean mValidationEnabled;

    /**
     * The com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionConfiguration obj
     * will be set by the Context to address the BPEL engine's connection creation
     * needs.
     */
    private ConnectionConfiguration mConnConfig = null;
    
    private BPELEventManager mEventManager = null;
    
    private Object mMemorySweepLock = new Object();
    
    private boolean mIsRecoveryComplete;
    
    public int mBatchRecoverySize = 50;
    
    /* Time in seconds that each thread will wait in case of DB unavailability/connection problems. Default value is
     * 15 seconds. This value is overridden by System Property 
     */
    private int mRetryIntInSec = 15;
    private final Object mRecoveryLock = new Object();
    private boolean recoveryInProgress = false;
    private BPELMemoryMonitor mMemoryMonitor;
    private DBConnectionFactory mDBConnectionFactory;
    private final Object mRecoverDanglingInstanceLock = new Object();
    private boolean mRecoverDanglingInstInProgress = false;
    private String BPELSE_ID = "bpelse1";
    private CustomClassLoaderUtil classLoaderContext;

    //maintains SU state for given su
    Map suStateMap = Collections.synchronizedMap(new HashMap<String, SUStates>());

    /**
     * Creates a new instance of Engine
     * 
     * @param mProperties properties
     * @param initialContext initial context
     */
    public EngineImpl(Properties properties, InitialContext initialContext) {
		this.mProperties = properties;
		this.mInitialContext = initialContext;
		mId = BPELSE_ID;

		if (mProperties.getProperty(ConnectionProperties.PERSISTENCEENABLED).equalsIgnoreCase(TRUE)) {
			configureRecovery();
			configureScalability();
			mIsClustered = isComponentInstalledInCluster();
			if (mIsClustered) {
				// configure to run in GlassFish cluster
				configureCluster1();
			} else {
				mIsClustered = isComponentConfiguredToRunInCluster();
				if (mIsClustered) {
					// configure to run in BPEL-SE cluster on independent GF
					// nodes
					configureCluster2();
				}
			}
		}
		setTransformEngine(mProperties.getProperty(Engine.TRANSFORM_ENGINE));
		classLoaderContext = new CustomClassLoaderUtil(getClass().getClassLoader());
		((BPELXPathContextFactory) Utility.JXPATH_FACTORY).setClassLoaderContext(classLoaderContext);
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getInterpreter()
     */
    public BPELInterpreter getInterpreter() {
        return mInterpreter;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#process()
     */
    public void process() {
        Collection processMgrs = mBPELProcessManagerMap.values();

        // Call the cleanup method here.
        BPELProcessManager bpMgr = null;
        Iterator itr = processMgrs.iterator();
        while (itr.hasNext() && (getState() == Engine.RUNNING)) {
            bpMgr = (BPELProcessManager) itr.next();
            bpMgr.purgeWaitingRequests();
        }

        if (mPersistEnabled) {

            if (!mDebugEnabled && mMemoryMonitor.isScalabilityEnabled() && !mMemoryMonitor.isBelowUpperMemoryThreshold()) {
                boolean isSuccessful = doMemoryMangement(false);
                if (!isSuccessful) {
                    /*
                     * This indicate that either memory sweep is in progress or memory has fallen below
                     * the defined levels. We are still continuing further, is this good idea?
                     */
                }
            }

            if (mIsClustered) {
                doClusteringTasks(processMgrs);
            } else {
                doRecoveryNonCluster();
            }
        }

        itr = processMgrs.iterator();
        while (itr.hasNext() && (getState() == Engine.RUNNING)) {
            bpMgr = (BPELProcessManager) itr.next();
            bpMgr.process();
        }
    }

    private void synchronizeMonitorStatus() {
		MonitorManager.synchronizeBPStatus (this);
	}
    /**
     * If the free heap space has fallen below the certain limit, as part of phase 1 solution
     * the long running process instance variable will be dereferenced and memory be made available. 
     * If the Phase-1 is not successful in bringing the memory down to defined threshold levels
     * and if Phase-2 Solution (if enabled) will kick in. This will passivate the idle instances.
     * 
     * @param idDuringRecovery to indicate that the call is during recovery
     * @return true indicates that either the memory sweep is in progress or
     * 				the memory could not be brought down to acceptable levels. 
     */
    private boolean doMemoryMangement(boolean isDuringRecovery) {
		boolean isSuccessful = false;

		synchronized (mMemorySweepLock) {
			if (mScalabilityInProgress) {
				return false;
			} else {
				mScalabilityInProgress = true;
			}
		}
		
		if (mMemoryMonitor.isPhase1Enabled()) {
			isSuccessful = doFreeMemoryForPhase1(mMemoryMonitor.getIdleThresholdStart());
		}
		
		if (!isDuringRecovery 
				&& !isSuccessful 
				&& mMemoryMonitor.isPhase2Enabled() 
				&& !mMemoryMonitor.isBelowUpperMemoryThreshold()) {
			
			/* This indicates that Phase 1 work for freeing memory did not free
			 * up enough memory as defined by ph1LowerFreeMemory ratio,
			 * hence the memory has reached critical levels.
			 * Phase 2 solution will be called.
			 */

			isSuccessful = doFreeMemoryForPhase2(mMemoryMonitor.getIdleThresholdStart());
			if (!isSuccessful) {
				/* if still unsuccessful, we should call for throttling here.
				 * The idea is that somehow the engine is not able to bring the memory
				 * down, hence we should not accept any new messages. 
				 * Note, however that engine engine still need to accept 
				 * the response/status messages or otherwise the in-memory/scalability 
				 * passivated instances will not complete.
				 */ 
			}
		}
		mScalabilityInProgress = false;
		return isSuccessful;
	}
    

    /**
	 * For Phase 1 of the Free memory call, iterate though all the in-memory
	 * instances that are not under active execution, because of wait/pending
	 * response/ pending status/or pending correlating receive and dereference
	 * the variables hence the memory associated with these variables will be
	 * reclaimed by GC.
	 * 
	 * The return flag indicates successful outcome of this call. i.e the
	 * scalability task was able to free enough memory as defined by
	 * ph1UpperFreeMemory (see BPELMemoryMonitorImpl)
	 * 
	 * @param memRelTimeCriterion
	 * @return boolean
	 */
    private boolean doFreeMemoryForPhase1(long memRelTimeCriterion) {

        Collection processMgrs = mBPELProcessManagerMap.values();

        BPELProcessManager bpMgr = null;

        Iterator processIter = processMgrs.iterator();
        while (processIter.hasNext()) {
            bpMgr = (BPELProcessManager) processIter.next();
            bpMgr.doPhase1ScalabilitySolution(memRelTimeCriterion);
        }

        /* the idea here is that if the required free memory is still not 
         * available keep recursing till we get rid of the variables for 
         * all the waiting/pending instances.
         * Successive recursion will reduce the identifying wait time 
         * criterion by half from previous run.
         * The recursion will continue till the lower limit specified
         * for identifying the instances.
         */
        
        if (!mMemoryMonitor.isBelowUpperMemoryThreshold()) {
            if (memRelTimeCriterion > mMemoryMonitor.getIdleThresholdEnd()) {
                memRelTimeCriterion = memRelTimeCriterion / 2;
                return doFreeMemoryForPhase1(memRelTimeCriterion);
            } else {
                return false;
            }
        }
        
        return true;
    }

    private boolean doFreeMemoryForPhase2(long instPassivationTimeCriterion) {
        Collection processMgrs = mBPELProcessManagerMap.values();

        BPELProcessManager bpMgr = null;

        Iterator processIter = processMgrs.iterator();
        while (processIter.hasNext()) {
            bpMgr = (BPELProcessManager) processIter.next();
            /*
             * Currently we do not support Instance passivation for business 
             * processes defined with Event Handlers.
             */
            if (!bpMgr.getBPELProcess().isEventHandlersDefined()) {
            	bpMgr.doPhase2ScalabilitySolution(instPassivationTimeCriterion);
            }
        }

        /*
         * The idea here is that if the required free memory is still not available
         * keep recursing till we get rid of the variables for all the waiting/pending instances.
         * Successive recursion will reduce the identifying wait time criterion by half from previous run.
         */ 
        if (!mMemoryMonitor.isBelowUpperMemoryThreshold()) {
            if (instPassivationTimeCriterion > mMemoryMonitor.getIdleThresholdEnd()) {
                instPassivationTimeCriterion = instPassivationTimeCriterion / 2;
                return doFreeMemoryForPhase2(instPassivationTimeCriterion);
            } else {
                return false;
            }
        }
        return true;
    }
    
	/**
     * Perform clustering tasks
     * @param processMgrs
     */
    private void doClusteringTasks(Collection processMgrs) {
        
        boolean returnFlag = mEngStateMgr.updateHeartbeat(mHeartbeatUpdateConfigTime);
        if (returnFlag) {
            handleSpecialCases(processMgrs);
        }
        
        doInstanceFailover(processMgrs);
    }
    
    /**
     * Check for the failed engine and acquire the dangling instances thereof.
     * 
     * @param processMgrs
     */
    private void doInstanceFailover(Collection processMgrs) {
        synchronized (mRecoverDanglingInstanceLock) {
            if (mRecoverDanglingInstInProgress) {
                return;
            }
            mRecoverDanglingInstInProgress = true;
        }

        try {
            recoverDanglingInstances(processMgrs);
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6024: Failed During Recovery"), ex);
        } finally {
            synchronized (mRecoverDanglingInstanceLock) {
                mRecoverDanglingInstInProgress = false;
            }
        }
    }
    
    /**
     * Check for dangling instances of failed engine and if any,
     * Recover them.
     * @param processMgrs
     */
    private void recoverDanglingInstances(Collection processMgrs) throws Exception {

        // TODO FIXME 
        // For some reason, processMgrs although passed in, is not being used. We could use this 
        // list to find out only those instances that we need to failover OR leave them out as in 99% cases, failover means
        // both engine will have the same set of processes anyway. One case it may show as issue is when the entire cluster 
        // of two nodes is down and while coming back up, one engine tries to pull the records of the other engine before
        // all the deployments happen. Again this is not a show stopper issue.
        // TODO FIXME

        int updateCount = mEngStateMgr.updateDanglingInstances(mHeartbeatUpdateConfigTime, mBatchRecoverySize);
        boolean flag;
        synchronized (newDeploymentLock) {
            flag = updateCount > 0 || mNewAddedDep;
            if (updateCount > 0) {
                /*
                 * It is very hard to write junit for regression testing of this feature
                 * (failover scalability). Created one test case testFailoverScalabilityTestCase3
                 * in system tests in class SimpleInvokeFailoverTest. This test would create
                 * 4 engines and feed 10 messages each. One of the engine (engine1) would be crashed and
                 * live engines would failover the instances of the failed engine. The recovery batch
                 * size is set to 3, hence the failover should happen in batches, total four batches
                 * of sizes 3, 3, 3 and 1. These batch could be picked by one or more of remaining live engines (3).
                 * The test verifies that all the  40 instances are recovered and completed at the end of the test.
                 * Also the junit will print the final instance count for each engine, it could read something like
                 * engine1=0, engine2=14, engine3=13, engine3=13 (the engine1 is crashed, engine2=14 represents
                 * that engine2 took over 4 instances of engine1 and so on).
                 * To verify the proper functioning of this, un-comment the following Log and the thread sleep
                 * and verify the output log for updateCount values and the engines thereof.
                 */

                if (LOGGER.isLoggable(Level.FINE)) {
                    LOGGER.log(Level.FINE, I18n.loc("BPCOR-3007: Recovery Called : Found {0} dangling instances " +
                            "for recovery/failover ..this engine id : {1}", updateCount, mId));
                }
            }

            if (mNewAddedDep) {
                LOGGER.log(Level.CONFIG, I18n.loc("BPCOR-4002: Recovery Called : On account of new service " +
                        "unit deployment to recover dangling instances (if any) for the new deployed " +
                        "process ..this engine id : {0}", mId));
            }
            mNewAddedDep = false;
        }
        if (flag) {
            // the false for the returned flag indicates that recovery of already acquired
            // instances was incomplete due to memory limitations.
            mIsRecoveryComplete = recover();
        }

        if (!mIsRecoveryComplete) {
            // since this engine is running low on memory, we do not want to
            // acquire more instances for failover, hence returning here.
            // when the next call for heart-beat update is made, we can try to recover
            // the instances of failed engine, assuming, they are not already acquired by
            // other live engine(s) in the cluster.

            return;
        }
        if (updateCount == mBatchRecoverySize) {
            // The call to acquire instances of failed engine resulted
            // in acquiring of BATCH_RECOVERY_SIZE instances (which is upper limit of how
            // many a live engine should acquire). This suggests that there may be other
            // instances that are available (of failed engine(s)) that need to be
            // failed over (acquired and recovered), hence making a recursive call.
            recoverDanglingInstances(processMgrs);
        }
    }
    
    /**
     * Special Cases: 
     * A. Handle Out of Order Correlated Events. 
     * B. Handle correlated messaging events that end up on different engine than the one 
     * that created the instance and also happen to arrive before the instance is 
     * persisted/passivated by the engine creating the instance. 
     * C. Handle two way invokes to sub bp defined with correlation and hence the possibility that the
     * sub bp instance might be passivated and further executed on different engine because the
     * correlated message (for sub bp) might arrive on different engine then the one that created
     * the instance (sub bp). For clustered case only in case of sub bp (defined with correlation)
     * the messages exchange will be lost in such case when the instance (sub bp) is passivated. We
     * rely on CRMP mechanism to periodically check the database for responses for such two way
     * invokes. 
     * 
     * Design for A and B 
     * 1. recurse through all Business Process Managers and get
     * events waiting on correlations, if yes 
     * 2. query the database to get such instances (only the
     * events are ready to be consumed based on the IMA type) for such correlation id, if yes 
     * 3. send them for recovery 
     * 
     * TODO events which are ready to be consumed need to be removed from the
     * correlated waiting events map at process manager. The activate instances api should also
     * return list of events which found the instances. 
     * 
     * Design for C: 
     * As soon as a two way invoke is
     * made and the response not found, as special BPIT is created and the crmpid and frame is saved
     * on it. This failover thread would check to see if response object exists in the database. If
     * yes, the response is constructed directly from the database and the saved callframe is
     * scheduled for further execution
     * 
     * @param processMgrs
     */
    private void handleSpecialCases(Collection processMgrs) {
        BPELProcessManager bpMgr = null;
        
        Iterator processIter = processMgrs.iterator();
        while (processIter.hasNext()) {
            bpMgr = (BPELProcessManager) processIter.next();

            List eventsList = bpMgr.getCorrelatedWaitingEvents();
            if (eventsList.size() > 0) {
                List toRecoverlist = mStateMgr.activateInstancesAndUpdateOwnership(mId, eventsList);

                if (toRecoverlist.size() > 0) {
                    try {
                    	if (LOGGER.isLoggable(Level.FINE)) {
                    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3008: Engine id : {0} : Recovering activated and " + 
                    				"newly owned instances: {1}", mId, String.valueOf(toRecoverlist)));
                    	}
                        bpMgr.recover(toRecoverlist);
                    } catch (Exception ex) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6022: Failed During Recovery of correlated " + 
                        		"instance in multiple engine"), ex);
                    }
                }
            }
            if( bpMgr.isPersistenceEnabled() && isClustered() ){
                    bpMgr.getResponsesForClusteredSubBPInvokes();
            }
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getNextScheduledTime()
     */
    public long getNextScheduledTime() {
        long leastBPITTimeout = 0;
        Collection processMgrs = mBPELProcessManagerMap.values();
        Iterator itr = processMgrs.iterator();

        while (itr.hasNext()) {
            BPELProcessManager bpMgr = (BPELProcessManager) itr.next();
            long bpitTimeout = bpMgr.getNextScheduledTime();
            if ((bpitTimeout > 0)
                    && ((leastBPITTimeout == 0) || (bpitTimeout < leastBPITTimeout))) {
                leastBPITTimeout = bpitTimeout;
            }
        }

        if (mPersistEnabled && mIsClustered) {
            long engineAliveUpdateTime = 0;
            long elapsedTimeSinceLastUpdate = System.currentTimeMillis()
                    - mEngStateMgr.getLastHeartbeatUpdateTime();
            if (elapsedTimeSinceLastUpdate > mHeartbeatUpdateConfigTime) {
                // this means that the elapsed time exceed the heatbeat time.
                // update immediately, return a very small value. Note, you
                // cannot return 0
                // as this will cause the in out thread to block on the channel
                // for message
                engineAliveUpdateTime = System.currentTimeMillis();
            } else {
                engineAliveUpdateTime = System.currentTimeMillis()
                        + (6 * mHeartbeatUpdateConfigTime) / 10;
            }

            // if the engineAliveUpdateTime is less than the least bpeit timout,
            // return it.
            if (leastBPITTimeout == 0
                    || (engineAliveUpdateTime < leastBPITTimeout)) {
                return engineAliveUpdateTime;
            }
        }
        return leastBPITTimeout;
    }

    /**
     * @see Engine#setOutChannel(com.sun.jbi.engine.bpel.core.bpel.engine.Channel)
     */
    public void setOutChannel(Channel channel) {
        mChannel = channel;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#addModel(String,
     *      com.sun.jbi.engine.bpel.core.bpms.bpel.model.BPELProcess)
     */
    public void addModel(RBPELProcess bpelProcess, String saName, String suName) {
        synchronized (newDeploymentLock) {
            // String id = bpelProcess.getBPELId();
            QName id = bpelProcess.getBPELId();
            if (mBPELProcessIDMap.get(id) != null) {
                throw new RuntimeException(I18n.loc("BPCOR-6025: Business processes with duplicate id(QName) " + 
                		"are not allowed in an engine. Business Process {0} id already registered", id.toString())); //$NON-NLS-1$
            }

            mBPELProcessIDMap.put(id, bpelProcess);

            BPELProcessManager processManager = new BPELProcessManagerImpl(bpelProcess, this, saName, suName);
            Object retObj = mBPELProcessManagerMap.put(bpelProcess, processManager);
            if (mBPELProcessEventsConfigMap != null) {
            	ProcessEventsConfig config = mBPELProcessEventsConfigMap.get(bpelProcess.getBPELId());
            	if (config != null) {
            		processManager.setGenerateEventsFlag(config.getProcessEventsFlag());
            		// This is to auto clean up the events flag map. This contents of the data structure
            		// are not relevant after the bpel models are loaded.
            		mBPELProcessEventsConfigMap.remove(bpelProcess.getBPELId());
            		if (mBPELProcessEventsConfigMap.isEmpty()) {
            			mBPELProcessEventsConfigMap = null;
            		}
            	}
            }
            if (retObj != null) {
                throw new RuntimeException(
                        "Fatal Error: this process is already loaded process Id: "
                                + bpelProcess.getBPELId().toString());
            }
            if (mDebugEnabled && getDebugger() != null) {
                ((DefaultDebugger) getDebugger()).processAdded(bpelProcess);
            }
        }
    }

    /**
     * This overloaded addModel() is used only by the BPELDebugger Engine implementation for junit
     * test case requirements. Ideally this has to be removed when the test cases are re-factored.
     * 
     * @param id location of the BPEL file.
     * @param bpelProcess runtime bpel process
     * @param processManager runtime manager for the bpel process.
     */
    public void addModel(RBPELProcess bpelProcess, BPELProcessManager processManager) {
        synchronized (newDeploymentLock) {
            // String id = bpelProcess.getBPELId();
            QName id = bpelProcess.getBPELId();
            if (mBPELProcessIDMap.get(id) != null) {
                throw new RuntimeException(I18n.loc("BPCOR-6025: Business processes with duplicate id(QName) " + 
                		"are not allowed in an engine. Business Process {0} id already registered")); //$NON-NLS-1$
            }

            mBPELProcessIDMap.put(id, bpelProcess);
            Object retObj = mBPELProcessManagerMap.put(bpelProcess, processManager);
            if (retObj != null) {
                throw new RuntimeException(
                        "Fatal Error: this process is already loaded process Id: "
                                + bpelProcess.getBPELId().toString());
            }
            mBPELProcessManagerMap.put(bpelProcess, processManager);
            mNewAddedDep = true;
            if (mDebugEnabled && getDebugger() != null) {
                ((DefaultDebugger) getDebugger()).processAdded(bpelProcess);
            }

        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#removeModel(java.lang.String,
     *      com.sun.jbi.engine.bpel.core.bpms.bpel.model.BPELProcess)
     */
    // public void removeModel(String id) {
    public void removeModel(QName id) {
        if (mBPELProcessIDMap.get(id) == null) {
            throw new RuntimeException(
            		I18n.loc("BPCOR-6026: Trying to unregister a process that has never been registered"));
        }

        RBPELProcess bpelProcess = (RBPELProcess) mBPELProcessIDMap.remove(id);
        mBPELProcessManagerMap.remove(bpelProcess);
        if (mDebugEnabled && getDebugger() != null) {
            ((DefaultDebugger) getDebugger()).processRemoved(bpelProcess);
        }
    }

    /**
     * @see Engine#addStartActivityModel(com.sun.jbi.engine.bpel.core.bpms.bpel.model.BPELProcess,
     *      com.sun.jbi.engine.bpel.core.bpms.common.model.bpel.runtime.RStartElement,
     *      java.lang.String)
     */
    public void addStartActivityModel(RBPELProcess bpelProcess, RStartElement sa, String operPattern) {
        BPELProcessManager procMgr = (BPELProcessManager) mBPELProcessManagerMap.get(bpelProcess);
        procMgr.addStartActivityModel(sa, operPattern);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#isDebugEnabled()
     */
    public boolean isDebugEnabled() {
        return mDebugEnabled;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setDebugEnabled(boolean)
     */
    public void setDebugEnabled(boolean debugEnabled) {
        mDebugEnabled = debugEnabled;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getDebugPort()
     */
    public String getDebugPort() {
        return mDebugPort;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setDebugPort(java.lang.String)
     */
    public void setDebugPort(String debugPort) {
        mDebugPort = debugPort;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getTransformEngine() */
	public TransformEngine getTransformEngine() {
		return mTransformEngine;
	}

	/** @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setTransformEngine(com.sun.jbi.engine.bpel.BPELSERuntimeConfigurationMBean.TransformEngine) */
	public void setTransformEngine(TransformEngine transformEngine) {
		mTransformEngine = transformEngine;
		((BPELXPathContextFactory) Utility.JXPATH_FACTORY).setTransformEngine(mTransformEngine);
	}

	private void setTransformEngine(String engine) {
		if (engine == null) {
			setTransformEngine(TransformEngine.XSLT_1_0);
		}
		else {
			try {
				setTransformEngine(TransformEngine.valueOf(engine));
			}
			catch (Exception e) {
				setTransformEngine(TransformEngine.XSLT_1_0);
			}
		}
	}
	
	/**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#resetProperties(java.util.Properties)
     */
    public void resetProperties(Properties properties) {
        Enumeration keys = properties.keys();
        String key = null;
        String val = null;

        while (keys.hasMoreElements()) {
            key = (String) keys.nextElement();
            val = properties.getProperty(key);

            if (key.equals(Engine.DEBUG_ENABLED)) {
                if (val.equals("true")) {
                    mDebugEnabled = true;
                    enableDebugger();
                } else {
                    mDebugEnabled = false;
                    disableDebugger();
                }
            } else if (key.equals(Engine.DEBUG_PORT)) {
                mDebugPort = val;
            } else if (key.equals(Engine.WAITING_REQUEST_LIFE_SPAN)) {
            	mWaitingRequestLifeSpan = Long.parseLong(val);
            } else if (key.equals(Engine.MONITOR_ENABLED)) {
                if (val.equalsIgnoreCase(TRUE) && !mMonitorEnabled) {
                    mProperties.put(key, val);
                    DBConnectionFactory monitorDBfactory = setupMonitorDB (mProperties);
                    EventProcessHelper eventHelper = new EventProcessHelper (this, mChannel);  
                    if (monitorDBfactory != null) {
                    	eventHelper.setDBFactory(monitorDBfactory);
                    }  
                      
                    mEventManager.init(eventHelper);
                    mEventManager.resetProperties(mProperties);
                    mMonitorEnabled = true;
                    synchronizeMonitorStatus();
                    mBPELProcessEventsConfigMap = 
                    	EventsConfigurationHelper.retrieveEventsGenerationFlag(monitorDBfactory);
                    if (mBPELProcessEventsConfigMap != null) {
                    	EventsConfigurationHelper.syncEventsGenerationFlag(this, mBPELProcessEventsConfigMap.values());
                    	// Map no longer needed so is set to null
                    	mBPELProcessEventsConfigMap = null;
                    }
                } else if (mMonitorEnabled && ! val.equalsIgnoreCase(TRUE)) {
                    mEventManager.resetProperties(mProperties);        
                    mMonitorEnabled = false;
                }
            } else if (key.equals(Engine.MONITOR_VARIABLE_ENABLED)) {
                if (val.equalsIgnoreCase(TRUE) && !mVariableMonitorEnabled) {
                    mProperties.put(key, val);
                    mVariableMonitorEnabled = true;
                } else if (mVariableMonitorEnabled
                        && !val.equalsIgnoreCase(TRUE)) {
                    mProperties.put(key, val);
                    mVariableMonitorEnabled = false;
                }
            } else if (key.equals(Engine.KPI_ENABLED)) {
                if (val.equalsIgnoreCase(TRUE) && !mKPIEnabled ) {
//                    for (BPELEventListener eventListener : mEventListeners) {
//                    	eventListener.resetProperties(mProperties);        
//                    }
                    mProperties.put(key, val);
                    mEventManager.resetProperties(mProperties);        
                    mKPIEnabled = true;
                }  else if (mKPIEnabled && ! val.equalsIgnoreCase(TRUE)) {
//                    for (BPELEventListener eventListener : mEventListeners) {
//                    	eventListener.resetProperties(mProperties);        
//                    }       
                    mProperties.put(key, val);
                    mEventManager.resetProperties(mProperties);   
                    mKPIEnabled = false;
                }
            }
            else if (key.equals(Engine.TRANSFORM_ENGINE)) {
            	setTransformEngine(TransformEngine.valueOf(val));
            }
            else if (key.equals(Engine.VALIDATION_ENABLED)) {
                setValidationEnabled(val.equalsIgnoreCase(TRUE));
            }
        }
    }

    /**
     * The following are the persisted related implementation.
     * 
     * @param pcId program counter ID
     * @return long branch ID
     */

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getCleanUpDelayTime()
     */
    public Long getCleanUpDelayTime() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getCleanUpMode()
     */
    public String getCleanUpMode() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getConnectionTimeOut()
     */
    public long getConnectionTimeOut() {
        // TODO Auto-generated method stub
        return 0;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getConnPoolsize()
     */
    public int getConnPoolsize() {
        // TODO Auto-generated method stub
        return 0;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getExpiration()
     */
    public Long getExpiration() {
        // TODO Auto-generated method stub.
        // TODO To be removed.
        return new Long(10000);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getId()
     */
    public String getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getLocation()
     */
    public String getLocation() {
        // return MESSAGES.getString("EngineImpl_6"); //$NON-NLS-1$

        if (mLocation == null) {
            try {
                mLocation = InetAddress.getLocalHost().getHostName();
            } catch (UnknownHostException e) {
                // e.printStackTrace();
                mLocation = I18n.loc("BPCOR-3009: some location"); //$NON-NLS-1$
            }
        }
        return mLocation;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getMaxConnPoolSize()
     */
    public int getMaxConnPoolSize() {
        // TODO Auto-generated method stub
        return 0;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getStateManager()
     */
    public StateManager getStateManager() {
        return mStateMgr;
    }

    /**
     * gets engine state manager
     * 
     * @return Returns the mEngStateMgr.
     */
    public EngineStateManager getEngStateMgr() {
        return mEngStateMgr;
    }

    /**
     * sets engine state manager
     * 
     * @param engStateMgr The mEngStateMgr to set.
     */
    public void setEngStateMgr(EngineStateManager engStateMgr) {
        mEngStateMgr = engStateMgr;
    }

    /**
     * check if persistence is enabled
     * 
     * @return Returns the mPersistEnabled.
     */
    public boolean isPersistenceEnabled() {
        return mPersistEnabled;
    }

    /**
     * sets persistence enabling flag
     * 
     * @param persistEnabled The mPersistEnabled to set.
     */
    public void enablePersistence(boolean persistEnabled) {
        mPersistEnabled = persistEnabled;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#isReliabilityEnabled()
     */
    public boolean isReliabilityEnabled() {
        return mReliabilityEnabled;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setReliabilityEnabled(boolean)
     */
    public void setReliabilityEnabled(boolean reliability) {
        mReliabilityEnabled = reliability;
    }
    
    /* 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getWaitingRequestLifeSpan()
	 */
	public long getWaitingRequestLifeSpan() {
		return mWaitingRequestLifeSpan;
	}

	/* 
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setWaitingRequestLifeSpan(long)
	 */
	public void setWaitingRequestLifeSpan(long waitingRequestLifeSpan) {
		mWaitingRequestLifeSpan = waitingRequestLifeSpan;
	}

	public long getEngineExpiryInterval() {
        return mHeartbeatUpdateConfigTime;
    }

    public void doRecoveryNonCluster() {
        // If the recovery was not completed in the last attempt (due to memory limitations)
        // during the forward execution, the recovery will get called again. To free up threads
        // to work in forward path, using single thread for identifying the recoverable
        // instances and constructing callframes for those for recovery.
        synchronized (mRecoveryLock) {
            if (recoveryInProgress) {
                return;
            }
            recoveryInProgress = true;
        }

        boolean newDepflag = false;
        synchronized (newDeploymentLock) {
            if (mNewAddedDep) {
                newDepflag = true;
                mNewAddedDep = false;
            }
        }
        try {
            if (newDepflag || !mIsRecoveryComplete) {
                LOGGER.log(Level.CONFIG, I18n.loc("BPCOR-4002: Recovery Called : On account of new service " +
                        "unit deployment to recover dangling instances (if any) for the new " +
                        "deployed process ..this engine id : {0}", mId));

                mIsRecoveryComplete = recover();
            }
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6024: Failed During Recovery"), ex);
        } finally {
            synchronized (mRecoveryLock) {
                recoveryInProgress = false;
            }
        }
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#recover()
     */
    public boolean recover() throws Exception {
        boolean isEnoughMemoryAvailable = false;

        if (mMemoryMonitor.isScalabilityEnabled() && !mMemoryMonitor.isBelowUpperMemoryThreshold()) {
            isEnoughMemoryAvailable = doMemoryMangement(true);
        } else {
            isEnoughMemoryAvailable = true;
        }

        boolean returnFlag;

        if (isEnoughMemoryAvailable) {
            // Get all the running (Status : RUNNING or Status : Suspended) instances, which are not
            // Passivated (Ownerlock : Y)
            Map[] maps = mEngStateMgr.getRunningAndSuspendedStates();
            Map runningInstMap = maps[0];
            Map suspendedInstMap = maps[1];

            if (isClustered()) {
                Map expiredOnAlarmInstancesMap = mEngStateMgr.getExpiredOnAlarmInstances();
                if (expiredOnAlarmInstancesMap.size() > 0) {
                    runningInstMap.putAll(expiredOnAlarmInstancesMap);
                }
            }
            /*
             * SerialProcessing: when the engine is in maxInstances/throttle
             * mode all the registered bpelProcess managers that do not have
             * instances to recover have to be marked by their flag of 
             * recoveryComplete indicating recovery is complete, so that
             * inbound events can be processed upon arrival.
             */
            setRecoveryFlagOnProcessManager(runningInstMap, suspendedInstMap);

            boolean isRecoveryComplete = recoverInstances(runningInstMap);
            if (isRecoveryComplete) {
                isRecoveryComplete = recoverInstances(suspendedInstMap);
            }
            returnFlag = isRecoveryComplete;
        } else {
            returnFlag = false;
        }
        return returnFlag;
    }
    
    /*
     * For the bpel processes that are not in the maps(ie, those processes
     * that do not have instances to recover) set the recoveryComplete flag to true.
     * These BpelProcessManager instances need to start processing inbound events 
     * immediately. 
     * TODO: This method will initially get called by all the BPELSEInOutThread instances
     * and hence could be wasting CPU cycles. Will have to iron this out with the 
     * batch recovery implementation to avoid this.
     */
    private void setRecoveryFlagOnProcessManager(Map runningInstanceMap, Map suspendedInstancesMap ) {
        Collection runningProcessIds = runningInstanceMap.keySet();
        Collection suspendedProcessIds = suspendedInstancesMap.keySet();
        Set allIds = new HashSet(runningProcessIds);
        allIds.addAll(suspendedProcessIds); // effectively UNION of the two sets.
        QName[] bpelProcessIds = (QName[]) allIds.toArray(new QName[] {});

        Iterator iter = mBPELProcessManagerMap.values().iterator();
        
        while (iter.hasNext()) {
        	BPELProcessManager processMgr = (BPELProcessManager) iter.next();
        	QName processQName = null;
        	RBPELProcess process = null;
        	boolean dontSetFlag = false;
        	for(int i = 0; i < bpelProcessIds.length; i++) {
        		processQName = bpelProcessIds[i];
        		process = (RBPELProcess) mBPELProcessIDMap.get(processQName);
        		if (processMgr.getBPELProcess().equals(process)) {
        			dontSetFlag = true;
        			break;
        		}
        	}
        	if (!dontSetFlag) {
        		processMgr.setRecoveryComplete();
        	}
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#recover()
     */
    private boolean recoverInstances(Map instancesMap) throws Exception {
        
        if (LOGGER.isLoggable(Level.FINE)) {
	        LOGGER.fine(I18n.loc("BPCOR-3010: Engine id : {0} in memory instances : {1}", 
	        		mId, String.valueOf(instancesMap)));
        }

        boolean isRecoveryComplete = true;

        QName[] bpelProcessIds = (QName[]) instancesMap.keySet().toArray(new QName[] {});
        RBPELProcess bpelProcess = null;
        QName bpelProcessId = null;
        
        for (int i = 0, size = bpelProcessIds.length; i < size; i++) {
            try {
                List recoverInstances = new ArrayList();
                bpelProcessId = bpelProcessIds[i];
                bpelProcess = (RBPELProcess) mBPELProcessIDMap.get(bpelProcessId);

                if (bpelProcess == null) {
                    LOGGER.log(Level.INFO, I18n.loc("BPCOR-5015: BP instance(s) for {0} are not recoverd as it is not deployed yet", bpelProcessId));
                    continue;
                }
                //get BPELProcessManager and check is it read to recover
                BPELProcessManager procMgr = (BPELProcessManager) mBPELProcessManagerMap.get(bpelProcess);

                if (!procMgr.isReadyToRecover()) {
                    if (LOGGER.isLoggable(Level.FINE)) {
                        LOGGER.log(Level.FINE, I18n.loc("BP instance(s) for {0} are not recoverd as its SU {1} is not started yet",
                                bpelProcessId, procMgr.getServiceUnitName()));
                    }
                    continue;
                }

                recoverInstances.addAll((List) instancesMap.get(bpelProcessId));
                // Remove those instances which are already in memory
                List bpInstInMemory = new ArrayList();
                List bpInst = procMgr.getInstances();
                synchronized (bpInst) {
                	bpInstInMemory.addAll(bpInst);
                }
                recoverInstances.removeAll(bpInstInMemory);

                if (LOGGER.isLoggable(Level.FINE)) {
                    Iterator iter = recoverInstances.iterator();
                    while (iter.hasNext()) {
                    	String instanceId = (String) iter.next();
                    	LOGGER.fine(I18n.loc("BPCOR-3012: Engine id : {0} Found Instance for recovery " + 
                    			"with instance id : {1}", mId, instanceId));
                    }
                }
                // should the recovery in batches be only enabled with scalability solution on,
                // or always
                if (mMemoryMonitor.isScalabilityEnabled() && recoverInstances.size() > mBatchRecoverySize) {
                    isRecoveryComplete = doBatchRecovery(procMgr, recoverInstances);
                } else {
                    procMgr.recover(recoverInstances);
                }

            } catch (RuntimeException e) {
                LOGGER.log(Level.WARNING, 
                		I18n.loc("BPCOR-6029: Exception occured while recovering instances of BPEL Process: {0}", 
                				bpelProcessId.toString()), e);
            }
        }
        
        if (mIsClustered) {
            scheduleBPITForNonExpiredOnAlarms();
        }
        
        return isRecoveryComplete;
    }

    private boolean doBatchRecovery(BPELProcessManager procMgr, List bpInstancesForRecovery) throws Exception {

        Object[] instancesArray = bpInstancesForRecovery.toArray();
        int instanceSize = instancesArray.length;
        int counter = 0;
        List instancesBatch = null;
        int remaining;
        int recordsTocopy;
        
        while (instanceSize > counter) {
            if (mMemoryMonitor.isBelowUpperMemoryThreshold()) {
                instancesBatch = new ArrayList();
                remaining = instanceSize - counter;
                recordsTocopy = mBatchRecoverySize < remaining ? mBatchRecoverySize : remaining;
                for (int i = 0; i < recordsTocopy; i++) {
                    instancesBatch.add(instancesArray[counter]);
                    counter++;
                }
                procMgr.recover(instancesBatch);
            } else {
                boolean isSuccessful = doMemoryMangement(true); 
                if (!isSuccessful) {
                    // this indicates that either memory sweep is in progress or enough free memory is not available,
                    // hence we do not want to schedule more instances for recovery. The recovery will be marked
                	// as incomplete and will be attempted again some other time.
                	// NOTE: Phase 2 scalability is not performed during recovery.
                    return false;
                }
            }
        }
        
        return true;
    }
    

    /**
     * Query the instances for pick for which onAlam is defined and has not expired yet.
     * Construct BusinessProcessInstanceThreadForClusteredPick for the remaining duration (of
     * alarm) and schedule on the ready to run queue. 
     * Do not acquire the ownerlock for such instances, as the onMessage
     * can happen on the the same or other engine (before the expiration of the on alarm).
     * Since the recovery can be called multiple times for cluster case, dont schedule BPIT for 
     * clustered pick for non expired on alarms which are already in memory.
     * 
     * @throws Exception
     */
    private void scheduleBPITForNonExpiredOnAlarms() throws Exception {
        Map nonExpiredOnAlarmInstancesMap = mEngStateMgr.getNonExpiredOnAlarmInstances();

        // Get list of in-memory special bpit for clustered pick and remove them
        // from the nonExpiredOnAlarmInstancesList, for the remaining, create special bpit
        // and schedule them on the ready to run queue
        String[] bpelProcessIds = (String[]) nonExpiredOnAlarmInstancesMap.keySet().toArray(
                new String[] {});
        String bpelProcessId = null;
        RBPELProcess bpelProcess = null;
        List nonExpiredOnAlarmInMemoryInstances = null;
        List nonExpiredOnAlarmDatabaseInstances = null;
        NonExpiredOnAlarmInstanceInfo instanceInfo = null;
        BusinessProcessInstanceThread bpitForClusteredPick = null;

        for (int i = 0, size = bpelProcessIds.length; i < size; i++) {
            bpelProcessId = bpelProcessIds[i];
            bpelProcess = (RBPELProcess) mBPELProcessIDMap.get(bpelProcessId);

            if (bpelProcess == null) {
                LOGGER.log(Level.INFO, I18n.loc("BPCOR-5015: BP instance(s) for {0} are not recoverd as it is not deployed yet", bpelProcessId));
                continue;
            }
            BPELProcessManager procMgr = (BPELProcessManager) mBPELProcessManagerMap.get(bpelProcess);

            // get the in memory instances for the business process
            nonExpiredOnAlarmInMemoryInstances = procMgr.getNonExpiredOnAlarmRunningInstances();

            // get the database instances for the business process
            nonExpiredOnAlarmDatabaseInstances = (List) nonExpiredOnAlarmInstancesMap.get(bpelProcessId);

            // remove the in memory from the ones found in the database.
            nonExpiredOnAlarmDatabaseInstances.removeAll(nonExpiredOnAlarmInMemoryInstances);

            // for the remaining instances, create bpit for clusteredpick and schedule it on
            // ready to run queue.
            for (int j = 0; j < nonExpiredOnAlarmDatabaseInstances.size(); j++) {
                instanceInfo = (NonExpiredOnAlarmInstanceInfo) nonExpiredOnAlarmDatabaseInstances.get(j);
                bpitForClusteredPick = new BusinessProcessInstanceThreadForClusteredPick(procMgr,
                        this, instanceInfo.getInstanceId(), instanceInfo.getWaitTime());
                procMgr.addNonExpiredOnAlarmRunningInstances(instanceInfo);
                procMgr.addToReadyToRunQueue(bpitForClusteredPick);
            }
        }
    }

    /**
     * End of the persisted related implementation.
     * 
     * @param event incoming event key
     * @param contents message content
     */
    public void process(InComingEventKeyImpl event,
            MessageContainer contents) {

        RBPELProcess process = event.getBPELProcess();
        BPELProcessManager procMgr = (BPELProcessManager) mBPELProcessManagerMap.get(process);
        
        procMgr.handleEvent(event, contents);
        process();
    }

    /**
     * gets channel
     * 
     * @return Channel channel
     */
    protected Channel getChannel() {
        return mChannel;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#preStop()
     */
    public void preStop() {
        synchronized (mStatusLock) {
            if (mState == STOPPED || mState == SHUTDOWN) {
                return;
            }
            mState = STOPPED;
            if (mDebugEnabled && getDebugger() != null) {
                getDebugger().detach();
            }
        }
        DBConnectionFactory.notifyBlockedThreads();
        if (mEventManager != null) {
            mEventManager.shutdown();
            mEventManager.forceShutdown();
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#preShutdown()
     */
    public void preShutdown() {
        synchronized (mStatusLock) {
            if (mState == SHUTDOWN) {
                return;
            }
            mState = SHUTDOWN;
            if (mDebugEnabled && getDebugger() != null) {
                getDebugger().detach();
            }
            try {
                disableDebugger();
            } catch (Exception e) {
                // Ignore the exception
                LOGGER.log(Level.WARNING, "Not able to disable debugger", e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#preStart()
     */
    public void preStart() {
    	
        synchronized (mStatusLock) {
            int stateBeforeStart = mState;
            
            try {
	            boolean isPersistenceEnabled = mProperties.getProperty(ConnectionProperties.PERSISTENCEENABLED).equalsIgnoreCase(TRUE) ? true : false;
	            
	            if (isPersistenceEnabled) {

            		if (!mIsClustered && mConnConfig != null) {
            			mConnConfig.createConnectionsPools();
            		}
	            	
	            	if (mDBConnectionFactory == null) {
	            		mDBConnectionFactory = new DBConnectionFactory(mProperties, mInitialContext, this);
	            	}
	            	
	            	if (checkConnectionIntegrity(mDBConnectionFactory)) {

                        STATUS statusSchema = PersistenceDBSchemaCreation.getInstance().checkTablesIntegrity(mDBConnectionFactory);

                        if (statusSchema == STATUS.CORRUPT) {
                            throw recoveryTablesCreationException();
                        }
                        if ((statusSchema == STATUS.EMPTY) && mIsClustered) {
                            throw recoveryTablesCreationException();
                        }
                        if (statusSchema == STATUS.EMPTY) {
                            createRecoveryTables();
                        }
                    } else {
                        throw new BPELConfigurationException("Database connection issue");
                    }

	            	mStateMgr = new StateManagerImpl(this, mDBConnectionFactory);
            		mEngStateMgr = new EngineStateManagerImpl(this, mDBConnectionFactory);
            		mPersistEnabled = true;
	            }
	            
	            String waitingRequestLifeSpan = mProperties.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN);
	            if (waitingRequestLifeSpan != null) {
	            	mWaitingRequestLifeSpan =
	            		Long.parseLong(mProperties.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN));
	            } else {
	            	mWaitingRequestLifeSpan = Engine.WAITING_REQUEST_LIFE_SPAN_FACTORYDEFAULT;
	            }

                String validationEnabled = mProperties.getProperty(Engine.VALIDATION_ENABLED, Engine.VALIDATION_ENABLED_FACCTORYDEFAULT.toString());
                mValidationEnabled = Boolean.parseBoolean(validationEnabled);

                mState = RUNNING;
                
                // TODO Need to find a better place to set the custom node pointer factory
                JXPathContextReferenceImpl.addNodePointerFactory(new BPELSEDOMPointerFactory());   
                
                configureDebugger();
                configureMonitor();
				
                // Engine has to be initialized before constructing an instance of interpreter
                mInterpreter = new BPELInterpreter(this);
                
            } catch (Exception e) {
                // Clean up the engine state
                mState = stateBeforeStart;
                mStateMgr = null;
                mEngStateMgr = null;
                mHeartbeatUpdateConfigTime = -1;
                mPersistEnabled = false;

                mDebugEnabled = false;
                mDebugPort = "0";
                mWaitingRequestLifeSpan = Engine.WAITING_REQUEST_LIFE_SPAN_FACTORYDEFAULT;

                mValidationEnabled = Engine.VALIDATION_ENABLED_FACCTORYDEFAULT;

                mInterpreter = null;

                if (e instanceof BPELConfigurationException) {
                	throw (BPELConfigurationException)e;
                }
                throw new RuntimeException(e);
            }
        }
    }

    private void createRecoveryTables() {
        PersistenceDBSchemaCreation.getInstance().createTables(mDBConnectionFactory);
        if (PersistenceDBSchemaCreation.getInstance().checkTablesIntegrity(mDBConnectionFactory) != STATUS.VALID) {
            throw recoveryTablesCreationException();
        }
    }

    private BPELConfigurationException recoveryTablesCreationException() {
        String datasourceNonXAJndiName = mProperties.getProperty(ConnectionProperties.DatabaseNonXAJNDIName);
        String datasourceXAJndiName = mProperties.getProperty(ConnectionProperties.DatabaseXAJNDIName);
        String msg = I18n.loc("BPCOR-7040: BPELSE Persistence Schema for JDBC Non-XA Resource {0} and/or JDBC XA Resource {1} "
                + " not Valid. Please verify that persistence schema exists and contain all the required tables.",
                datasourceNonXAJndiName, datasourceXAJndiName);
        return new BPELConfigurationException(msg);
    }

    private void createMonitoringTables() {
        MonitorDBSchemaCreation.getInstance().createTables(mDBConnectionFactory);
        if (MonitorDBSchemaCreation.getInstance().checkTablesIntegrity(mDBConnectionFactory) != STATUS.VALID) {
            throw monitoringTablesCreationException();
        }
    }

    private BPELConfigurationException monitoringTablesCreationException() {
        String datasourceNonXAJndiName = mProperties.getProperty(ConnectionProperties.DatabaseNonXAJNDIName);
        String datasourceXAJndiName = mProperties.getProperty(ConnectionProperties.DatabaseXAJNDIName);
        String msg = I18n.loc("BPCOR-7037: BPELSE Monitoring Schema for JDBC Non-XA Resource {0} and/or JDBC XA Resource {1} "
                + " not Valid. Please verify that monitoring schema exists and contain all the required tables.",
                datasourceNonXAJndiName, datasourceXAJndiName);
        return new BPELConfigurationException(msg);
    }
    
	/**
     * If the engine is stopped/shutdown and database is bounced, upon restart
     * the appserver may return the stale connection. The following will check 
     * for such connections, and if any, invalidate them. 
     * 
     * This api will verify both the Non XA and XA connections.
     * 
     * @param connFac
     * @return
     * @throws Exception
     */
    private boolean checkConnectionIntegrity(DBConnectionFactory connFac) {
    	try {
			// verify non-xa connections
			AbstractDBConnection dbConn = null;
			try {
				dbConn = connFac.createNonXAConnection();
				if (!connFac.validateNonXAConnection(dbConn)) {
					return false;
				}
			} finally {
				if (dbConn != null) {
					try {
						dbConn.close();
					} catch (SQLException connCloseErr) {
						LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6072: Exception thrown when closing a connection"), 
        						connCloseErr);
					}
				}
			}

			// verify xa connections
			dbConn = null;
			try {
				dbConn = connFac.createXAConnection();
				if (!connFac.validateXAConnection(dbConn)) {
					return false;
				}
			} finally {
				if (dbConn != null) {
					try {
						dbConn.close();
					} catch (SQLException connCloseErr) {
						LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6072: Exception thrown when closing a connection"), 
        						connCloseErr);
					}
				}
			}
		} catch (Exception e) {
			String possibleCause = "Database Server may be down"; 
			String message = I18n.loc("BPCOR-7034: Exception thrown while connecting to the persistence database. " +
					"\nException Details : {0}. \nPossible Cause : {1} ", e.getMessage(), possibleCause);
			LOGGER.log(Level.SEVERE, message, e);
			throw new BPELConfigurationException(message);
		}
		
		return true;
    }
     
	private void configureDebugger() {
        // Always read the debug port: CR6414510
        mDebugPort = mProperties.getProperty(Engine.DEBUG_PORT);
        if (mProperties.getProperty(Engine.DEBUG_ENABLED).equalsIgnoreCase(TRUE)) {
            mDebugEnabled = true;
            enableDebugger();
        }
    }
    
    private void configureMonitor() {
        //Check prop, if monitoring is enabled, create db connection factory for monitor
        //If not enabled, return null
        mEventManager = new BPELEventManagerImpl (20, true);
        DBConnectionFactory monitorDBfactory = setupMonitorDB (mProperties);                
        EventProcessHelper eventHelper = new EventProcessHelper (this, mChannel);
        if (monitorDBfactory != null) {
			eventHelper.setDBFactory(monitorDBfactory);
		}

		mEventManager.init(eventHelper);
		mEventManager.startQueue();
		MonitorManager.needSynch();
		if (mMonitorEnabled) {
			synchronizeMonitorStatus();
			// DEVNOTE VM: This method is called before the bpel process model are added to the engine. If the sequence 
			// of events are changed in the future, i.e. this method is called after the bpel models are loaded then
			// we will need to make changed to the addModel() method as well as explicitly call the bpel event
			// synchronization method as in resetProperties() method.
			mBPELProcessEventsConfigMap = EventsConfigurationHelper.retrieveEventsGenerationFlag(monitorDBfactory);
		}
    }
    
    private boolean isComponentInstalledInCluster() {
		String sIsClustered = System.getProperty(IS_CLUSTERED);
		boolean retValue = TRUE.equals(sIsClustered) ? true : false;
		return retValue;
	}

	/**
	 * Configure engine id and heartbeat
	 */
	private void configureCluster1() {
		mId = System.getProperty(INSTANCE_NAME);
		if (Utility.isEmpty(mId)) {
			mId = mProperties.getProperty(JUNIT_ENGINEID);
		}
		configureHeartbeat();
	}

	private boolean isComponentConfiguredToRunInCluster() {
		String sIsClustered = System.getProperty(IS_CLUSTERED_OVERRIDE_BPELSE);
		boolean retValue = TRUE.equals(sIsClustered) ? true : false;
		return retValue;
	}

	/**
	 * Configure engine id and heartbeat
	 */
	private void configureCluster2() {
		mId = getEngineIdForCluster();
		configureHeartbeat();
	}
    
    private String getEngineIdForCluster() {
		String engineId_Cluster_overrider = System.getProperty(ENGINEID_CLUSTERED_OVERRIDE_BPELSE);
    	if (engineId_Cluster_overrider == null || engineId_Cluster_overrider.equals("")) {
        	String msg = I18n.loc("BPCOR-7132: Engine is configured to be run one cluster mode, " +
        			"\nPlease specify Unique Engine Id (for each engine) using System Property - " + ENGINEID_CLUSTERED_OVERRIDE_BPELSE);
        	throw new BPELConfigurationException(msg);
    	}
    	return engineId_Cluster_overrider;
    }
    
    private void configureHeartbeat() {
		String engineExpiaryInterval = mProperties.getProperty(Engine.ENGINE_EXPIRY_INTERVAL);

		if (engineExpiaryInterval == null) {
			mHeartbeatUpdateConfigTime = 1000 * ENGINE_EXPIRY_INTERVAL_FACCTORYDEFAULT;
		} else {
			mHeartbeatUpdateConfigTime = 1000 * Integer.parseInt(engineExpiaryInterval);
		}
    }
    
    /**
	 * If persistence is enabled, scalability solution will be ON by default.
	 * This can be explicitly turned off by using system property
	 * "com.sun.jbi.bpelse.scalabilityLevel=none"
	 */
    private void configureScalability() {
    	String strScalabilityLevel = System.getProperty(SCALABILITY_LEVEL);
    	
    	if (strScalabilityLevel != null) {
    		if (!strScalabilityLevel.equals(SCALABILITY_LEVEL_NONE) && 
    				!strScalabilityLevel.equals(SCALABILITY_LEVEL_PHASE1) && 
    				!strScalabilityLevel.equals(SCALABILITY_LEVEL_PHASE2)) {
    			
    			String message = I18n.loc("BPCOR-6169: Scalability Level not correctly Specified. " +
    					"Please check the System Property Value for - {0} property. Found - {1}, but only one of - {2}, {3}, {4} is allowed",
    					SCALABILITY_LEVEL, strScalabilityLevel, SCALABILITY_LEVEL_NONE, SCALABILITY_LEVEL_PHASE1, SCALABILITY_LEVEL_PHASE2);
    			LOGGER.log(Level.SEVERE, message);
    			throw new BPELConfigurationException(message);
    		}
    	} else {
    		// if no system property for scalability is defined, the default level is Phase2
    		strScalabilityLevel = SCALABILITY_LEVEL_PHASE2;
    	}
    	
    	if (strScalabilityLevel.equals(SCALABILITY_LEVEL_NONE)) {
    		this.mMemoryMonitor = new BPELMemoryMonitorImpl(SCALABILITY_LEVEL_NONE, -1, -1, -1);
    		
    	} else {
    		
    		float upperMemoryThreshold;
    		float lowerMemoryThreshold;
    		long idleThreshold;
    		
    		try {
    			String strUpperMemoryThreshold = System.getProperty(UPPER_MEMORY_THRESHOLD);
    			String strLowerMemoryThreshold = System.getProperty(LOWER_MEMORY_THRESHOLD);
    			String strIdleThreshold = System.getProperty(IDLE_THRESHOLD);
    			
    			upperMemoryThreshold = getFloatValue(strUpperMemoryThreshold, UPPER_MEMORY_THRESHOLD_FACTORYDEFAULT);
    			lowerMemoryThreshold = getFloatValue(strLowerMemoryThreshold, LOWER_MEMORY_THRESHOLD_FACTORYDEFAULT);
    			idleThreshold = getLongValue(strIdleThreshold, IDLE_THRESHOLD_FACTORYDEFAULT);
    			
    		} catch (NumberFormatException nfe) {
    			String message = I18n.loc("BPCOR-6172: Scalability Values not correctly Specified");
    			LOGGER.log(Level.SEVERE, message, nfe.getMessage());
    			throw new BPELConfigurationException(message + nfe.getMessage());
    		}
    		
    		if (upperMemoryThreshold < lowerMemoryThreshold) {
    			String message = I18n.loc("BPCOR-6171: Scalability Values incorrect. " +
    					"The Specified value for Property - {0} must be higher than - {1}", UPPER_MEMORY_THRESHOLD, LOWER_MEMORY_THRESHOLD);
    			LOGGER.log(Level.SEVERE, message);
    			throw new BPELConfigurationException(message);
    		}
    		
    		this.mMemoryMonitor = new BPELMemoryMonitorImpl(strScalabilityLevel, upperMemoryThreshold, lowerMemoryThreshold, idleThreshold);
    	}
    }
    
    private void configureRecovery() {
    	String recoveryBatchSize = System.getProperty(RECOVERY_BATCH_SIZE);
    	if (recoveryBatchSize != null) {
    		this.mBatchRecoverySize = Integer.parseInt(recoveryBatchSize);
    	}
    	
    	String retryInt = System.getProperty(DB_RETRY_INT_PROPERTY_IN_SEC);
    	if (retryInt != null) {
    		try {
    			mRetryIntInSec = Integer.parseInt(retryInt);
    		} catch (Exception e) {/* Do nothing. default value will be used*/}
    	}
    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3000: Database connection retry interval is every {0} seconds.", 
    				new Integer(mRetryIntInSec)));
    	}
    }
    
	public int getRetryInterval() {
		return mRetryIntInSec;
	}
    
	
    private DBConnectionFactory setupMonitorDB (Properties props) {
    	DBConnectionFactory dbConnFactory = null;
    	String monitorEnabled = props.getProperty(Engine.MONITOR_ENABLED);
        String variableEnabled =  props.getProperty(Engine.MONITOR_VARIABLE_ENABLED);
        String kpiEnabled =  props.getProperty(Engine.KPI_ENABLED);
        if (monitorEnabled != null && monitorEnabled.equalsIgnoreCase(
                TRUE)  && !mMonitorEnabled) {
        	try {
        		if (mConnConfig != null) {
        			mConnConfig.createConnectionsPools();
        		}
        	} catch (Exception configExep) {
        		LOGGER.log(Level.SEVERE, 
        				I18n.loc("BPCOR-7005: Exception thrown while creating ConnectionConfiguration."), configExep);            		
        		//throw new RuntimeException(exep, configExep);
        	}        	
        	 mMonitorEnabled = true;
             if (mDBConnectionFactory == null) {
                 dbConnFactory = new DBConnectionFactory(mProperties, mInitialContext, this);
                 mDBConnectionFactory = dbConnFactory;
             }else {
                 dbConnFactory = mDBConnectionFactory;
             }

             STATUS statusSchema = MonitorDBSchemaCreation.getInstance().checkTablesIntegrity(mDBConnectionFactory);
             
             if(statusSchema == STATUS.CORRUPT){
                 throw monitoringTablesCreationException();
             }
             if((statusSchema == STATUS.EMPTY)&& mIsClustered){
                 throw monitoringTablesCreationException();
             }
             if(statusSchema == STATUS.EMPTY){
                 createMonitoringTables();
             }
        } else if (mDBConnectionFactory != null) {
            // this is the condition that the instance variable mMonitorEnabled = true, ie its been set
            // from the properties and then the user STOPS and then STARTS the engine without SHUTDOWN
            dbConnFactory = mDBConnectionFactory;
        }
        if (variableEnabled != null && variableEnabled.equalsIgnoreCase(TRUE)  && ! mVariableMonitorEnabled) {
                mVariableMonitorEnabled = true;
        } 
        if (kpiEnabled != null && kpiEnabled.equalsIgnoreCase(TRUE)  && !mKPIEnabled){
                mEventManager.resetProperties(mProperties);        
                mKPIEnabled = true;
        }     
        return dbConnFactory;

	}

	public Collection getBPELProcesses() {
        return mBPELProcessManagerMap.keySet();
    }

    public Collection<BPELProcessManager> getBPELProcessManagers() {
        return mBPELProcessManagerMap.values();
    }

    public BPELProcessManager getBPELProcessManager(QName bpelId) {
        Collection<BPELProcessManager> managers = mBPELProcessManagerMap.values();
        for (BPELProcessManager manager : managers) {
            if (bpelId.equals(manager.getBPELProcess().getBPELId())) {
                return manager;
            }
        }
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getState()
     */
    public int getState() {
        return mState;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#isClustered()
     */
    public boolean isClustered() {
        return mIsClustered;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine#disableDebugger()
     */
    public void disableDebugger() {
        LOGGER.log(Level.INFO, "BPELInterpreter_Disabling_debug_server");
    
        if (mDebugServer != null) {
            LOGGER.log(Level.INFO, "BPELInterpreter_Destroying_running_debug_server");
            mDebugServer.destroy();
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine#enableDebugger()
     */
    public void enableDebugger() {
        LOGGER.log(Level.INFO, "BPELInterpreter_Enabling_debug_server");
    
        String debugPort = getDebugPort();
    
        if (debugPort != null) {
            int port = Integer.valueOf(debugPort).intValue();
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-5002: Restarting existing debug server on port: {0}", debugPort));
    
            if (port > 0) {
                if (mDebugServer != null) {
                    LOGGER.log(Level.INFO, I18n.loc("BPCOR-5003: BPELInterpreter Destroying existing debug server"));
                    mDebugServer.destroy();
                }
    
                mDebugServer = new DebugServer(this, this, port, new HashMap(), new HashMap());
                LOGGER.log(Level.INFO, I18n.loc("BPCOR-5004: BPELInterpreter Starting debug server"));
                mDebugServer.start();
            }
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine#getDebugger()
     */
    public BPELDebugger getDebugger() {
        return mDebugger;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine#setDebugger(org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger)
     */
    public void setDebugger(BPELDebugger debugger) {
        mDebugger = debugger;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine#setDebuggerPort(java.lang.String)
     */
    public void setDebuggerPort(String port) {
        LOGGER.log(Level.CONFIG, I18n.loc("BPCOR-4003: Setting debugger port: {0}", port));
    
        if (isDebugEnabled()) {
            if (mDebugServer != null) {
            	LOGGER.log(Level.INFO, I18n.loc("BPCOR-5003: BPELInterpreter Destroying existing debug server"));
                mDebugServer.destroy();
            }
    
            mDebugServer = new DebugServer(this, this, Integer.valueOf(port).intValue(), new HashMap(), new HashMap());
            LOGGER.log(Level.INFO, I18n.loc("BPCOR-5004: BPELInterpreter Starting debug server"));
            mDebugServer.start();
        }
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#setConnectionConfiguration(com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionConfiguration)
     */
    public void setConnectionConfiguration(ConnectionConfiguration connConfig) {
    	mConnConfig = connConfig;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getConnectionConfiguration()
     */
    public ConnectionConfiguration getConnectionConfiguration() {
    	return mConnConfig;
    }

	public void postEvent(BPELEvent event) {
		// TODO Auto-generated method stub
		mEventManager.processEvent(event);
	}

	public boolean isMonitorEnabled () {
		return mMonitorEnabled;
	}
    
    public boolean isVariableMonitorEnabled () {
        return mVariableMonitorEnabled;
    }    
	
	public boolean isKPIEnabled () {
		return mKPIEnabled;
	}

    public BPELMemoryMonitor getBpelMemoryMonitor() {
        return mMemoryMonitor;
    }

    public void setBpelMemoryMonitor(BPELMemoryMonitor bpelMemoryMonitor) {
        this.mMemoryMonitor = bpelMemoryMonitor;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getDBConnectionFactory()
     */
    public DBConnectionFactory getDBConnectionFactory() {
        //return new DBConnectionFactory(mProperties, mInitialContext, this);
    	return mDBConnectionFactory;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine#getDeploymentLookup()
     */
    public DeploymentLookup getDeploymentLookup() {
    	return mDeploymentLookup;
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine
     * 	#setDeploymentLookup(com.sun.jbi.common.qos.descriptor.DeploymentLookup)
     */
    public void setDeploymentLookup(DeploymentLookup deploymentLookup) {
		mDeploymentLookup = deploymentLookup;
	}
    
    private float getFloatValue(String str, float defaultVal) {
		if (str != null && !str.equals("")) {
			return Float.valueOf(str.trim()).floatValue();
		} 
		
		return defaultVal;
    }
    
    private long getLongValue(String str, long defaultVal) {
    	if (str != null && !str.equals("")) {
    		return Long.valueOf(str.trim()).longValue();
    	} 
    	
    	return defaultVal;
    }
    
    public void installQosParams(String suName, String suPath)throws DeploymentException {
        mChannel.installServiceQualities(suName, suPath);
    }

    public void uninstallQosParams(String suName) {
        mChannel.uninstallServiceQualities(suName);
    }

    public CustomClassLoaderUtil getClassLoaderContext() {
        return classLoaderContext;
    }

    public void setThreadMgr(ThreadManager threadMgr) {
        this.threadMgr = threadMgr;
    }
    
    public void notifyThreads() {
        if (threadMgr == null) {
            return;
        }
        threadMgr.notifyThreads();
    }

    public Map<String, Object> getApplicationVariables() {
        return mApplicationVariables;
    }

    public void setApplicationVariables(Map<String, Object> appVars) {
        mApplicationVariables = appVars;
    }

    public void setValidationEnabled(boolean validationEnabled) {
        if (validationEnabled && !mValidationEnabled ) {
            mValidationEnabled = validationEnabled;

            mProperties.put(Engine.VALIDATION_ENABLED, validationEnabled);
            mEventManager.resetProperties(mProperties);
        }  else if (!validationEnabled && mValidationEnabled) {
            mValidationEnabled = validationEnabled;
            mProperties.put(Engine.VALIDATION_ENABLED, validationEnabled);
            mEventManager.resetProperties(mProperties);
        }
    }

    public boolean isValidationEnabled() {
        return mValidationEnabled;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.Engine
     * 	#isSUStarted(String suName)
     */
    public boolean isSUStarted(String suName) {
        return (suStateMap.get(suName) == SUStates.Started);
    }

    public void setSUState(String suName, SUStates state) {
        if (state == SUStates.Started) {
            synchronized (newDeploymentLock) {
                suStateMap.put(suName, SUStates.Started);
                //update deployment flag, so that recovery is initiated
                mNewAddedDep = true;
            }
        } else if (state == SUStates.Stop) {
            suStateMap.put(suName, SUStates.Stop);
        } else if (state == SUStates.ShutDown) {
            //do clean-up
            suStateMap.remove(suName);
        }
    }
}
