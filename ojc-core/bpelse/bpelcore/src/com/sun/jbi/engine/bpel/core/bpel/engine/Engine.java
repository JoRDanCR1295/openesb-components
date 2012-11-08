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
 * @(#)Engine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.util.Collection;
import java.util.Properties;

import javax.jbi.management.DeploymentException;
import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionConfiguration;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import java.util.Map;

/**
 * Engine
 *
 * @author Sun Microsystems
 * @version $Revision: 1.37 $
 */
public interface Engine {

    public enum TransformEngine {

        XSLT_1_0, XSLT_2_0
    }
    /** Database persistence enabled setting when there is no setting defined in jbi.xml */
    public static final Boolean PERSISTENCE_ENABLED_FACCTORYDEFAULT = false;
    /** Database JNDI Name setting when there is no setting defined in jbi.xml for the NonXA connection pool*/
    public static final String DB_NONXA_JNDI_NAME_FACCTORYDEFAULT = "jdbc/DerbyNonXA";
    /** Database JNDI Name setting when there is no setting defined in jbi.xml for the XA connection pool*/
    public static final String DB_XA_JNDI_NAME_FACCTORYDEFAULT = "jdbc/DerbyXA";
    /** debug setting when there is no setting defined in jbi.xml */
    public static final Boolean DEBUG_ENABLED_FACCTORYDEFAULT = false;
    /** debug port setting when there is no setting defined in jbi.xml */
    public static final Integer DEBUG_PORT_FACCTORYDEFAULT = 3343;
    /** number of threads setting when there is no setting defined in jbi.xml */
    public static final Integer THREAD_COUNT_FACCTORYDEFAULT = 10;
    /** Engine Expiry Interval setting used in cluster when there is no setting defined in jbi.xml */
    public static final Integer ENGINE_EXPIRY_INTERVAL_FACCTORYDEFAULT = 15;
    /** Engine Waiting Request Life Span with there is not setting defined in jbi.xml */
    public static final Integer WAITING_REQUEST_LIFE_SPAN_FACTORYDEFAULT = 0;
    /** Monitor setting when there is no setting defined in jbi.xml */
    public static final Boolean MONITOR_ENABLED_FACCTORYDEFAULT = false;
    /** Monitoring Variable setting when there is no setting defined in jbi.xml */
    public static final Boolean MONITOR_VARIABLE_ENABLED_FACCTORYDEFAULT = false;
    /** KPI setting when there is no setting defined in jbi.xml */
    public static final Boolean KPI_ENABLED_FACCTORYDEFAULT = false;
    /** bpel validation setting when there is no setting defined in jbi.xml */
    public static final Boolean VALIDATION_ENABLED_FACCTORYDEFAULT = true;
    public static final String IS_CLUSTERED = "com.sun.jbi.isClustered"; //$NON-NLS-1$
    public static final String IS_CLUSTERED_OVERRIDE_BPELSE = "com.sun.jbi.bpelse.isClustered"; //$NON-NLS-1$    
    public static final String ENGINEID_CLUSTERED_OVERRIDE_BPELSE = "com.sun.jbi.bpelse.isClustered.engineId"; //$NON-NLS-1$    
    public static final String INSTANCE_NAME = "com.sun.jbi.instanceName"; //$NON-NLS-1$
    public static final String JUNIT_ENGINEID = "com.sun.jbi.bpelse.system-tests.cluster.engineid";  //$NON-NLS-1$
    /** Scalability Related */
    public static final String SCALABILITY_LEVEL = "com.sun.jbi.bpelse.scalabilityLevel"; //$NON-NLS-1$
    public static final String UPPER_MEMORY_THRESHOLD = "com.sun.jbi.bpelse.scalability.upperMemoryThreshold"; //$NON-NLS-1$
    public static final String LOWER_MEMORY_THRESHOLD = "com.sun.jbi.bpelse.scalability.lowerMemoryThreshold"; //$NON-NLS-1$
    public static final String IDLE_THRESHOLD = "com.sun.jbi.bpelse.scalability.idleThreshold"; //$NON-NLS-1$
    public static final String SCALABILITY_LEVEL_NONE = "none"; //$NON-NLS-1$
    public static final String SCALABILITY_LEVEL_PHASE1 = "phase1"; //$NON-NLS-1$
    public static final String SCALABILITY_LEVEL_PHASE2 = "phase2"; //$NON-NLS-1$
    public static final float UPPER_MEMORY_THRESHOLD_FACTORYDEFAULT = 0.75f;
    public static final float LOWER_MEMORY_THRESHOLD_FACTORYDEFAULT = 0.50f;

    /*
     * Memory release age criterion. This defines the age below which
     * the instances will not be picked up for scalability passivation. Default 5 Secs
     */
    public static final long IDLE_THRESHOLD_FACTORYDEFAULT = 2;
    public static final String RECOVERY_BATCH_SIZE = "com.sun.jbi.bpelse.recoveryBatchSize";
    /* The system property name to get the retry interval from */
    public static final String DB_RETRY_INT_PROPERTY_IN_SEC = "com.sun.jbi.bpelse.dbConnRetryIntInSec";
    /** join-only receive */
    int RECEIVE_TYPE_NO_COR_JOIN_ONLY = 1;
    /** create-only receive */
    int RECEIVE_TYPE_CREATE_ONLY = 2;
    /** create-or-correlate receive */
    int RECEIVE_TYPE_CREATE_OR_CORRELATE = 3;
    /** correlate-only receive */
    int RECEIVE_TYPE_CORRELATE_ONLY = 4;
    /** create-or-correlate receive for duplicate operations within same BPEL */
    int RECEIVE_TYPE_CREATE_OR_CORRELATE_DUP_OPER = 5;
    /** bpel debug server port */
    String DEBUG_PORT = "DebugPort"; //$NON-NLS-1$
    /** whether bpel debugger is enabled */
    String DEBUG_ENABLED = "DebugEnabled"; //$NON-NLS-1$
    /** whether bpel validation is enabled */
    String VALIDATION_ENABLED = "ValidationEnabled"; //$NON-NLS-1$
    String TRANSFORM_ENGINE = "TransformEngine";
    //The following string values are borrowed from WSDL 2.0 spec.
    /** In Only Operation pattern */
    String IN_ONLY = "http://www.w3.org/2004/03/wsdl/in-only"; //$NON-NLS-1$
    /** Robust In Only Operation pattern */
    String ROBUST_IN_ONLY = "http://www.w3.org/2004/03/wsdl/robust-in-only"; //$NON-NLS-1$
    /** In Out Operation pattern */
    String IN_OUT = "http://www.w3.org/2004/03/wsdl/in-out"; //$NON-NLS-1$
    /** In Optional Out Operation pattern */
    String IN_OPTIONAL_OUT = "http://www.w3.org/2004/03/wsdl/in-opt-out"; //$NON-NLS-1$

    /* flags for clean up of the persistent state of the BP */
    /* never delete the persisted data after completion of BP instance*/
    /** no delete */
    String NO_DELETE = "NO_DELETE"; //$NON-NLS-1$

    /* always delete the persisted data after completion of BP instance
     * No guarantees, it could delete immediately or could schedule
    a delete periodically*/
    /** delete */
    String DELETE = "DELETE"; //$NON-NLS-1$

    /* delete the persisted data after a configured time elapsed,
    guaranteed to
     * delete only after the configured time*/
    /** deferred delete */
    String DEFERRED_DELETE = "DEFERRED_DELETE"; //$NON-NLS-1$
    /** deferred delete until value */
    long DEFER_DELETE_UNTIL = 1000000;
    /** Username property tag string pattern */
    String DB_USERNAME = "DB_UserName"; //$NON-NLS-1$
    /** password property tag string pattern */
    String DB_PASSWORD = "DB_Password"; //$NON-NLS-1$
    /** persistence enabled/disabled property tag string pattern */
    String PERSISTENCE_ENABLED = "PersistenceEnabled"; //$NON-NLS-1$
    /** monitor enabled **/
    String MONITOR_ENABLED = "MonitoringEnabled";
    /** kpi enabled **/
    String KPI_ENABLED = "KPIEnabled";
    /** variable monitor enabled **/
    String MONITOR_VARIABLE_ENABLED = "MonitoringVariableEnabled";
    /** persistence enabled/disabled property tag string pattern */
    String RELIABILITY_ENABLED = "ReliabilityEnabled"; //$NON-NLS-1$
    /** Database JNDI Name property tag string pattern */
    String DB_XA_JNDI_NAME = "DatabaseXAJNDIName"; //$NON-NLS-1$
    String DB_NON_XA_JNDI_NAME = "DatabaseNonXAJNDIName"; //$NON-NLS-1$
    String THREAD_COUNT = "ThreadCount";
    /** engine expiry interval property tag string pattern */
    String ENGINE_EXPIRY_INTERVAL = "EngineExpiryInterval"; //$NON-NLS-1$
    /** waiting request life span property tag string pattern */
    String WAITING_REQUEST_LIFE_SPAN = "WaitingRequestLifeSpan"; //$NON-NLS-1$
    public static float HEARTBEAT_UPDATE_FACTOR = 0.6f;
    /** Running state **/
    public static final int RUNNING = 0;
    /** Stopped state **/
    public static final int STOPPED = 1;
    /** Shutdown state **/
    public static final int SHUTDOWN = 2;

    /**
     * gets connction pool size
     *
     * @return int connection pool size
     */
    int getConnPoolsize();

    /**
     * gets maximum connection pool size
     *
     * @return int maximum connection pool size
     */
    int getMaxConnPoolSize();

    /**
     * gets connection timeout
     *
     * @return long connection timeout
     */
    long getConnectionTimeOut();

    /**
     * gets clean-up mode
     *
     * @return String clean-up mode
     */
    String getCleanUpMode();

    /**
     * gets clean-up delay time
     *
     * @return long clean-up delay time
     */
    Long getCleanUpDelayTime();

    /**
     * gets engine ID
     *
     * @return String engine ID
     */
    String getId();

    /**
     * gets location
     *
     * @return String engine location
     */
    String getLocation();

    /**
     * gets engine lease expiration
     *
     * @return long engine lease expiration
     */
    Long getExpiration();

    /**
     * gets state manager
     *
     * @return StateManager state manager
     */
    StateManager getStateManager();

    /**
     * gets engine state manager
     *
     * @return Returns the mEngStateMgr.
     */
    EngineStateManager getEngStateMgr();

    /**
     * check if persistence is enabled
     *
     * @return boolean: if persistence is enabled, returns true; otherwise, returns false
     */
    boolean isPersistenceEnabled();

    /**
     * @return Returns the reliabilityEnabled.
     */
    public boolean isReliabilityEnabled();

    /**
     * @param reliability Enabled the reliability of the message exchanges
     */
    public void setReliabilityEnabled(boolean reliability);

    /** 
     * Executes engine recovery. The return flag indicates that the recovery 
     * of the instances completed or not. During recovery if there are huge 
     * number of instances to be recovered, we do not want to load all the 
     * Instances in the memory as the engine may run out of memory. Following 
     * is the behavior for single engine and cluster case.
     * 
     * Single Engine Recovery:
     * - To fee up threads for forward path execution, the recovery will be performed
     *   using one thread only. If one thread is in the middle of performing recovery
     *   other threads will be returned.
     * - If the instances to be recovered are larger than the defined batch size
     *   the recovery will be performed in batches.
     * - After each batch is scheduled, memory check will be made to see if the 
     *   available memory is greater than as defined by phase 1 lower limit. 
     *   (refer scalabillity design/configurations for details on this).
     * - If the recovery cannot be completed (due to memory limitations), 
     *   this method will return false will be returned.
     *
     * Cluster Case:
     * - For cluster case, the logic for recovery is same as single engine case
     *   but, recovery of instances is proceeded by failover of instances. 
     *   For completeness, here is quick overview of the failover logic for 
     *   cluster. When any engine discovers that some engine(s) in cluster has
     *   failed, it will try to acquire the ownership of the instances of the 
     *   failed engine(s) up-to maximum of batch size and recover them using 
     *   this api. This call will be recursive till all the instances of the 
     *   failed engine(s) are recovered.
     * - Note that this api checks for the size of the instances to be recovered
     *   against the batch size, and since for cluster we are acquiring only 
     *   batch size, so this check seems redundant for cluster case, but this 
     *   check is required, as explained below:
     *   Consider a situation wherein there are two engines in a cluster processing
     *   requests. Due to some error, both engine dies. Thereafter, say only one engine
     *   is started. This time, this (restarting) engine need to recover the instances
     *   of the other engine and also its own instances. Hence, under such situation
     *   the instances to be recover can exceed the batch size when this api is called.
     *   Hence, this check is needed for cluster case as well. (Same argument applies
     *   if only one engine is installed in cluster).
     *   
     * @throws Exception Exception
     */
    boolean recover() throws Exception;

    /**
     * gets bpel processor
     *
     * @return bpel processor
     */
    public BPELInterpreter getInterpreter();

    /**
     * process
     */
    public void process();

    /**
     * End of the persisted related implementation.
     * 
     * @param event incoming event key
     * @param contents message content
     */
    public void process(InComingEventKeyImpl event, MessageContainer contents);

    /**
     * gets next scheduled time
     *
     * @return long scheduled time
     */
    public long getNextScheduledTime();

    /**
     * sets outbound channel
     *
     * @param channel engine channel
     */
    public void setOutChannel(Channel channel);

    /**
     * adds bpel model
     *
     * @param id location of the BPEL file.
     * @param bpelProcess runtime bpel process
     */
    void addModel(RBPELProcess bpelProcess, String saName, String suName);

    /**
     * This overloaded addModel() is used only by the BPELDebugger Engine implementation for junit test case requirements.
     * Ideally this has to be removed when the test cases are re-factored.
     * @param id location of the BPEL file.
     * @param bpelProcess runtime bpel process
     * @param processManager runtime manager for the bpel process.
     */
    void addModel(RBPELProcess bpelProcess, BPELProcessManager processManager);

    /**
     * removes bpel model
     *
     * @param id bpel process model ID
     */
    //void removeModel(String id);
    void removeModel(QName id);

    /**
     * This is to support WSDL 1.1 overloaded operations.
     *
     * @param bpelProcess runtime bpel process
     * @param sa start activity
     * @param operPattern operation pattern
     */
    void addStartActivityModel(RBPELProcess bpelProcess, RStartElement sa, String operPattern);

    /**
     * DOCUMENT ME!
     *
     * @return boolean
     */
    boolean isDebugEnabled();

    /**
     * DOCUMENT ME!
     *
     * @param debugEnabled The mDebugEnabled to set.
     */
    void setDebugEnabled(boolean debugEnabled);

    /**
     * DOCUMENT ME!
     *
     * @return Returns the mDebugPort.
     */
    String getDebugPort();

    /**
     * DOCUMENT ME!
     *
     * @param debugPort The mDebugPort to set.
     */
    void setDebugPort(String debugPort);

    /**
     * Fetches the {@link TransformEngine} configured for this component.
     * @return the <code>TransformEngine</code> configured for this component.
     */
    TransformEngine getTransformEngine();

    /**
     * Sets the {@link TransformEngine} for this component.
     * @param transformEngine The configured engine.
     */
    public void setTransformEngine(TransformEngine transformEngine);

    /**
     * Returns application variables avaiable for this component
     * @return
     */
    Map<String, Object> getApplicationVariables();

    /**
     * sets application variables available for this component
     * @param appVars
     */
    void setApplicationVariables(Map<String, Object> appVars);

    /**
     * The life span of waiting requests in seconds.
     * 
     * @return The life span of waiting requests in seconds.
     */
    long getWaitingRequestLifeSpan();

    /**
     * 
     * @param waitingRequestLifeSpan The life span of waiting requests in seconds.
     */
    void setWaitingRequestLifeSpan(long waitingRequestLifeSpan);

    /**
     * API to change the engine properties
     *
     * @param properties properties
     */
    void resetProperties(Properties properties);

    /**
     * A place to do cleanup before being stopped.
     *
     */
    void preStop();

    /**
     * A place to do cleanup before being shutdown.
     *
     */
    void preShutdown();

    /**
     * Setting the state before start.
     *
     */
    void preStart();

    /**
     * gets the list of BPELProcesses deployed on the engine 
     * @return Collection
     */
    public Collection getBPELProcesses();

    /**
     * gets the BPELProcessManager for the given bpelId.
     * @return Collection
     */
    public BPELProcessManager getBPELProcessManager(QName bpelId);

    /**
     * Get a Collection of BPELProcessManagers registered with the engine.
     * @return
     */
    public Collection<BPELProcessManager> getBPELProcessManagers();

    /**
     * Get the Engine Expiry Interval Property
     * @return long
     */
    public long getEngineExpiryInterval();

    /**
     * Get the Engine state
     * 
     * @return int, RUNNING, STOPPED or SHUTDOWN
     */
    public int getState();

    /**
     * Check if the engine is participating in cluster.
     * 
     * @return
     */
    public boolean isClustered();

    /**
     * Register the ConnectionConfiguration on the engine. The engine should use
     * this class to create the Connection objects(connection pools, jdbc jndi resources, etc)
     * This should be done by the Context in which the engine is running in (ex: appserver). 
     * The context would have access and information needed to create the connections.
     * @param connConfig of type ConnectionConfiguration.
     */
    public void setConnectionConfiguration(ConnectionConfiguration connConfig);

    /**
     * returns the ConnectionConfiguration class that was set on this Engine.
     * @return ConnectionConfiguration object
     */
    public ConnectionConfiguration getConnectionConfiguration();

    /**
     * Posts a BPEL event to the event queue.
     * As the event queue has a capacity of Integer.MAX_VALUE, this method returns immediately
     * @param event
     */
    public void postEvent(BPELEvent event);

    /**
     * Returns the monitorEnabled flag
     * @return
     */
    public boolean isMonitorEnabled();

    /**
     * Returns the KPIEnabled flag
     * @return
     */
    public boolean isKPIEnabled();

    /**
     * Returns the VariableMonitoringEnabled flag
     * @return
     */
    public boolean isVariableMonitorEnabled();

    /**
     * Returns BPELMemoryMonitor
     * @return
     */
    public BPELMemoryMonitor getBpelMemoryMonitor();

    /**
     * Set BPEL Memory Monitor
     * @param bpelMemoryMonitor
     */
    public void setBpelMemoryMonitor(BPELMemoryMonitor bpelMemoryMonitor);

    /**
     * @return
     */
    public DBConnectionFactory getDBConnectionFactory();

    /**
     * The DeploymentLookup associated with this engine component context.
     * @param deploymentLookup
     */
    void setDeploymentLookup(DeploymentLookup deploymentLookup);

    /**
     * Return the DeploymentLookup associated with this engine component context.
     * @return
     */
    DeploymentLookup getDeploymentLookup();

    /**
     * Get retry interval for re-trying for database connection in the event
     * if transient failures in connection to database.
     *
     * @return
     */
    int getRetryInterval();

    public void installQosParams(String suName, String suPath) throws DeploymentException;

    public void uninstallQosParams(String suName);

    public CustomClassLoaderUtil getClassLoaderContext();

    /**
     * Notifies threads on the work waiting in there is work in ReadyToRunQueue
     * IMA activities when they are participating in transaction (XA) their
     * Synchronization object is notified on completion of transaction.current
     * implementation of these notification methods create BPIT and put into
     * ReadyToRunQueue. This works fine when Engine is processing actively, but
     * instance gets stuck at this activity otherwise.
     */
    public void notifyThreads();

    public void setThreadMgr(ThreadManager threadMgr);

    /**
     * sets validation enabled flag value
     * @param validationEnabled - validation enabled flag
     */
    public void setValidationEnabled(boolean validationEnabled);

    /**
     * returns validation enabled flag
     * @return ValidationEnabled - validation enabled flag
     */
    public boolean isValidationEnabled();

    /**
     * It queries su status
     * @param suName - service unit name
     * @return true if given suName is stated
     */
    public boolean isSUStarted(String suName);

    public enum SUStates {

        ShutDown, Stop, Started
    }

    /**
     * Set or update state of SU
     * @param suName
     * @param state
     */
    public void setSUState(String suName, SUStates state);
}
