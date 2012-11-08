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
 * @(#)RoundRobin.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.scheduler;

import com.sun.jbi.engine.iep.core.runtime.change.OperatorPropertyChange;
import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeObject;
import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeSet;
import com.sun.jbi.engine.iep.core.runtime.data.access.DataAccessHandler;
import java.util.List;
import java.util.Collections;
import java.util.Properties;
import java.util.Map;

import java.sql.Timestamp;
import java.sql.Connection;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorComparator;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.NDC;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.logging.Level;

/**
 * RoundRobin.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class RoundRobin implements Scheduler {

    private static final Messages mMessages = Messages.getMessages(RoundRobin.class);
    private Properties mConfigProp;
    private long mTimeUnit;
    private transient boolean mStop;
    private Object mMonitor = new Object();
    private Object mMonitorStop = new Object();
    private Thread mThread;
    private QueryPlan mQueryPlan;
    private Map<String, Object> mProcessingState;
    private Properties mPlanPs;
    private Map<String, Object> mOperatorPs;
    // For Ext Data Access
    private DataAccessHandler mDAHandler = new DataAccessHandler();
    //
    //For Admin bean Operator property change scheduled list.
    //private List<OperatorPropertyChange> mPropChangeTasks ;
    private RuntimeChangeSet rtSet;
    private boolean mChangesCompleted = false;
    //
    private int mMaxBatchSize;

    private Timestamp timestampToProcess(Timestamp suggestedTsToProcess, Timestamp prevTsToProcess) {
        return new Timestamp(Math.min(prevTsToProcess.getTime() + mMaxBatchSize * 1000,
                suggestedTsToProcess.getTime()));
    }

    private boolean hasWorkToDo(Timestamp timestamp) {
        List<Operator> opList = mQueryPlan.getOperatorList();
        for (Operator op : opList) {
            if (op.hasWorkToDo(timestamp)) {
                return true;
            }
        }
        return false;
    }

    public RoundRobin(Map<String, Object> prop) {
        mConfigProp = (Properties) prop.get(PROP_CONFIG_PROPERTIES);
        mMaxBatchSize = PropertyUtil.getint(mConfigProp, PROP_MAXIMUM_BATCH_SIZE, MAXIMUM_BATCH_SIZE_FACTORY_DEFAULT);
        mTimeUnit = PropertyUtil.getlong(prop, PROP_TIME_UNIT, 1000L);
        mQueryPlan = (QueryPlan) prop.get(PROP_QUERY_PLAN);
    }

    private String getName() {
        return mQueryPlan.getInstanceId() + "(RoundRobin-Scheduler)";
    }

    public QueryPlan getQueryPlan() {
        return mQueryPlan;
    }

    @SuppressWarnings("unchecked")    //mOperatorPs = (Map<String, Object>)mProcessingState.get(PS_OPERATOR_STATE);
    public void run() {
        Connection con = null;
        List<Operator> opList = null;
        String instanceId = null;
        try {
            NDC.enter("Application", "IEPSE", "Task", getName() + ".run");
            instanceId = mQueryPlan.getInstanceId();
            Timestamp tsToProcess;

            con = Util.getConnection(mConfigProp);
            con.setAutoCommit(false);

            mProcessingState = Util.getProcessingState(con, mQueryPlan.getId());
            mPlanPs = (Properties) mProcessingState.get(PS_PLAN_STATE);
            mOperatorPs = (Map<String, Object>) mProcessingState.get(PS_OPERATOR_STATE);
            String prevTsToProcessStr = mPlanPs.getProperty(PS_PREV_TIMESTAMP_TO_PROCESS);

            // Initialized to the previous save value, and updated to timestampToProcess 
            // after timestampToProcess is used in operator.hasWorkTodo or operator.operate
            Timestamp prevTsToProcess = prevTsToProcessStr == null ? new Timestamp(0L) : new Timestamp(Long.parseLong(prevTsToProcessStr));

            opList = mQueryPlan.getOperatorList();
            Collections.sort(opList, OperatorComparator.getInstance());
            for (Operator op : opList) {
                op.setRuntimeConnection(con);
                Properties opPs = (Properties) mOperatorPs.get(op.getId());
                if (opPs == null) {
                    continue;
                }
                opPs.setProperty(PS_PREV_TIMESTAMP_TO_PROCESS, prevTsToProcessStr);
                op.initProcessingState(opPs);
            }

            tsToProcess = Util.nextTimestampToCheck(con, mQueryPlan);
            while (!isStopped()) {
                //Need to move into a queue of runnable for synchronization .
                checkAndResetOperator(con);
                while (!hasWorkToDo(tsToProcess)) {
                    prevTsToProcess = tsToProcess;
                    mPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_CHECK, "" + prevTsToProcess.getTime());
                    Util.updateProcessingState(con, mQueryPlan.getId(), mProcessingState);
                    con.commit();
                    synchronized (mMonitor) {
                        try {
                            mMonitor.wait(mTimeUnit);
                            checkAndResetOperator(con);
                        } catch (InterruptedException e) {
                            mMessages.log(Level.WARNING, "RoundRobin.scheduler_is_interrupted", instanceId, e);
                        }
                    }
                    if (isStopped()) {
                        con.commit();
                        return;
                    }
                    tsToProcess = timestampToProcess(Util.nextTimestampToCheck(con, mQueryPlan), prevTsToProcess);
                }
                for (Operator op : opList) {
                    op.operate(tsToProcess);
                    //For Ext Data Access
                    mDAHandler.handle(op, con);
                }

                prevTsToProcess = tsToProcess;
                tsToProcess = timestampToProcess(Util.nextTimestampToCheck(con, mQueryPlan), prevTsToProcess);
                mPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_CHECK, "" + prevTsToProcess.getTime());
                mPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_PROCESS, "" + prevTsToProcess.getTime());
                for (Operator op : opList) {
                    mOperatorPs.put(op.getId(), op.getProcessingState());
                }
                Util.updateProcessingState(con, mQueryPlan.getId(), mProcessingState);
                con.commit();
                Thread.yield();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "RoundRobin.scheduler_fail", instanceId, e);
        } finally {
            for (Operator op : opList) {
                op.unsetRuntimeConnection();
            }
            Util.close(con);
            con = null;
            // see stop
            destroyThread();
            synchronized (mMonitorStop) {
                mMonitorStop.notifyAll();
            }
            NDC.exit("Application", "IEPSE", "Task", getName() + ".run");
        }
    }
    // Each RoundRobin instance runs no more than one thread 
    public synchronized void start() {
        if (mThread == null) {
            mThread = new Thread(this, getName());
        }
        mStop = false;
        mThread.start();
    }

    public void stop() {
        String instanceId = mQueryPlan.getInstanceId();
        setStop();
        synchronized (mMonitor) {
            mMonitor.notifyAll();
        }
        while (!isThreadDestroyed()) {
            synchronized (mMonitorStop) {
                try {
                    mMonitorStop.wait(200);
                } catch (InterruptedException e) {
                    mMessages.log(Level.SEVERE, "RoundRobin.scheduler_fail_to_stop", instanceId, e);
                }
            }
        }
    }

    private synchronized void setStop() {
        mStop = true;
    }

    private synchronized boolean isStopped() {
        return mStop;
    }

    private synchronized void destroyThread() {
        mThread = null;
    }

    private synchronized boolean isThreadDestroyed() {
        return mThread == null;
    }

    private void checkAndResetOperator(Connection con) throws Exception {
        if (mChangesCompleted && rtSet != null && rtSet.hasChanges()) {
            for (RuntimeChangeObject rCO : rtSet.getChangeList()) {
                if (rCO.getType() == RuntimeChangeObject.OPERATOR_PROPERTY_CHANGE) {
                    OperatorPropertyChange pCO = (OperatorPropertyChange) rCO;
                    Operator opr = mQueryPlan.getOperatorByName(pCO.getMOprName());
                    opr.setAdministrableProperty(pCO.getMPropName(), pCO.getMPropValue());

                    mQueryPlan.changeAndPersistOperatorProperty(con, opr,
                            pCO.getMPropName(), pCO.getMPropValue().toString());
                    con.commit();
                    if (opr.needsReset()) {
                        opr.setRuntimeConnection(con);
                    }

                }
            }
            //assuming one time task irrespective of failure or success.  
            rtSet.clear();
            mChangesCompleted = false;
        }
    }

    public String scheduleOperatorPropertyChange(OperatorPropertyChange val) {
        String result = null;
        if (rtSet == null) {
            rtSet = new RuntimeChangeSet();
        }
        rtSet.addAChange(val);
        return result;

    }

    public String scheduleApplyChanges() {
        String result = null;
        mChangesCompleted = true;
        return result;
    }

    public void ignoreChangeSet() {
        mChangesCompleted = false;
        if (rtSet != null) {
            rtSet.clear();
        }
    }
}
