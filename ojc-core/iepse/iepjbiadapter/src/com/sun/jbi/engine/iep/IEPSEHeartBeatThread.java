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
 * @(#)IEPSEHeartBeatThread.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.NDC;
import com.sun.jbi.engine.iep.core.runtime.util.Token;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;
import java.sql.Timestamp;
import java.sql.Connection;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * IEPSEHeartBeatThread.java
 *
 * Created on Sep 18, 2007, 10:56:41 AM
 *
 * @author Bing Lu
 */
public class IEPSEHeartBeatThread extends Thread implements OperatorConstants {

    private static final Messages mMessages = Messages.getMessages(IEPSEHeartBeatThread.class);
    private static final Logger mLogger = Messages.getLogger(IEPSEHeartBeatThread.class);
    private boolean mRunning;
    private Object mRunningMonitor = new Object();
    private boolean mStopped;
    private Object mStoppedMonitor = new Object();
    private ExtendedComponentContext mExtendedContext;

    public IEPSEHeartBeatThread(ExtendedComponentContext extendedContext) {
        super("IEPSEHeartBeatThread");
        mExtendedContext = extendedContext;
        mRunning = false;
    }

    public void run() {
        Connection con = null;
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEHeartBeatThread.run");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEHeartBeatThread.Started_IEP_service_engine_heart_beat_thread"));
            }
            mRunning = true;
            mStopped = false;
            Properties configProp = mExtendedContext.getConfigProperties();
            String engineId = configProp.getProperty(PROP_ENGINE_ID);
            int engExpInterval = Integer.valueOf(configProp.getProperty(PROP_ENGINE_EXPIRY_INTERVAL));
            long engExpTime = System.currentTimeMillis() + 1000L * engExpInterval;
            IEPEngine engine = mExtendedContext.getEngine();
            con = Util.getConnection(configProp);
            con.setAutoCommit(false);

            while (mRunning) {
                long currentTime = System.currentTimeMillis();
                engExpInterval = Integer.valueOf(configProp.getProperty(PROP_ENGINE_EXPIRY_INTERVAL));
                // Update heart-beat every 60% of engine_expiry_interval
                if (engExpTime - currentTime > 400L * engExpInterval) {
                    long waitTime = (engExpTime - currentTime) - 400L * engExpInterval;
                    synchronized (mRunningMonitor) {
                        try {
                            mRunningMonitor.wait(waitTime);
                        } catch (InterruptedException e) {
                            mLogger.log(Level.WARNING, mMessages.getString("IEPSEHeartBeatThread.IEP_service_engine_heart_beat_thread_is_interrupted"), e);
                        }
                    }
                }
                if (!mRunning) {
                    return;
                }
                //upate engine expiration time with
                engExpTime = System.currentTimeMillis() + 1000L * engExpInterval;
                try {
                    Util.updateEngineLeaseExpiration(con, engineId, new Timestamp(engExpTime));
                    con.commit();
                } catch (Exception e) {
                    Util.rollback(con);
                }

                List<String> startedOrphanList = null;
                Token token = null;
                try {
                    // ACQUIRE TOKEN
                    token = new Token(con);

                    // Undeploy undeployed event processors whose engine is dead
                    // see IEPSEServiceUnitManager's shutdown()
                    List<String> undeployedOrphanList = Util.getUndeployedOrphanPlanList(con, engineId);
                    for (int i = 0; i < undeployedOrphanList.size(); i++) {
                        String instanceId = undeployedOrphanList.get(i);
                        String status = Util.getPlanStatus(con, instanceId);
                        if (status == null) {
                            continue;
                        }
                        QueryPlan queryPlan = Util.getPlanByInstanceId(con, configProp, instanceId);
                        queryPlan.undeploy(con);
                    }
                    Util.removePlanFromDatabase(con, undeployedOrphanList);

                    // Get those started event processors whose engine is dead
                    startedOrphanList = Util.getStartedOrphanPlanList(con, engineId);
                    for (int i = 0; i < startedOrphanList.size(); i++) {
                        String instanceId = startedOrphanList.get(i);
                        Util.setPlanStatus(con, instanceId, STATUS_DEPLOYED);
                    }
                    con.commit();
                } catch (Exception e) {
                    Util.rollback(con);
                } finally {
                    if (token != null) {
                        token.close();
                    }
                } // RELEASE TOKEN

                // Take over and start those started orphan event processors
                engine.start(startedOrphanList);

                Thread.yield();
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEHeartBeatThread.IEP_service_engine_heart_beat_thread_finished"));
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, mMessages.getString("IEPSEHeartBeatThread.IEP_service_engine_heart_beat_thread_failed"), e);
        } finally {
            Util.close(con);
            mStopped = true;
            synchronized(mStoppedMonitor) {
                mStoppedMonitor.notifyAll();
            }
            NDC.exit("Application", "IEPSE", "Task", "IEPSEHeartBeatThread.run");
        }
    }

    public void stopAndWait() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEHeartBeatThread.Ceasing_IEP_service_engine_heart_beat_thread"));
        }
        mRunning = false;
        synchronized (mRunningMonitor) {
            mRunningMonitor.notifyAll();
        }
        while (!mStopped) {
            synchronized(mStoppedMonitor) {
                try {
                    mStoppedMonitor.wait();
                } catch (InterruptedException e) {
                }   
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("IEPSEHeartBeatThread.Ceased_IEP_service_engine_heart_beat_thread_successfully"));
        }
    }
}
