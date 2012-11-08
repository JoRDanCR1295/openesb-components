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
 * @(#)DefaultIEPEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime;

import com.sun.jbi.engine.iep.core.runtime.change.OperatorPropertyChange;
import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeObject;
import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeSet;
import com.sun.jbi.engine.iep.core.runtime.client.pojo.ProfilerRoundTrip;
import com.sun.jbi.engine.iep.core.runtime.debugger.DebugProcessor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.Statement;
import com.sun.jbi.engine.iep.core.runtime.scheduler.RoundRobin;
import com.sun.jbi.engine.iep.core.runtime.scheduler.Scheduler;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Token;
import java.util.logging.Level;


public class DefaultIEPEngine implements OperatorConstants, IEPEngine {
    private static final Messages mMessages = Messages.getMessages(DefaultIEPEngine.class);

    private static DefaultIEPEngine mInstance;
    private Properties mConfigProp;
    private Map<String, Scheduler> mSchedulerMap;
    private Map<String,DebugProcessor> mDebugMap;

    private DefaultIEPEngine() {
        mSchedulerMap = new HashMap<String, Scheduler>();
    }
    
    // Called by com.sun.jbi.engine.iep.core.jbiadapter.IEPSEDeployer
    private QueryPlan getPlanByInstanceId(Connection con, String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler == null) {
            return Util.getPlanByInstanceId(con, mConfigProp, instanceId);
        }
        return scheduler.getQueryPlan();
    }
    // Called by management bean to get the same instance of plan as used by engine, instead of creating new 
    // instance of plan as per previous method.
    public QueryPlan getScheduledPlanByInstanceId(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler == null) {
            return null;
        }
        return scheduler.getQueryPlan();
    }
    
    public void init(Properties configProp) throws Exception {
        mConfigProp = configProp;
        if (RUNTIME_STYLE_EMBEDDED.equals(mConfigProp.getProperty(PROP_RUNTIME_STYLE))) {
            mConfigProp.setProperty(PROP_DB_SCHEMA, "APP");
        }
        boolean isSuccess = Util.initializeDB(mConfigProp);
        if (!isSuccess) {
            StringBuffer sb = new StringBuffer();
            sb.append(mMessages.getString("DefaultIEPEngine.Failed_to_initialize_IEP_Engine_instance"));
            sb.append("\n");
            sb.append(mMessages.getString("DefaultIEPEngine.1.ENSURE_DERBY_ORACLE_ETC"));
            sb.append("\n");
            sb.append(mMessages.getString("DefaultIEPEngine.2.The_configuration_properties_are_correct"));
            sb.append("\n");
            sb.append(Util.getConfigPropAsString(configProp));
            sb.append("\n");
            sb.append(mMessages.getString("DefaultIEPEngine.Configuration_retry"));
            sb.append("\n");
            throw new Exception(sb.toString());
        }
    }
    
    public boolean isInitialized() throws Exception {
        return mConfigProp != null;
    }

    public void setConfigProperties(Properties configProp) throws Exception {
        mConfigProp = configProp;
    }

    public Properties getConfigProperties() throws Exception {
        return mConfigProp;
    }
    
    public String getId() {
        return mConfigProp.getProperty(PROP_ENGINE_ID);
    }
    // All server instances are homogeneously competing to execute this method.
    // First come, first win.
    public void start(List<String> instanceIdList) throws Exception {
        String engineId = mConfigProp.getProperty(PROP_ENGINE_ID);
        Connection con = null;
        Token token = null;
        try {
            con = Util.getConnection(mConfigProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            for (int i = 0; i < instanceIdList.size(); i++) {
                String instanceId = instanceIdList.get(i);
                String status = Util.getPlanStatus(con, instanceId);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_has_status", new Object[]{instanceId, status});
                }    
                if (status == null || (status.equals(STATUS_STARTED) && !Util.isPlanOwner(con, engineId, instanceId))) {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_keeps_its_status", new Object[]{instanceId, status});
                    }    
                    continue;
                }
                // Two cases will end up here:
                // 1.The plan is not started yet. This happens during normal execution
                // 2.The plan is already started, and the engine is already the plan owner. This happens when
                //   the app. server is restarted after getting crashed/killed.
                Scheduler scheduler = mSchedulerMap.get(instanceId);
                if (scheduler == null) {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Starting_event_processor", instanceId);
                    }    
                    QueryPlan queryPlan = getPlanByInstanceId(con, instanceId);
                    Map<String, Object> schedulerProp = new HashMap<String, Object>();
                    schedulerProp.put(PROP_CONFIG_PROPERTIES, mConfigProp);
                    schedulerProp.put(PROP_TIME_UNIT, "1000L");
                    schedulerProp.put(PROP_QUERY_PLAN, queryPlan);
                    scheduler = new RoundRobin(schedulerProp);
                    mSchedulerMap.put(instanceId, scheduler);
                    scheduler.start();
                    Util.setPlanStatusAndEngineId(con, instanceId, STATUS_STARTED, engineId);
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_changes_its_status", new Object[]{instanceId, STATUS_STARTED});
                    }    
                } // else query-plan with instance-id = instanceId already started
            }
            con.commit();
        } catch (Exception e) {
            Util.rollback(con);
            throw e;
        } finally {
            if (token != null) {
                token.close();
            }
            Util.close(con);
        }
        // RELEASE TOKEN
    }
    public void start(String instanceId) throws Exception {
        ArrayList<String> instanceIdList = new ArrayList<String>();
        instanceIdList.add(instanceId);
        start(instanceIdList);
    }

    public boolean isScheduled(String instanceId) throws Exception {
        return mSchedulerMap.containsKey(instanceId);
    }
    
    // Executing server instance differs from other server instances
    public void stop(List<String> instanceIdList) throws Exception {
        Connection con = null;
        Token token = null;
        try {
            con = Util.getConnection(mConfigProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            for (int i = 0; i < instanceIdList.size(); i++) {
                String instanceId = instanceIdList.get(i);
                String status = Util.getPlanStatus(con, instanceId);
                if (status == null) {
                	//If status is null, it means that the plan was not found. This happens
                	//if the plan has already been removed by another instance in a cluster
                	//In that case, remove the instance 
                	mSchedulerMap.remove(instanceId);
                    continue;
                }
                
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_has_status", new Object[]{instanceId, status});
                }
                
                if (status.equals(STATUS_STOPPED) || status.equals(STATUS_UNDEPLOYED)) {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_keeps_its_status", new Object[]{instanceId, status});
                    }    
                    continue;
                } 
                // 1. Regardless whether this server instance is the executor,
                //    sets the plan status to stopped if not yet. Because
                //    the executing server instance maybe fake alive: it is dead
                //    but it is alive according the EMS_ENGINE table.
                // 2. User may shutdown a started service assembly in one shot. In
                //    that case, one iepse may execute both stop and undeploy before
                //    any other iepse executes stop.
                Util.setPlanStatus(con, instanceId, STATUS_STOPPED);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_changes_its_status", new Object[]{instanceId, STATUS_STOPPED});
                }    
                Scheduler scheduler = mSchedulerMap.get(instanceId);
                if (scheduler != null) {
                    scheduler.stop();
                    mSchedulerMap.remove(instanceId);
                }
                // else query-plan with instance-id = instanceId 
                // 1. is already stopped by me
                // 2. is executed by other server instance in the cluster
                   
            }
            con.commit();
        } catch (Exception e) {
            Util.rollback(con);
            throw e;
        } finally {
            if (token != null) {
                token.close();
            }
            Util.close(con);
        }        
        // RELEASE TOKEN
    }
    public void stop(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler != null) {
            scheduler.stop();
            mSchedulerMap.remove(instanceId);
        }
        // else query-plan with instance-id = instanceId already stopped
    }
    
    public void pause(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler != null) {
            scheduler.stop();
        } else {
            throw new Exception(mMessages.getString("DefaultIEPEngine.Event_processor_pause_failure", new Object[]{instanceId}));
        }
        
    }
    
    public void resume(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler != null) {
            scheduler.start();
        } else {
            throw new Exception(mMessages.getString("DefaultIEPEngine.Event_processor_resume_failure", new Object[]{instanceId}));
        }
                    
    }

    public void deploy(QueryPlanInfo queryPlanInfo) throws Exception {
        ArrayList<QueryPlanInfo> planInfoList = new ArrayList<QueryPlanInfo>();
        planInfoList.add(queryPlanInfo);
        deploy(planInfoList);
    }
    
    // All server instances are homogeneously competing to execute this method.
    // First come, first win.
    public void deploy(List<QueryPlanInfo> queryPlanInfoList) throws Exception {
        Connection con = null;
        Token token = null;
        List<QueryPlan> plans = new ArrayList<QueryPlan>();
        
        try {
            con = Util.getConnection(mConfigProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            Util.savePlanToDatabase(con, queryPlanInfoList);
            for (int i = 0; i < queryPlanInfoList.size(); i++) {
                QueryPlanInfo queryPlanInfo = queryPlanInfoList.get(i);
                String instanceId = queryPlanInfo.getInstanceId();
                String status = Util.getPlanStatus(con, instanceId);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_has_status", new Object[]{instanceId, status});
                }    
                // STATUS_STARTED: IEPSEServiceUnitManager two phase start:
                //     1st phase: init() set status to 'deployed'
                //     2nd phase: start() set status to 'started'
                // Since one iepse can execute both phase before any other iepse executes init(),
                // we have to test the case where status == 'deployed'.
                if (status == null || status.equals(STATUS_DEPLOYED) || status.equals(STATUS_STARTED)) {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_keeps_its_status", new Object[]{instanceId, status});
                    }    
                    continue;
                }
                QueryPlan queryPlan = getPlanByInstanceId(con, instanceId);
                //first deploy external resource
                //note we need to do this first in this loop
                //to avoid deadlock
                //we call deploy in a seperate loop
                //below, if we call it in same loop we might have a deadlock
                //and connection time out when deploying external
                //resources
                if(queryPlan != null) {
                    queryPlan.deployExternalResource();
                    plans.add(queryPlan);
                }
                
            }
            
            for(int i =0; i < plans.size(); i++) {
                QueryPlan queryPlan = plans.get(i);
                String instanceId = queryPlan.getInstanceId();
                queryPlan.deploy(con);
                
                
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Starting_event_processor", instanceId);
                }    
                
                Util.setPlanStatus(con, instanceId, STATUS_DEPLOYED);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_changes_its_status", new Object[]{instanceId, STATUS_DEPLOYED});
                }    
            }
            
            con.commit();
        } catch (Exception e) {
            Util.rollback(con);
            throw e;
        } finally {
            if (token != null) {
                token.close();
            }
            Util.close(con);
        }
        // RELEASE TOKEN
    }
    
    public void undeploy(String instanceId) throws Exception {
        ArrayList<String> instanceIdList = new ArrayList<String>();
        instanceIdList.add(instanceId);
        undeploy(instanceIdList);
    }
    
    // All server instances are homogeneously competing to execute this method.
    // First come, first win.
    public void undeploy(List<String> instanceIdList) throws Exception {
        Connection con = null;
        Token token = null;
        List<QueryPlan> plans = new ArrayList<QueryPlan>();
        
        try {
            con = Util.getConnection(mConfigProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            for (int i = 0; i < instanceIdList.size(); i++) {
                String instanceId = instanceIdList.get(i);
                String status = Util.getPlanStatus(con, instanceId);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_has_status", new Object[]{instanceId, status});
                }    
                if (status == null || status.equals(STATUS_UNDEPLOYED)) {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_keeps_its_status", new Object[]{instanceId, status});
                    }    
                    continue;
                }
                QueryPlan queryPlan = getPlanByInstanceId(con, instanceId);
                if(queryPlan != null) {
                    //first undeploy external resource
                    //note we need to do this first in this loop
                    //to avoid deadlock
                    //we call undeploy in a seperate loop
                    //below, if we call it in same loop we have a deadlock
                    //and connection time out when undeploying external
                    //resources
                    queryPlan.undeployExternalResource();
                    plans.add(queryPlan);
                }
            }
            
            for (int i = 0; i < plans.size(); i++) {
                QueryPlan queryPlan = plans.get(i);
                String instanceId = queryPlan.getInstanceId();
                queryPlan.undeploy(con);
                Util.setPlanStatus(con, instanceId, STATUS_UNDEPLOYED);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_changes_its_status", new Object[]{instanceId, STATUS_UNDEPLOYED});
                }    
            }
            
            con.commit();
        } catch (Exception e) {
            Util.rollback(con);
            throw e;
        } finally {
            if (token != null) {
                token.close();
            }
            Util.close(con);
        }
        // RELEASE TOKEN
    }
    
    private void undeployAll() throws Exception {
        List list = null;
        Connection con = null;
        try {
            con = Util.getConnection(mConfigProp);
            list = Util.getPlanList(con);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DefaultIEPEngine.Failed_to_undeploy_all_event_processors", e);
            return;
        } finally {
            Util.close(con);
        }
        if (list == null) {
            return;
        }
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processors_undeployed_id_instance-id_name");
        }    
        for (int i = 0, I = list.size(); i < I; i++) {
            String[] row = (String[])list.get(i);
            undeploy(row[1]);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.logOriginal(Level.FINE, "(" + row[0] + ", "+ row[1] + ", " + row[2] + ")");
            }    
        }
    }
    
    private void printContent(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        QueryPlan plan = null;
        if (scheduler != null) {
            plan = scheduler.getQueryPlan();
        } else {
            Connection con = null;
            try {
                con = Util.getConnection(mConfigProp);
                plan = getPlanByInstanceId(con, instanceId);
            } catch (Exception e) {
                return;
            } finally {
                Util.close(con);
            }
        }
        List opList = plan.getOperatorList();
        mMessages.println("DefaultIEPEngine.Event_processor_content", instanceId);
        for (int i = 0, I = opList.size(); i < I; i++) {
            Operator op = (Operator)opList.get(i);
            mMessages.printlnOriginal("\t" + op.getName() + ", " + op.getId());
        }
    }
    
    private void reportLocal(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler != null) {
            QueryPlan plan = scheduler.getQueryPlan();
            List opList = plan.getOperatorList();
            mMessages.println("DefaultIEPEngine.Event_processor_report", instanceId);
            for (int i = 0, I = opList.size(); i < I; i++) {
                Operator op = (Operator)opList.get(i);
                if (op.hasReport()) {
                    mMessages.printlnOriginal("\t" + op.getName());
                    mMessages.printlnOriginal(op.getReport(2));
                }
            }
        }
    }

    private void resetReportLocal(String instanceId) throws Exception {
        Scheduler scheduler = mSchedulerMap.get(instanceId);
        if (scheduler != null) {
            QueryPlan plan = scheduler.getQueryPlan();
            List opList = plan.getOperatorList();
            for (int i = 0, I = opList.size(); i < I; i++) {
                Operator op = (Operator)opList.get(i);
                op.resetReport();
            }
        }
    }

    private void printDeployed() throws Exception {
        Connection con = null;
        try {
            con = Util.getConnection(mConfigProp);
            List list = Util.getPlanList(con);
            mMessages.println("DefaultIEPEngine.Event_processors_deployed_id_instance-id_name");
            for (int i = 0, I = list.size(); i < I; i++) {
                String[] row = (String[])list.get(i);
                mMessages.printlnOriginal("(" + row[0] + ", "+ row[1] + ", " + row[2] + ")");
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DefaultIEPEngine.printDeployed_failed", e);
            return;
        } finally {
            Util.close(con);
        }
    }
    
    private void printLocalStarted() throws Exception {
        Iterator iterator = mSchedulerMap.keySet().iterator();
        mMessages.println("DefaultIEPEngine.Event_processors_started");
        while (iterator.hasNext()) {
            String instanceId = (String)iterator.next();
            mMessages.printlnOriginal(instanceId);
        }
    }    
    
    public void startAll() throws Exception {
        ArrayList<String> list = new ArrayList<String>(mSchedulerMap.keySet());
        for (int i = 0, I = list.size(); i < I; i++) {
            String instanceId = list.get(i);
            start(instanceId);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_starts_successfully", instanceId); 
            }    
        }
    }

    public void stopAll() throws Exception {
        ArrayList<String> list = new ArrayList<String>(mSchedulerMap.keySet());
        for (int i = 0, I = list.size(); i < I; i++) {
            String instanceId =list.get(i);
            stop(instanceId);
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "DefaultIEPEngine.Event_processor_stops_successfully", instanceId); 
            }    
        }
    }
    
    public void unregister() throws Exception {
    }
    
    public void quit(boolean clean) throws Exception {    
        stopAll();
        if (clean) {
            destroy();
        }
    }
    
    public void destroy() throws Exception {
        undeployAll();
        Util.cleanDB(mConfigProp);
    }
    
    // return a list of deployed instanceIds
    public String[] listDeployed() throws Exception {
        List<String> list = new ArrayList<String>();
        Connection con = null;
        try {
            con = Util.getConnection(mConfigProp);
            List<String[]> planList = Util.getPlanList(con);
            for (int i = 0, I = planList.size(); i < I; i++) {
                // row: (id, instance-id, name)
                String[] row = planList.get(i);
                list.add(row[1]);
            }
        } catch (Exception e) {
            mMessages.log(Level.WARNING, "DefaultIEPEngine.Failed_to_list_deployed_event_processors", e);
        } finally {
            Util.close(con);
        }
        return (String[])list.toArray(new String[0]);
    }
    
    private static String[] parseCommand(String command) {
        StringTokenizer st = new StringTokenizer(command, " ");
        List<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            list.add(st.nextToken());
        }
        return (String[])list.toArray(new String[0]);
    }
    
    private void sql(String command) {
        Connection con = null;
        Statement s = null;
        try {
            con = Util.getConnection(mConfigProp);
            s = con.createStatement();
            s.execute(command);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DefaultIEPEngine.Failed_executing", command, e);
        } finally {
            Util.close(s);
            Util.close(con);
        }
    }
    
    private void roundTip(String instanceId, String streamInput, String streamOutput, String inputTemplateFile, int inBatchSize, int outBatchSize, int totalBatches, String logFile) {
        Properties config = new Properties(mConfigProp);
        config.put("InstanceId", instanceId);
        config.put("StreamInput", streamInput);
        config.put("StreamOutput", streamOutput);
        config.put("InputBatchSize", inBatchSize);
        config.put("OutputBatchSize", outBatchSize);
        config.put("TotalBatches", totalBatches);
        config.put("InputTemplateFile", inputTemplateFile);
        config.put("LogFile", logFile);
        ProfilerRoundTrip prt = new ProfilerRoundTrip(config);
        try {
            prt.run();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
        
    public static void main(String args[]) {
        StringBuffer menu = new StringBuffer();
        menu.append("\n>action? ");
        menu.append("(deploy instance-id plan-file-path)");
        menu.append("(undeploy instance-id)");
        menu.append("|(undeploy-all)");
        menu.append("|(start instance-id)");
        menu.append("|(stop instance-id)");
        menu.append("|(print-content instance-id)");
        menu.append("|(report-local instance-id)");
        menu.append("|(reset-report-local instance-id)");
        menu.append("|(print-deployed)");
        menu.append("|(print-local-started)");
        menu.append("|(sql command)");
        menu.append("|(round-trip instance-id stream-input stream-output input-template-file input-batch-size output-batch-size total-batches log-file)");
        menu.append("|(quit)");
        menu.append("|(clean-quit)");
        BufferedReader userIn = null;
        try {
            mInstance = getInstance();
            mInstance.init(System.getProperties());
            userIn = new BufferedReader(new InputStreamReader(System.in));
            while (true) {
                mMessages.printlnOriginal(menu.toString());
                mMessages.printOriginal(">>");
                String ans = userIn.readLine();
                if (ans == null) {
                    break;
                }
                if (ans.trim().equals("")) {
                    continue;
                }
                String[] cmd = parseCommand(ans);
                if ("deploy".startsWith(cmd[0])) {
                    mInstance.deploy(new QueryPlanInfo(cmd[1], cmd[2]));
                } else if ("undeploy".startsWith(cmd[0])) {
                    mInstance.undeploy(cmd[1]);
                } else if ("undeploy-all".startsWith(cmd[0])) {
                    mInstance.undeployAll();
                } else if ("start".startsWith(cmd[0])) {
                    mInstance.start(cmd[1]);
                } else if ("stop".startsWith(cmd[0])) {
                    mInstance.stop(cmd[1]);
                } else if ("print-content".startsWith(cmd[0])) {
                    mInstance.printContent(cmd[1]);
                } else if ("report-local".startsWith(cmd[0])) {
                    mInstance.reportLocal(cmd[1]);
                } else if ("reset-report-local".startsWith(cmd[0])) {
                    mInstance.resetReportLocal(cmd[1]);
                } else if ("print-deployed".startsWith(cmd[0])) {
                    mInstance.printDeployed();
                } else if ("print-local-started".startsWith(cmd[0])) {
                    mInstance.printLocalStarted();
                } else if ("sql".startsWith(cmd[0])) {
                    StringBuffer sb = new StringBuffer();
                    for (int i = 1; i < cmd.length; i++) {
                        sb.append(cmd[i] + " ");
                    }
                    mInstance.sql(sb.toString());
                } else if ("round-trip".startsWith(cmd[0])) {
                    String instanceId = cmd[1];
                    String streamInput = cmd[2];
                    String streamOutput = cmd[3];
                    String inputTemplateFile = cmd[4];
                    int inputBatchSize = Integer.parseInt(cmd[5]);
                    int outputBatchSize = Integer.parseInt(cmd[6]);
                    int totalBatches = Integer.parseInt(cmd[7]);
                    String logFile = cmd[8];
                    mInstance.roundTip(instanceId, streamInput, streamOutput, inputTemplateFile, inputBatchSize, outputBatchSize, totalBatches, logFile);
                } else if ("quit".startsWith(cmd[0])) {
                    mInstance.quit(false);
                    System.exit(0);
                } else if ("clean-quit".startsWith(cmd[0])) {
                    mInstance.quit(true);
                    System.exit(0);
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DefaultIEPEngine.main_failure", e);
        } finally {
            try {
                if (userIn != null) {
                    userIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "DefaultIEPEngine.Closing_userIn_failed", e1);
            }
        }
    }

    public static DefaultIEPEngine getInstance() throws Exception {
        if (mInstance == null) {
            createInstance();
        }
        return mInstance;
    }
    
    private static synchronized void createInstance() throws Exception 
    {
        if (mInstance != null) {
            return;
        }
        try {
            mInstance = new DefaultIEPEngine();
        } catch (Exception e) {
        }
    }
    
    
    // For debugging purposes.
    private Scheduler mUnsetScheduler = null;
    public void setIEPInstanceForDebugging(String planId){
        if(mDebugMap == null){
            mDebugMap = new HashMap<String,DebugProcessor>();
        }
        mUnsetScheduler = mSchedulerMap.remove(planId);
        mUnsetScheduler.stop();
        mUnsetScheduler.getQueryPlan();
        QueryPlan queryPlan = mUnsetScheduler.getQueryPlan();
        Map<String, Object> schedulerProp = new HashMap<String, Object>();
        schedulerProp.put(PROP_CONFIG_PROPERTIES, mConfigProp);
        schedulerProp.put(PROP_TIME_UNIT, "1000L");
        schedulerProp.put(PROP_QUERY_PLAN, queryPlan);
        DebugProcessor dp = new DebugProcessor(schedulerProp);
        mDebugMap.put(planId, dp);
        
    }
    public void unsetIEPInstanceForDebugging(String planId) {
        mDebugMap.remove(planId);
        mSchedulerMap.put(planId, mUnsetScheduler);
        mUnsetScheduler.start();
    }
    public DebugProcessor getDebugProcessor(String planId){
        return mDebugMap.get(planId);
    }
    //
    
    public String scheduleOperatorPropertyChange(String planId,String operatorName,
            String propertyName, Object propertyValue) throws Exception {
        String result = null ;
        Scheduler scheduler = mSchedulerMap.get(planId);
        OperatorPropertyChange oprC = new OperatorPropertyChange(planId,operatorName,propertyName,propertyValue);
        result =  scheduler.scheduleOperatorPropertyChange(oprC);
        return result;
    }

    /// Managing runtime changes  can be moved to new ChangeHandler class.
    private RuntimeChangeSet rSet = null;
    public void applyChangeSet() {
        Iterator<Scheduler> iter = mSchedulerMap.values().iterator();
        while (iter.hasNext()){
            Scheduler sc = iter.next();
            sc.scheduleApplyChanges();
        }
    }

    public void ignoreChangeSet() {
        Iterator<Scheduler> iter = mSchedulerMap.values().iterator();
        while (iter.hasNext()){
            Scheduler sc = iter.next();
            sc.ignoreChangeSet();
        }
    }

    public void startNewChangeSet() {
        rSet = new RuntimeChangeSet();
    }

    public String addNewChange(RuntimeChangeObject obj) throws Exception {
        String result = null;
        rSet.addAChange(obj);
        return result;
    }
}