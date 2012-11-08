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
package com.sun.jbi.engine.iep.core.runtime.debugger;

import com.sun.jbi.engine.iep.core.runtime.data.access.DataAccessTabularInfo;
import com.sun.jbi.engine.iep.core.runtime.data.access.OperatorDA;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorComparator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

/**
 *
 * @author rdwivedi
 */
public class DebugProcessor implements OperatorConstants, DebugConstants {

    private static final Messages mMessages = Messages.getMessages(DebugProcessor.class);
    private Properties mConfigProp;
    private QueryPlan mQueryPlan;
    private Map<String, Object> mProcessingState;
    private Properties mPlanPs;
    private Map<String, Object> mOperatorPs;
    private Timestamp mTimeStamp = null;
    Connection mCon = null;
    private List<String> mListOfPauseBefore = null;
    private List<Operator> mOperatorsList = null;
    private int executedOperatorIndex = 0;
    private Operator latestExecutedOperator = null;
    private String currentPausedBefore = null;
    private boolean debugLevelComplete = false;

    public DebugProcessor(Map<String, Object> prop) {
        mConfigProp = (Properties) prop.get(PROP_CONFIG_PROPERTIES);
        mQueryPlan = (QueryPlan) prop.get(PROP_QUERY_PLAN);
        mListOfPauseBefore = new ArrayList<String>();
        mOperatorsList = mQueryPlan.getOperatorList();
        Collections.sort(mOperatorsList, OperatorComparator.getInstance());
        
    }

    public void putPauseBefore(String oprName) {
        if (!mListOfPauseBefore.contains(oprName)) {
            mListOfPauseBefore.add(oprName);
        }
    }

    public void removePauseBefore(String oprName) {
        if (mListOfPauseBefore.contains(oprName)) {
            mListOfPauseBefore.remove(oprName);
        }
    }
    @SuppressWarnings("unchecked")
    //mOperatorPs = (Map<String, Object>)mProcessingState.get(PS_OPERATOR_STATE);
    public void startOfCycle() throws Exception {
        //Start the cycle of iep process.
        
        mCon = Util.getConnection(mConfigProp);
        //mCon.setAutoCommit(false);
        mProcessingState = Util.getProcessingState(mCon, mQueryPlan.getId());
        mPlanPs = (Properties) mProcessingState.get(PS_PLAN_STATE);
        mOperatorPs = (Map<String, Object>) mProcessingState.get(PS_OPERATOR_STATE);
        String prevTimestampToProcess = mPlanPs.getProperty(PS_PREV_TIMESTAMP_TO_PROCESS);
        mTimeStamp = Util.nextTimestampToCheck(mCon, mQueryPlan);
        //mOperatorsList = mQueryPlan.getOperatorList();
        //Collections.sort(mOperatorsList, OperatorComparator.getInstance());
        for (Operator op : mOperatorsList) {
            op.setRuntimeConnection(mCon);
            Properties opPs = (Properties) mOperatorPs.get(op.getId());
            if (opPs == null) {
                continue;
            }
            opPs.setProperty(PS_PREV_TIMESTAMP_TO_PROCESS, prevTimestampToProcess);
            op.initProcessingState(opPs);
        }

    }

    private void endOfOneCompleteCycle() {

        String instanceId = mQueryPlan.getInstanceId();
        List<Operator> opList = null;
        mPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_CHECK, "" + mTimeStamp.getTime());
        mPlanPs.setProperty(PS_PREV_TIMESTAMP_TO_PROCESS, "" + mTimeStamp.getTime());
        opList = mQueryPlan.getOperatorList();
        Collections.sort(opList, OperatorComparator.getInstance());
        for (Operator op : opList) {
            mOperatorPs.put(op.getId(), op.getProcessingState());
        }
        try {
            Util.updateProcessingState(mCon, mQueryPlan.getId(), mProcessingState);
        //mCon.commit();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DebugProcessor.update_state_failed", instanceId, e);
        } finally {
            for (Operator op : opList) {
                op.unsetRuntimeConnection();
            }
            Util.close(mCon);
            mCon = null;
        }
        executedOperatorIndex = 0;
        currentPausedBefore = null;
    }

    public String next() throws Exception {
        if (executedOperatorIndex == 0) {
            startOfCycle();
        }
        Operator opr = mOperatorsList.get(executedOperatorIndex++);
        executeOperator(opr);
        latestExecutedOperator = opr;
        if (executedOperatorIndex == mOperatorsList.size()) {
            endOfOneCompleteCycle();
            executedOperatorIndex = 0;
        }
        String str = mMessages.getString("DebugProcessor.Last_executed_operator_is", opr.getName());
        return str;
    }

    public String completeCycle() throws Exception {
        if (executedOperatorIndex == 0) {
            next();
        }
        while (executedOperatorIndex > 0) {
            next();
        }
        return latestExecutedOperator.getName();
    }

    public String nextCompleteCyclesOfExecution(int noOfCyclesToExecute) throws Exception {
        long sT = System.currentTimeMillis();
        for (int i = 0; i < noOfCyclesToExecute; i++) {
            completeCycle();
        }
        String str = mMessages.getString("DebugProcessor.NoOfCyclesCompleted", noOfCyclesToExecute,(System.currentTimeMillis() - sT));
        return str;
        //"No of Cycles Completed = " + noOfCyclesToExecute  + " in approximate " + (System.currentTimeMillis() - sT) + " milliseconds";
    }

    public String nextCompleteCyclesOfExecutionForTime(long milliSeconds) throws Exception {
        long sT = System.currentTimeMillis();
        int counter = 0;
        while ((System.currentTimeMillis() - sT) <= milliSeconds) {
            completeCycle();
            counter++;
        }
        String str = mMessages.getString("DebugProcessor.NoOfCyclesCompleted", counter,(System.currentTimeMillis() - sT));
        return str;
        //"No of Cycles Completed = " + counter + " in approximate " + (System.currentTimeMillis() - sT) + " milliseconds";
    }

    public String nextTillPause() throws Exception {

        if (mListOfPauseBefore.isEmpty()) {
            return (mMessages.getString("DebugProcessor.NoPause_Exception"));
        } else {
            
            Operator opr = mOperatorsList.get(executedOperatorIndex);
            if (currentPausedBefore != null && currentPausedBefore.equals(opr.getName())) {
                next();
                opr = mOperatorsList.get(executedOperatorIndex);
                currentPausedBefore = null;
            }
            while (opr != null && !mListOfPauseBefore.contains(opr.getName())) {
                next();
                opr = mOperatorsList.get(executedOperatorIndex);
            }
            currentPausedBefore = opr.getName();
            return mMessages.getString("DebugProcessor.Paused_Before",opr.getName());
        }
    }

    private void executeOperator(Operator opr) {

        opr.operate(mTimeStamp);

    }

    public DebugInfo getDebugInfoForCurrentExecutionCycle(String oprName) {
        DebugInfo info = null;
        Operator opr = latestExecutedOperator;
        if (opr == null) {
            
            info = new DebugInfo(mMessages.getString("DebugProcessor.No_Operator_Executed"));
            info.addDebugInfo(mMessages.getString("DebugProcessor.Debug_Error"), mMessages.getString("DebugProcessor.No_Operator_Executed"));
        } else {
            if (oprName != null && oprName.length() > 0 && !oprName.equals(latestExecutedOperator.getName())) {
                opr = findOperator(oprName);
            }
            if (opr == null) {
                info = new DebugInfo(mMessages.getString("DebugProcessor.No_Operator"));
                info.addDebugInfo(mMessages.getString("DebugProcessor.Name") + oprName, "null");
            } else {
                info = new DebugInfo(opr.getName());
                info.addDebugInfo(mMessages.getString("DebugProcessor.Name"), opr.getName());
                info.addDebugInfo(mMessages.getString("DebugProcessor.TableName"), opr.getQueueName());
                try {
                    info.addDebugInfo(mMessages.getString("DebugProcessor.Data_For_Next_Cycle"),
                            getValidDataAvailableForNextCycle(opr));
                } catch (Exception e) {
                    info.addDebugInfo(mMessages.getString("DebugProcessor.Exception_Query_Table"),
                            e.getMessage());
                }
            }
        }
        return info;
    }

    private Operator findOperator(String oprName) {
        Operator opr = null;
        for (Operator op : mOperatorsList) {
            if (op.getName().equals(oprName)) {
                opr = op;
            }
        }
        return opr;
    }

    private DataAccessTabularInfo getValidDataAvailableForNextCycle(Operator opr) throws Exception {
        DataAccessTabularInfo info = null;
        boolean localConnection = false;
        try {
            if (mCon == null) {
                mCon = Util.getConnection(mConfigProp);
                localConnection = true;
            }
           OperatorDA da = new OperatorDA();
           da.execute(opr, mCon);
           info = da.getTabularData();
           // info = getResultOfLastExecution(opr, mCon);
            if (info == null) {
                if (debugLevelComplete) {
                    info = getCurrentRows(opr);
                } 
            }

        } catch (Exception e) {
            try {
                e.printStackTrace();
                mMessages.log(Level.SEVERE, "DebugProcessor.DataAccess_Error", e);
                info = getCurrentRows(opr);
            } catch (Exception f) {
                mMessages.log(Level.SEVERE, "DebugProcessor.DataAccess_Error", f);
                throw f;
            }
        } finally {
            if (localConnection) {
                if (mCon != null) {
                    Util.close(mCon);
                    mCon = null;
                }
            }
        }
        return info;
    }

    private DataAccessTabularInfo getCurrentRows(Operator opr) throws Exception {
        String tabName = opr.getQueueName();
        String qry = "Select * from " + tabName;//+ " where EMS_TAG='+'";
        //String qry2 = "Select * from "+ tabName + " where EMS_TAG='+'";

        Statement stmt = null;
        ResultSet rs = null;
        DataAccessTabularInfo dInfo = new DataAccessTabularInfo();
        try {
            stmt = mCon.createStatement();
            rs = stmt.executeQuery(qry);
            ResultSetMetaData md = rs.getMetaData();
            int cCount = md.getColumnCount();
            ArrayList<String> cList = new ArrayList<String>();
            for (int i = 1; i <= cCount; i++) {
                cList.add(md.getColumnName(i));
            }
            dInfo.setColumns(cList);
            while (rs.next()) {
                ArrayList<Object> rList = new ArrayList<Object>();
                for (int i = 1; i <= cCount; i++) {
                    Object val = rs.getObject(i);
                    rList.add(val);
                }
                dInfo.addDataRow(rList);
            }
        } catch (SQLException e) {
            throw e;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (rs != null) {
                rs.close();
            }
        }
        return dInfo;
    }

   

    private String setDebugLevel(String comp) {
        if (comp.equalsIgnoreCase("complete")) {
            this.debugLevelComplete = true;
            return mMessages.getString("DebugProcessor.Debug_Level_Complete");
        } else {
            this.debugLevelComplete = false;
            return mMessages.getString("DebugProcessor.Debug_Level_ShowNew");
        }

    }

    // Execute Debug commands
    public String execute(String cmd, String[] params) throws Exception {
        String result = null;
        if (cmd == null || cmd.length() <= 0) {
            result = mMessages.getString("DebugProcessor.No_Command");
        }

        // executions
        if (cmd.equalsIgnoreCase("setDebugLevel")||cmd.equalsIgnoreCase("sdl")) {
            if (params != null && params.length > 1) {
                String str = params[0];

                result = setDebugLevel(str);
            } else {
                result = mMessages.getString("DebugProcessor.Not_Enough_Param",cmd)
                + mMessages.getString("DebugProcessor.Params_Req","'complete or showNew'");
                
            }
        }
        if (cmd.equalsIgnoreCase("next")||cmd.equalsIgnoreCase("n")) {
            result = next();
        }
        if (cmd.equalsIgnoreCase("completeCycle")||cmd.equalsIgnoreCase("cc")) {
            result = completeCycle();
        }
        if (cmd.equalsIgnoreCase("nextCompleteCycles")||cmd.equalsIgnoreCase("ncc")) {
            if (params != null && params.length > 1) {
                String str = params[0];
                int noOfTimes = Integer.parseInt(str.trim());
                result = nextCompleteCyclesOfExecution(noOfTimes);
            } else {
                result = mMessages.getString("DebugProcessor.Not_Enough_Param",cmd) +
                        mMessages.getString("DebugProcessor.Params_Req","noOfTimes as integer");
            }
        }
        if (cmd.equalsIgnoreCase("nextCompleteCyclesForTimePeriod")||cmd.equalsIgnoreCase("nct")) {
            if (params != null && params.length > 1) {
                String str = params[0];
                long time = Long.parseLong(str.trim());
                result = nextCompleteCyclesOfExecutionForTime(time);
            } else {
                result = mMessages.getString("DebugProcessor.Not_Enough_Param",cmd) +
                        mMessages.getString("DebugProcessor.Params_Req"," time in milliseconds(long) ");
            }
        }

        if (cmd.equalsIgnoreCase("nextTillPause")||cmd.equalsIgnoreCase("ntp")) {
            result = nextTillPause();
        }
        if (cmd.equalsIgnoreCase("nextOperator")||cmd.equalsIgnoreCase("no")) {
            result = "";
            Operator opr = mOperatorsList.get(executedOperatorIndex);
            if (opr == null) {
                result = mMessages.getString("DebugProcessor.No_Operator");
            } else {
                result = opr.getName();
            }
        }
        
        if (cmd.equalsIgnoreCase("listOperators")||cmd.equalsIgnoreCase("lo")) {
            result = "";
            for(int i = 0 ; i < mOperatorsList.size();i++){
                Operator opr = mOperatorsList.get(i);
                if(i<executedOperatorIndex) {
                    result = result + "   " + opr.getName()+"\n";
                } else {
                    result = result + "** " + opr.getName()+"\n";
                }
            }
        }
        if (cmd.equalsIgnoreCase("listOperatorProperties")||cmd.equalsIgnoreCase("lop")) {
            result = "";
            
            String str = mOperatorsList.get((executedOperatorIndex==0)?0:executedOperatorIndex-1).getName();
            if (params != null && params.length > 1) {
                str = params[0];
            }
            Operator opr = findOperator(str);
            if (opr == null) {
                result = mMessages.getString("DebugProcessor.Opr_Name_NotFound",str);
            } else {
            Map m = opr.getOperatorProperties();
            Iterator iter = m.keySet().iterator();
            while(iter.hasNext()) {
                Object k = iter.next();
                Object val = m.get(k);
                result = result + "["+k.toString()+"]\t =" + val.toString()+"\n";
            }
            }
        }
        if (cmd.equalsIgnoreCase("addPauseBefore")||cmd.equalsIgnoreCase("apb")) {
            if (params != null && params.length > 1) {
                String str = params[0];
                Operator opr = findOperator(str);
                if (opr != null) {
                    putPauseBefore(str);
                    result = mMessages.getString("DebugProcessor.Pause_Before_Added",str);
                } else {
                    result = mMessages.getString("DebugProcessor.Opr_Name_NotFound",str);
                    
                }

            } else {
                result = mMessages.getString("DebugProcessor.Not_Enough_Param",cmd) +
                        mMessages.getString("DebugProcessor.Params_Req"," Name of an operator "); 
                        
            }
        }

        if (cmd.equalsIgnoreCase("removePauseBefore")||cmd.equalsIgnoreCase("rpb")) {
            if (params != null && params.length > 1) {
                String str = params[0];
                Operator opr = findOperator(str);
                if (opr != null) {
                    removePauseBefore(str);
                    result = mMessages.getString("DebugProcessor.Pause_Before_Removed",str);
                } else {
                    result = mMessages.getString("DebugProcessor.Opr_Name_NotFound",str);
                }

            } else {
                result =  mMessages.getString("DebugProcessor.Not_Enough_Param",cmd) +
                        mMessages.getString("DebugProcessor.Params_Req"," Name of an operator "); 
            }
        }

        return result;

    }

    
}
