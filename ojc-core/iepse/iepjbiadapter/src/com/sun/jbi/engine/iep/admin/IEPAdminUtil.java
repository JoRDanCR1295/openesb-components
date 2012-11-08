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

package com.sun.jbi.engine.iep.admin;

import com.sun.jbi.engine.iep.ExtendedComponentContext;
import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.debugger.DebugInfo;
import com.sun.jbi.engine.iep.core.runtime.debugger.DebugProcessor;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorHelper;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.SaveStream;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;

import java.sql.Connection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author rdwivedi
 */
public class IEPAdminUtil {
    
    

    private static final Messages mMessages = Messages.getMessages(IEPAdminUtil.class);
    private static final Logger mLogger = Messages.getLogger(IEPAdminUtil.class);
    private ExtendedComponentContext mCtx = null; 
    private static String SUCCESS = "success";
    private static String FAILED = "failed";
    
    public IEPAdminUtil() {
        
    }
    
    public IEPAdminUtil(ExtendedComponentContext ctx){
        mCtx = ctx;
    }
    public String[] getListOfOperators(String planName) {
        int i = 0;
        String[] str = null;
        QueryPlan plan = getQueryPlan(planName);
        if( plan != null ) {
        List l = plan.getOperatorList();
        str = new String[l.size()];
        for(Operator opr: plan.getOperatorList()){
            str[i++] = opr.getName();
        }
        }
        return str;
    }
    public String[] getListOfOperators(QueryPlan plan) {
        int i = 0;
        String[] str  = null;
        //QueryPlan plan = getQueryPlan(planName);
        if(plan != null) {
        List l = plan.getOperatorList();
        str = new String[l.size()];
        for(Operator opr: plan.getOperatorList()){
            str[i++] = opr.getName();
        }
        }
        return str;
    }
    
    public Map<String,Object> getAdministrablePropertiesForOperator(String planName , String oprName) {
        Map<String,Object> map = null;
        //
        QueryPlan plan = getQueryPlan(planName);
        if(plan== null) {
            return null;
        } 
        Operator opr = plan.getOperatorByName(oprName);
        if(opr == null) {
            return null;
        } 
        map = opr.getAdministrableProperties();
        return map;
    }
    public Map<String,Object> getAdministrablePropertiesForOperator(QueryPlan plan , String oprName) {
        Map<String,Object> map = null;
        //
        //QueryPlan plan = getQueryPlan(planName);
        if(plan== null) {
            return null;
        }
        Operator opr = plan.getOperatorByName(oprName);
        if(opr == null) {
            return null;
        } 
        map = opr.getAdministrableProperties();
        return map;
    }
    private QueryPlan getQueryPlan(String planName) {
        
        //DeploymentRecord rec =  mCtx.getDeploymentTable().getRecordByDeployName(planName);
        QueryPlan plan = null;
        try {
            plan = mCtx.getEngine().getScheduledPlanByInstanceId(planName);
        } catch(Exception e){
            //TODO  handle exception.
        }
        return plan;
        
    }
    
    
    public String setManageablePropertyValue(String planName, String oprName,
            String propName, Object value) throws Exception  {
        /**
        QueryPlan plan = getQueryPlan(planName) ;
        Operator opr = plan.getOperatorByName(oprName);
        opr.setAdministrableProperty(propName,value);
         */
        IEPEngine engine = mCtx.getEngine();
        return engine.scheduleOperatorPropertyChange(planName,oprName,propName,value);
    }
    
    public String setIEPForDebugging(String planName) {
        // check if plan with plan name exists.
        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
            String errMsg = mMessages.getString("IEPAdminUtil.Plan_not_found", new Object[]{planName});
            return FAILED + " " + errMsg;
        }
        mCtx.getEngine().setIEPInstanceForDebugging(planName);
        return SUCCESS;
    }
    public String unsetIEPForDebugging(String planName) {
        //QueryPlan plan = getQueryPlan(planName) ;
        //if(plan== null){
        //    return FAILED+": Plan " + planName + " not found.";
        //}
        mCtx.getEngine().unsetIEPInstanceForDebugging(planName);
        return SUCCESS;
    }
    public String executeNext(String planName) throws Exception{
        DebugProcessor dp = mCtx.getEngine().getDebugProcessor(planName);
        return dp.next();
    }
    
    public String getCurrentDebugInfo(String planName,String  oprName) throws Exception {
        
        String result = null;
        //QueryPlan plan = getQueryPlan(planName) ;
        //if(plan== null){
        //    return "Plan " + planName + " not found.";
       //}
        DebugProcessor dp = mCtx.getEngine().getDebugProcessor(planName);
        if(dp== null){
            result = mMessages.getString("IEPAdminUtil.Plan_not_found", new Object[]{planName});
        } else {
            DebugInfo info = dp.getDebugInfoForCurrentExecutionCycle(oprName);
            if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("IEPAdminUtil.Debug_Info", new Object[]{info.getString()}));
                }
            result = info.getString();
        }
        return  result;
        
    }
    
    public String executeDebugCommand(String cmd , String planName , String[] args) throws Exception {
        
        try {
        String result = null;
        //QueryPlan plan = getQueryPlan(planName) ;
        //if(plan== null){
        //    return "Plan " + planName + " not found.";
        //}
        DebugProcessor dp = mCtx.getEngine().getDebugProcessor(planName);
        if(dp== null){
            result = mMessages.getString("IEPAdminUtil.Plan_not_found", new Object[]{planName});
        } else {
            result =  dp.execute(cmd,args);
        }
        return result;
        } catch(Exception e){
            //mLogger.log(Level.SEVERE, "Error", e);
            throw e;
        }
    }
    // Methods for Data Access 
    public String setDataAccessEnabled(String planName, String oprName, String extTableName) {
        
        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
            return FAILED;
        }
        Operator opr = plan.getOperatorByName(oprName);
        opr.setDataAccessEnabled(true);
        opr.setExtTable(extTableName);
        return SUCCESS;
    }

    public String disableDataAccess(String planName, String oprName) {
        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
            return FAILED;
        }
        Operator opr = plan.getOperatorByName(oprName);
        opr.setDataAccessEnabled(false);
        return SUCCESS;
    }
     public String enableSaveStream(String planName, String operatorName , String jndi, String tableName, String isGlobal) {
        String result = null;

        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
            return FAILED;
        }
        IEPEngine engine = mCtx.getEngine();
        String instanceId = plan.getInstanceId();
        if(instanceId != null) {

            try {
                //stop the scheduler for instance
                engine.pause(instanceId);

                //add a new save stream to given input stream
                SaveStream saveStream = OperatorHelper.getDefault().createAndAddSaveStream(operatorName, plan, jndi, tableName,isGlobal);
                if(saveStream != null) {
                    result = saveStream.getName();
                    
                    saveStream.deployExternalResource();
//                  call deploy on save stream to create tables
                    saveStream.deploy(Util.getConnection(mCtx.getConfigProperties()));
                }

                

                //restart the scheduler
                engine.resume(instanceId);
            } catch(Exception ex) {
                result = ex.getMessage();
            }

        }
        return result;
        
    }
    public String removeSaveStream(String planName, String saveStreamName) {
        String result = null;

        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
            return FAILED;
        }
        IEPEngine engine = mCtx.getEngine();
        String instanceId = plan.getInstanceId();
        if(instanceId != null) {

            try {
                //stop the scheduler for instance
                engine.pause(instanceId);
                
                //get saveStream
                Operator saveStream = plan.getOperatorByName(saveStreamName);
                if(saveStream != null) {
                    
                    saveStream.undeployExternalResource();
                    //call undeploy on save stream to create tables
                    saveStream.undeploy(Util.getConnection(mCtx.getConfigProperties()));
                }
                
                //remove save stream from given input stream
                boolean isRemoved = OperatorHelper.getDefault().removeSaveStream(saveStreamName, plan);
                result = Boolean.toString(isRemoved);

                
                //restart the scheduler
                engine.resume(instanceId);
            
            } catch(Exception ex) {
                result = ex.getMessage();
            }
        }

        return result;
    }
    public String startNewChangeSet() {
        String result = null;
        IEPEngine engine = mCtx.getEngine();
        engine.startNewChangeSet();
        return result;
    }

    public String applyChangeSet() throws Exception {
        String result = null;
        IEPEngine engine = mCtx.getEngine();
        engine.applyChangeSet();
        return result;
    }
    public String ignoreChangeSet() {
        String result = null;
        IEPEngine engine = mCtx.getEngine();
        engine.ignoreChangeSet();
        return result;
    }
    
    public String exportPlan(String planName, String planVersion) throws Exception {
        String result = null;
        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
//          log message and rethrow exception
            String errMsg = mMessages.getString("IEPAdminUtil.Plan_not_found", new Object[]{planName});
            mLogger.log(Level.SEVERE, errMsg);
            throw new Exception(FAILED + " " + errMsg);
        }
        
        String instanceId = plan.getInstanceId();
        if(instanceId != null) {
            Connection con = Util.getConnection(mCtx.getConfigProperties());
            result = Util.getPlanContent(con, planName, instanceId, planVersion);
        }
        return result;
    }
    
    public String purgePlanVersions(String planName) throws Exception {
        String result = FAILED;
        QueryPlan plan = getQueryPlan(planName) ;
        if(plan== null){
//          log message and rethrow exception
            String errMsg = mMessages.getString("IEPAdminUtil.Plan_not_found", new Object[]{planName});
            mLogger.log(Level.SEVERE, errMsg);
            throw new Exception(FAILED + " " + errMsg);
        }
        
        String instanceId = plan.getInstanceId();
        if(instanceId != null) {
            Connection con = Util.getConnection(mCtx.getConfigProperties());
            Util.deletePlanVersions(con, planName, instanceId);
            result = SUCCESS;
        }
        
        return result;
    }
    
    public String purgeAllPlanVersions() throws Exception {
        String result = FAILED;
        Connection con = Util.getConnection(mCtx.getConfigProperties());
        Util.deleteAllPlanVersions(con);
        result = SUCCESS;
        return result;
    }
}
