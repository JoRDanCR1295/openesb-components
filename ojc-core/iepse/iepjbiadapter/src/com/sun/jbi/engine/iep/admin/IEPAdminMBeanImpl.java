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
 * @(#)IEPManagementMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.admin;

import com.sun.jbi.engine.iep.DeploymentRecord;
import com.sun.jbi.engine.iep.ExtendedComponentContext;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Implementatio of IEPAdminMBean . It provides API support for handling IEP engine 
 * at runtime. 
 * @author rdwivedi
 */
public class IEPAdminMBeanImpl implements IEPAdminMBean {

    ExtendedComponentContext mCtxt;
    IEPAdminUtil mUtil = null;

    public IEPAdminMBeanImpl(ExtendedComponentContext ctx) {
        mCtxt = ctx;
        mUtil = new IEPAdminUtil(mCtxt);
    }

    public String[] listProcessAttributes(String serviceUnitName) {
        int i = 0;
        List<DeploymentRecord> l = null;
        if(serviceUnitName==null || serviceUnitName.length()<1){
            // Since we can get the complete list of IEP plans.
             l =  mCtxt.getDeploymentTable().getRecordList();
        } else {
            l = mCtxt.getDeploymentTable().
                    getRecordListByServiceUnitName(serviceUnitName);
        }
        String[] str = new String[l.size()];
        for (DeploymentRecord rec : l) {
            str[i++] = rec.getDeployName();
        }
        return str;
    }

    public String[] listOperators(String processDeployName) {
        int i = 0;
        DeploymentRecord rec = mCtxt.getDeploymentTable().getRecordByDeployName(processDeployName);
        IEPAdminUtil util = new IEPAdminUtil(mCtxt);
        return util.getListOfOperators(processDeployName);
    }

    public Map<String, Object> getManagedAttributesForOperator(String processDeployName, String operatorName) {
        Map<String, Object> map = null;
        DeploymentRecord rec = mCtxt.getDeploymentTable().getRecordByDeployName(processDeployName);
        IEPAdminUtil util = new IEPAdminUtil(mCtxt);
        return util.getAdministrablePropertiesForOperator(processDeployName, operatorName);
    }

    public String setManageablePropertyValue(String planName, String oprName, 
            String propName, Object value) throws Exception  {
        IEPAdminUtil util = new IEPAdminUtil(mCtxt);
        return util.setManageablePropertyValue(planName, oprName, propName, value);
    }

    public String setIEPForDebugging(String planName) {
        return mUtil.setIEPForDebugging(planName);
    }

    public String unsetIEPForDebugging(String planName) {
        return mUtil.unsetIEPForDebugging(planName);
    }

    public String executeNext(String planName) throws Exception {
        try {
            return mUtil.executeNext(planName);
        } catch (Exception e) {
            throw e;
        }
    }

    public String getCurrentDebugInfo(String planName, String oprName) throws Exception {
        try {
            return mUtil.getCurrentDebugInfo(planName, oprName);
        } catch (Exception e) {
            throw e;
        }
    }

    public String executeDebugCommand(String cmd, String planName, Object args) 
            throws Exception {
        try {
            return mUtil.executeDebugCommand(cmd, planName, (String[]) args);
        } catch (Exception e) {
            throw e;
        }
    }

    public List getInputOperators(String planName) {
        return new ArrayList();
    }

    public List getOutputOperators(String planName) {
        return new ArrayList();
    }
    
    public String setEnableDataAccessObject(String planName, String operatorName, String tabName)
            throws Exception {
        
        return mUtil.setDataAccessEnabled(planName, operatorName, tabName);
        
    }
    public String disableDataAccess(String planName, String oprName) {
        return mUtil.disableDataAccess(planName, oprName);
    }
    public String setIEPDataAccessProperty(String planName, String operatorName, String propertyName , String value ) throws Exception {
        String result = null;
        
        return result;
    }
    
    public String enableSaveStream(String planName, String operatorName , String jndi, String tableName,String isGlobal) {
        
        return mUtil.enableSaveStream(planName,operatorName,jndi,tableName,isGlobal);
        
    }
    public String removeSaveStream(String planName, String saveStreamName) {
        return mUtil.removeSaveStream(planName,saveStreamName);
    }

    public String startNewChangeSet() {
        return mUtil.startNewChangeSet();
    }

    public String applyChangeSet() throws Exception {
        return mUtil.applyChangeSet();
    }

    public String ignoreChangeSet() {
        return mUtil.ignoreChangeSet();
    }
    
    public String exportPlan(String planName, String planVersion) throws Exception {
        return mUtil.exportPlan(planName, planVersion);
    }
    
    public String purgePlanVersions(String planName) throws Exception {
        return mUtil.purgePlanVersions(planName);
    }
    
    public String purgeAllPlanVersions() throws Exception {
        return mUtil.purgeAllPlanVersions();
    }
}
