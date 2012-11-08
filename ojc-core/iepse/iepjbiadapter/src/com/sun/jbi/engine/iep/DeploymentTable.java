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
 * @(#)DeploymentTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

import java.util.*;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;

/**
 * DeploymentTable.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class DeploymentTable {
    private List<DeploymentRecord> mRecordList;
    
    /** Creates a new instance of DeploymentTable */
    public DeploymentTable() {
        mRecordList = new LinkedList<DeploymentRecord>();
    }
    
    public synchronized void addRecord(String serviceUnitRootPath, String serviceUnitName, String deployName, QueryPlan plan) {
       if (getRecordByDeployName(deployName) == null) {
           DeploymentRecord dr = new DeploymentRecord(serviceUnitRootPath, serviceUnitName, deployName, plan);
           mRecordList.add(dr);
       }
    }
    
    public synchronized void removeRecordsByServiceUnitName(String serviceUnitName) {
        for (int i = mRecordList.size() - 1; i >=0; i--) {
            DeploymentRecord dr = mRecordList.get(i);
            if (dr.getServiceUnitName().equals(serviceUnitName)) {
                mRecordList.remove(dr);
            }
        }
    }
    
    public synchronized List<DeploymentRecord> getRecordListByServiceUnitName(String serviceUnitName) {
        List<DeploymentRecord> ret = new ArrayList<DeploymentRecord>();
        for (DeploymentRecord dr : mRecordList) {
            if (dr.getServiceUnitName().equals(serviceUnitName)) {
                ret.add(dr);
            }
        }
        return ret;
    }
    
     public synchronized DeploymentRecord getRecordByDeployName(String deployName) {
        for (DeploymentRecord dr : mRecordList) {
            if (dr.getDeployName().equals(deployName)) {
                return dr;
            }
        }
        return null;
    }
    
    public synchronized boolean hasRecords(String serviceUnitName) {
        return getRecordListByServiceUnitName(serviceUnitName).size() > 0;
    }

    public synchronized DeploymentRecord getRecordByInstanceId(String instanceId) {
         if(instanceId == null) {
             return null;
         }
         
         for (DeploymentRecord dr : mRecordList) {
              QueryPlan plan = dr.getPlan();
             if (plan != null && instanceId.equals(plan.getInstanceId())) {
                 return dr;
             }
         }
         
         return null;
     }
     
    public synchronized List<String> getDeployNameList() {
        List<String> ret = new ArrayList<String>();
        for (DeploymentRecord dr : mRecordList) {
            ret.add(dr.getDeployName());
        }
        return ret;
    }
    
    public synchronized List<DeploymentRecord> getRecordList() {
        return Collections.unmodifiableList(mRecordList);
    }
    
}
