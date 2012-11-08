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
 * @(#)DeploymentRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;

/**
 * DeploymentRecord.java
 *
 * serviceUnitName (1) ---------- (*) instanceId (each su can contain multiple .iep files)
 * deployName (*) ---------- (1) plan (each plan can have multiple instances)
 *
 * The deployNames are unique within a jbi engine
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
*/
public class DeploymentRecord {
    private String mServiceUnitRootPath;
    private String mServiceUnitName;
    private String mDeployName;
    private QueryPlan mPlan;
    private boolean mStarted;
    private List<ServiceEndpoint> mProviderEndpointList = new ArrayList<ServiceEndpoint>();
    
    public Timestamp timestamp = new Timestamp(0);
    
    /** Creates a new instance of DeploymentRecord */
    public DeploymentRecord(String serviceUnitRootPath,
                            String serviceUnitName, 
                            String deployName, 
                            QueryPlan plan) 
    {
        mServiceUnitRootPath = serviceUnitRootPath;
        mServiceUnitName = serviceUnitName;
        mDeployName = deployName;
        mPlan = plan;
    }
    
    public String getServiceUnitRootPath() {
        return mServiceUnitRootPath;
    }
    
    public String getServiceUnitName() {
        return mServiceUnitName;
    }
    
    public String getDeployName() {
        return mDeployName;
    }
    
    public QueryPlan getPlan() {
        return mPlan;
    }
    
    public void setStarted(boolean started) {
        mStarted = started;
    }
    
    public boolean isStarted() {
        return mStarted;
    }
    
    public void addProviderEndpoint(ServiceEndpoint serviceEndpoint) {
        if (!mProviderEndpointList.contains(serviceEndpoint)) {
            mProviderEndpointList.add(serviceEndpoint);
        }
    }
    
    public ServiceEndpoint getProviderEndpoint(String opName) {
        String operation = NameUtil.makeJavaId(opName);
        for (int i = 0; i < mProviderEndpointList.size(); i++) {
            ServiceEndpoint sep = mProviderEndpointList.get(i);
            if (sep.getEndpointName().equals(operation)) {
                return sep;
            }
        }
        return null;
    }
    
    public List<ServiceEndpoint> getProviderEndpointList() {
        return new ArrayList<ServiceEndpoint>(mProviderEndpointList);
    }
    
    public boolean isProviderEndpoint(ServiceEndpoint serviceEndpoint) {
        return mProviderEndpointList.contains(serviceEndpoint);
    }
}
