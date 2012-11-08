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
 * @(#)IEPEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime;

import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeObject;
import com.sun.jbi.engine.iep.core.runtime.debugger.DebugProcessor;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import java.util.List;
import java.util.Properties;

/**
 * IEPEngine.java
 *
 * Created on May 27, 2005, 5:32 PM
 *
 * @author Bing Lu
 */
public interface IEPEngine {
    public void init(Properties prop) throws Exception;
    public boolean isInitialized() throws Exception;
    public void setConfigProperties(Properties configProp) throws Exception;
    public Properties getConfigProperties() throws Exception;
    public void start(String instanceId) throws Exception;
    public void start(List<String> instanceIdList) throws Exception;
    public boolean isScheduled(String instanceId) throws Exception;
    public void stop(String instanceId) throws Exception;
    public void stop(List<String> instanceIdList) throws Exception;
    public void stopAll() throws Exception;
    public void pause(String instanceId) throws Exception;
    public void resume(String instanceId) throws Exception;
    public void deploy(QueryPlanInfo queryPlanInfo) throws Exception;
    public void deploy(List<QueryPlanInfo> queryPlanInfoList) throws Exception;
    public void undeploy(String instanceId) throws Exception;
    public void undeploy(List<String> instanceIdList) throws Exception;
    public void unregister() throws Exception;
    public void destroy() throws Exception;
    
    public String[] listDeployed() throws Exception;
    public String getId() ;
    public QueryPlan getScheduledPlanByInstanceId(String id) throws Exception;
    
    public void setIEPInstanceForDebugging(String planId);
    public void unsetIEPInstanceForDebugging(String planId);
    public DebugProcessor getDebugProcessor(String planId);
    
    //Following can be moved to new interface with it;s own manager impl to handle
    // run time modifications to IEP engine.
    public void startNewChangeSet();
    
    public String addNewChange(RuntimeChangeObject obj) throws Exception;
    
    public String scheduleOperatorPropertyChange(String planId,String operatorName,
            String propertyName, Object propertyValue) throws Exception;
       
    public void applyChangeSet() throws Exception;
    
    public void ignoreChangeSet() ;
    //
    
}
