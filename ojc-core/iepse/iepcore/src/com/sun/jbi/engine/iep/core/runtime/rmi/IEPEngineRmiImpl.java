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
 * @(#)IEPEngineRmiImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.rmi;

import com.sun.jbi.engine.iep.core.runtime.change.RuntimeChangeObject;
import com.sun.jbi.engine.iep.core.runtime.debugger.DebugProcessor;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.util.Properties;

import com.sun.jbi.engine.iep.core.runtime.DefaultIEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.List;

/**
 * IEPEngineRmiImpl.java
 *
 * Created on May 23, 2005, 5:48 PM
 *
 * @author Bing Lu
 */
public class IEPEngineRmiImpl extends UnicastRemoteObject implements IEPEngineRmi {
    private static final Messages mMessages = Messages.getMessages(IEPEngineRmiImpl.class);
    
    private transient IEPEngineRmiRegistrar mRegistrar = null;
    private transient DefaultIEPEngine mEngine = null;
    private static final String mID = "RMI_IEPSE";
    
    /** Creates a new instance of IEPEngineRmiImpl */
    public IEPEngineRmiImpl(IEPEngineRmiRegistrar registrar) throws RemoteException {
        mRegistrar = registrar;
    }
    
    public void init(Properties prop) throws RemoteException {
        if (mEngine != null) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Init_can_only_be_called_once"));
        }
        try {
            mEngine = DefaultIEPEngine.getInstance();
            mEngine.init(prop);
        } catch (Exception e) {
            throw new RemoteException(e.getMessage());
        }
    }
    
    public boolean isInitialized() throws RemoteException {
        return mEngine != null;
    }
    
    public Properties getConfigProperties() throws RemoteException {
         try {
             return mEngine.getConfigProperties();
         } catch (Exception e) {
             throw new RemoteException(e.getMessage());
         }
    }

    public void setConfigProperties(Properties configProp) throws RemoteException {
         try {
             mEngine.setConfigProperties(configProp);
         } catch (Exception e) {
             throw new RemoteException(e.getMessage());
         }
     }

    private void checkInitialzation() throws RemoteException {
        if (mEngine == null) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Must_call_initProperties_prop_first"));
        }
    }

    public void start(String instanceId) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.start(instanceId);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_start", instanceId), e);
        }
    }
    
    public void start(List<String> instanceIdList) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.start(instanceIdList);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_start", instanceIdList), e);
        }
    }
    
    public boolean isScheduled(String instanceId) throws RemoteException {
        checkInitialzation();
        try {
            return mEngine.isScheduled(instanceId);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_check_if_schedued", instanceId), e);
        }
    }
    
    public void stop(String instanceId) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.stop(instanceId);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_stop", instanceId), e);
        }
    }
    
    public void stop(List<String> instanceIdList) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.stop(instanceIdList);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_stop", instanceIdList), e);
        }
    }

    public void stopAll() throws RemoteException {
        checkInitialzation();
        try {
            mEngine.stopAll();
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_stopAll"), e);
        }
    }

    public void pause(String instanceId) throws Exception {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void resume(String instanceId) throws Exception {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    
    public void deploy(QueryPlanInfo queryPlanInfo) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.deploy(queryPlanInfo);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_deploy", queryPlanInfo), e);
        }
    }
    
    public void deploy(List<QueryPlanInfo> queryPlanInfoList) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.deploy(queryPlanInfoList);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_deploy", queryPlanInfoList), e);
        }
    }
    
    public void undeploy(String instanceId) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.undeploy(instanceId);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_undeploy", instanceId), e);
        }
    }
    
    public void undeploy(List<String> instanceIdList) throws RemoteException {
        checkInitialzation();
        try {
            mEngine.undeploy(instanceIdList);
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_undeploy", instanceIdList), e);
        }
    }

    public void unregister() throws RemoteException {
        mRegistrar.unregister();
    }
    
    public void destroy() throws RemoteException {
        checkInitialzation();
        try {
            mEngine.destroy();
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_destroy"), e);
        }
    }
    
    //==============================================
    public String[] listDeployed() throws RemoteException {
        checkInitialzation();
        try {
            return mEngine.listDeployed();
        } catch (Exception e) {
            throw new RemoteException(mMessages.getString("IEPEngineRmiImpl.Fail_to_listDeployed"), e);
        }
    }
    
    public String getId() {
        return mID;
    }

    public QueryPlan getScheduledPlanByInstanceId(String id) throws Exception {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setIEPInstanceForDebugging(String planId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void unsetIEPInstanceForDebugging(String planId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public DebugProcessor getDebugProcessor(String planId) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String scheduleOperatorPropertyChange(String planId, String operatorName, String propertyName, Object propertyValue) throws Exception {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void startNewChangeSet() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String addNewChange(RuntimeChangeObject obj) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void applyChangeSet() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void ignoreChangeSet() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
