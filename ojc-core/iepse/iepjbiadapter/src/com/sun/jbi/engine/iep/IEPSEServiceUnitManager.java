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
 * @(#)IEPSEServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.SendFailureListener;
import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.util.DirectoryUtil;
import com.sun.jbi.engine.iep.core.runtime.util.NDC;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.alert.IEPSEAlertSender;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;

/**
 * IEPSEServiceUnitManager.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class IEPSEServiceUnitManager implements ServiceUnitManager {

    private static final Messages mMessages = Messages.getMessages(IEPSEServiceUnitManager.class);
    private static Logger mLogger = Messages.getLogger(IEPSEServiceUnitManager.class);
    
    public static final String PARTNER_MYROLE = "myRole";
    public static final String PARTNER_PARTNERROLE = "partnerRole";

    private ExtendedComponentContext mExtendedContext;
    private IEPSendFailureListener sendFailureListener;

    
    /** Creates a new instance of IEPSEDeployer */
    public IEPSEServiceUnitManager() {
    }

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }

    private String createExceptionMessage(String componentName, String taskName, String status, String locToken, String locParam, String locMessage, Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);

        return retMsg;
    }

    void initialize(ExtendedComponentContext extendedContext) {
        mExtendedContext = extendedContext;
        sendFailureListener = new IEPSendFailureListener(mExtendedContext);
        mLogger = Messages.getLogger(IEPSEServiceUnitManager.class);
    }

    /**
     * Deploy a Service Unit to the component. This is called by the JBI 
     * implementation in order to deploy the given artifact to the implementing 
     * component. 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
    public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
    	try {
    		NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.deploy");
    		if (mLogger.isLoggable(Level.FINE)) {
    			mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Deploying_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
    		}
    		ComponentContext context = mExtendedContext.getComponentContext();
    		String taskName = "deploy";
    		DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
    		if (deploymentTable.hasRecords(serviceUnitName)) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.SU_already_deployed", serviceUnitName));    			
    			String extMsg = createExceptionMessage(context.getComponentName(), taskName, "FAILED", "IepSeSum_Deploy_1", null, "Service Unit has already been deployed", null);
    			throw new DeploymentException(extMsg);
    		}
    		try {
    			if (mLogger.isLoggable(Level.FINE)) {
    				mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.IEPSE_deploying", serviceUnitName));
    			}

    			String[] ext = new String[]{".iep"};
    			List<File> iepFiles = DirectoryUtil.getFilesRecursively(new File(serviceUnitRootPath), ext);
    			Map<String, String> iepFileTable = new HashMap<String, String>();
    			int serviceUnitRootPathLen = serviceUnitRootPath.length() + 1; //1: for appending '/'
    			for (int i = 0; i < iepFiles.size(); i++) {
    				File f = (File) iepFiles.get(i);
    				String fPath = f.getPath();
    				String instanceId = NameUtil.makeInstanceId(fPath.substring(serviceUnitRootPathLen));
    				if(deploymentTable.getRecordByInstanceId(instanceId) != null){
    					throw new Exception(mMessages.getString("IEPSEServiceUnitManager.Duplicate_Event_Processor", instanceId));
    				}
    				iepFileTable.put(instanceId, f.getPath());
    			}

    			// deployment
    			Properties configProp = mExtendedContext.getConfigProperties();
    			ArrayList<String> instanceIdList = new ArrayList<String>();
    			ArrayList<QueryPlanInfo> queryPlanInfoList = new ArrayList<QueryPlanInfo>();
    			for (Iterator<String> itr = iepFileTable.keySet().iterator(); itr.hasNext();) {
    				// seen IEPSEInOnlyThread's process() and IEPSEOutOnlyThread's process()
    				String instanceId = itr.next();
    				String planPath = iepFileTable.get(instanceId);
    				instanceIdList.add(instanceId);
    				QueryPlanInfo qpi = new QueryPlanInfo(instanceId, planPath);
    				queryPlanInfoList.add(qpi);
    			}

    			IEPEngine engine = mExtendedContext.getEngine();
    			// 1. deploy
    			try {
    				engine.deploy(queryPlanInfoList);
    			} catch (Exception e) {
    				/*
    				 * Remove all the plans that where inserted in the savePlansToDatabase call as the service 
    				 * unit deployment of them failed. Failure to deploy even one of the plans is treated as 
    				 * the Service Unit deployment failure.
    				 * TODO: Ideally the Plan should be saved to the EMS_PLAN table as part of the engine 
    				 * deploy method in one atomic DB operation. Have to explore if this is possible.
    				 */
		      
    				Util.removePlanFromDatabase(instanceIdList, configProp);
    				// rethrow the exception so that the enclosing catch will throw the deployment exception 
    				// to fail the deployment the Service Unit.
    				throw e;
    			}

    			for (int i = 0; i < queryPlanInfoList.size(); i++) {
    				QueryPlanInfo qpi = queryPlanInfoList.get(i);
    				String instanceId = qpi.getInstanceId();
    				QueryPlan plan = Util.getPlanByInstanceId(configProp,
    						instanceId);

    				// 3. create deployment record
    				deploymentTable.addRecord(serviceUnitRootPath,
    						serviceUnitName, instanceId, plan);
    			}
    			if (mLogger.isLoggable(Level.FINE)) {
    				mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.IEPSE_deployed_successfully",
    						instanceIdList));
    			}
    		} catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.Failed_to_deploy_su", serviceUnitName), e);
    			String extMsg = createExceptionMessage(context.getComponentName(), taskName, "FAILED", "IepSeSum_Deploy_2", null, "Service Unit deploy error: " + e.getMessage(), e);
    			throw new DeploymentException(extMsg, e);
    		}            
    		String retMsg = createSuccessMessage("deploy", context.getComponentName());
    		if (mLogger.isLoggable(Level.FINE)) {
    			mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Deployed_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
    		}
    		return retMsg;
    	} finally {
    		NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.deploy");
    	}
    }

    /**
     * Undeploy a Service Unit from the component. This is called by the JBI 
     * implementation in order to undeploy the given Service Unit from the 
     * implementing component. The deployment must be shut down before it 
     * can be undeployed
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.undeploy");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Undeploying_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
            }
            ComponentContext context = mExtendedContext.getComponentContext();
            
            
            DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
            IEPEngine engine = mExtendedContext.getEngine();
            try {
                // undeloyment goes through deployment steps in the reverse order. see init()
                if (deploymentTable.hasRecords(serviceUnitName)) {
                    List<DeploymentRecord> deploymentRecordList = deploymentTable.getRecordListByServiceUnitName(serviceUnitName);
                    ArrayList<String> instanceIdList = new ArrayList<String>();
                    for (int i = 0,  I = deploymentRecordList.size(); i < I; i++) {
                        DeploymentRecord dr = deploymentRecordList.get(i);
                        String iepInstanceId = dr.getPlan().getInstanceId();
                        instanceIdList.add(iepInstanceId);
                    }
                    // 3. delete deployment record
                    deploymentTable.removeRecordsByServiceUnitName(serviceUnitName);
                    
                    // 2. undeploy from db. see IEPSEHeartBeatThread's run()
                    engine.undeploy(instanceIdList);
                    
                    // 1. delete from db
                    Util.removePlanFromDatabase(instanceIdList, mExtendedContext.getConfigProperties());
                }
            } catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.Failed_to_undeploy_su", serviceUnitName), e);            	
                String exMsg = createExceptionMessage(context.getComponentName(), "undeploy", "FAILED", "IepSeSum_Undeploy_1", null, "Service Unit undeploy error: " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
            
            
            String retMsg = createSuccessMessage("undeploy", context.getComponentName());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Undeployed_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
            }
            return retMsg;
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.undeploy");
        }
    }

    /**
     * Initialize the deployment. This is the first phase of a two-phase start, 
     * where the component must prepare to receive service requests related to 
     * the deployment (if any). 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.init");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Initializing_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
            }
            String taskName = "init";
            ComponentContext context = mExtendedContext.getComponentContext();
            Properties configProp = mExtendedContext.getConfigProperties();
            DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
            try {
                // 4. register services
                SUDescriptorSupport descriptorSupport = null;
                try {
                    //Get the Consumes and Provides endpoint information from the SU descriptor
                    descriptorSupport = new SUDescriptorSupport(serviceUnitRootPath);
                } catch (ConfigurationException ex) {
                    throw new Exception(mMessages.getString("Error_parsing_the_descriptor_file_for_the_Service_Unit_at", serviceUnitRootPath), ex);
                }
                EndpointIdentifier[] endpointIdentifier = descriptorSupport.getServices();
                for (int i = 0; i < endpointIdentifier.length; i++) {
                    EndpointIdentifier ei = endpointIdentifier[i];
                    QName serviceName = ei.getServiceName();
                    String endpointName = ei.getEndpointName();
                    String deployName = serviceName.getNamespaceURI();
                    String instanceId = deployName;
                    QueryPlan plan = Util.getPlanByInstanceId(configProp, instanceId);
                    if(plan == null) {
                        throw new Exception(mMessages.getString("IEPSEServiceUnitManager.Failed_to_find_deployment_record", new Object[] {deployName, deploymentTable.getDeployNameList(), serviceUnitRootPath}));
                    }
                    deploymentTable.addRecord(serviceUnitRootPath, serviceUnitName, deployName, plan);
                    DeploymentRecord dr = deploymentTable.getRecordByDeployName(deployName);
                    if (ei.isProvider()) {  // inbound
                        // FIX ME: Resolver must be set prior to activiting any endpoint
                        // See 5.6.2.1.5 of JBI 1.0 pr spec 
                        ServiceEndpoint serviceEndpoint = context.activateEndpoint(serviceName, endpointName);
                        dr.addProviderEndpoint(serviceEndpoint);
                    } 
                }
                // Specify wire qualities
                mExtendedContext.getDeliveryChannel().installServiceQualities(
                        serviceUnitName, serviceUnitRootPath);
                mExtendedContext.getDeliveryChannel().addSendFailureListener(sendFailureListener);
            } catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.Failed_to_init_su", serviceUnitName), e);
                String extMsg = createExceptionMessage(context.getComponentName(), taskName, "FAILED", "IepSeSum_Init_2", null, "Service Unit init error: " + e.getMessage(), e);
                throw new DeploymentException(extMsg, e);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Initialized_service_unit", new Object[]{serviceUnitName, serviceUnitRootPath}));
            }
            //Alert
            IEPEngine engine = mExtendedContext.getEngine();
            IEPSEAlertSender.getInstance().alertSUStatusChange(engine.getId(), serviceUnitName, NotificationEvent.OPERATIONAL_STATE_STARTING);
            
            //
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.init");
        }
    }

    /**
     * Shut down the deployment. This causes the deployment to return to the 
     * state it was in after deploy() and before init(). 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.shutDown");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Shuting_down_service_unit", serviceUnitName));
            }
            ComponentContext context = mExtendedContext.getComponentContext();
            DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
            try {
                // undeloyment goes through deployment steps in the reverse order. see init()
                if (deploymentTable.hasRecords(serviceUnitName)) {
                    List<DeploymentRecord> deploymentRecordList = deploymentTable.getRecordListByServiceUnitName(serviceUnitName);
                    // 4. unregister services
                    for (int i = 0,  I = deploymentRecordList.size(); i < I; i++) {
                        DeploymentRecord dr = deploymentRecordList.get(i);
                        List providerEndpointList = dr.getProviderEndpointList();
                        for (int j = 0,  J = providerEndpointList.size(); j < J; j++) {
                            ServiceEndpoint serviceEndpoint = (ServiceEndpoint) providerEndpointList.get(j);
                            context.deactivateEndpoint(serviceEndpoint);
                        }
                    }
                }
                mExtendedContext.getDeliveryChannel().uninstallServiceQualities(serviceUnitName);
                mExtendedContext.getDeliveryChannel().removeSendFailureListener(this.sendFailureListener);
            } catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.SU_shutdown_error", serviceUnitName), e);            	
                String exMsg = createExceptionMessage(context.getComponentName(), "undeploy", "FAILED", "IepSeSum_Shutdown_1", null, "Service Unit shutdown error: " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Shut_down_service_unit", serviceUnitName));
            }
            //Alert
            IEPEngine engine = mExtendedContext.getEngine();
            IEPSEAlertSender.getInstance().alertSUStatusChange(engine.getId(), serviceUnitName, NotificationEvent.OPERATIONAL_STATE_SHUTDOWN);
            //
            
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.shutDown");
        }
    }

    /**
     * Start the deployment. This is the second phase of a two-phase start, 
     * where the component can now initiate service requests related to the 
     * deployment.     
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void start(String serviceUnitName) throws DeploymentException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.start");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Starting_service_unit", serviceUnitName));
            }
            ComponentContext context = mExtendedContext.getComponentContext();
            DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
            IEPEngine engine = mExtendedContext.getEngine();
            try {
                List<DeploymentRecord> recordList = deploymentTable.getRecordListByServiceUnitName(serviceUnitName);
                ArrayList<String> instanceIdList = new ArrayList<String>();
                for (int i = 0; i < recordList.size(); i++) {
                    DeploymentRecord r = recordList.get(i);
                    String instanceId = r.getPlan().getInstanceId();
                    instanceIdList.add(instanceId);
                }
                
                engine.start(instanceIdList);
                for (int i = 0; i < recordList.size(); i++) {
                    DeploymentRecord r = recordList.get(i);
                    r.setStarted(true);
                    mExtendedContext.startPollingTable(r);
                    mExtendedContext.startReplayStream(r);
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.IEPSE_started", instanceIdList));
                }
            } catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.Failed_to_start_su", serviceUnitName), e);
                String exMsg = createExceptionMessage(context.getComponentName(), "stop", "FAILED", "IepSeSum_Start_1", null, "Service Unit start error: " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Started_service_unit", serviceUnitName));
            }
            //Alert
            IEPSEAlertSender.getInstance().alertSUStatusChange(engine.getId(), serviceUnitName, NotificationEvent.OPERATIONAL_STATE_STARTED);
            //
            
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.start");
        }
    }

    /**
     * Stop the deployment. This causes the component to cease generating 
     * service requests related to the deployment. This returns the deployment 
     * to a state equivalent to after init() was called    
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void stop(java.lang.String serviceUnitName) throws DeploymentException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.stop");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Stopping_service_unit", serviceUnitName));
            }
            ComponentContext context = mExtendedContext.getComponentContext();
            DeploymentTable deploymentTable = mExtendedContext.getDeploymentTable();
            IEPEngine engine = mExtendedContext.getEngine();
            try {
                List<DeploymentRecord> recordList = deploymentTable.getRecordListByServiceUnitName(serviceUnitName);
                ArrayList<String> instanceIdList = new ArrayList<String>();
                for (int i = 0; i < recordList.size(); i++) {
                    DeploymentRecord r = recordList.get(i);
                    String instanceId = r.getPlan().getInstanceId();
                    instanceIdList.add(instanceId);
                }
                for (int i = 0; i < recordList.size(); i++) {
                    DeploymentRecord r = recordList.get(i);
                    r.setStarted(false);
                    mExtendedContext.stopPollingTable(r);
                    mExtendedContext.stopReplayStream(r);
                }
                engine.stop(instanceIdList);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.IEPSE_stopped", instanceIdList));
                }
            } catch (Exception e) {
    			mLogger.log(Level.SEVERE, mMessages.getString("IEPSEServiceUnitManager.Failed_to_stop_su", serviceUnitName), e);            	
                String exMsg = createExceptionMessage(context.getComponentName(), "stop", "FAILED", "IepSeSum_Stop_1", null, "Service Unit stop error: " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSEServiceUnitManager.Stopped_service_unit", serviceUnitName));
            }
            //Alert
            IEPSEAlertSender.getInstance().alertSUStatusChange(engine.getId(), serviceUnitName, NotificationEvent.OPERATIONAL_STATE_STOPPED);
            //
            
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSEServiceUnitManager.stop");
        }
    }
    
    
}

class IEPSendFailureListener implements SendFailureListener {
    private ExtendedComponentContext context;
    IEPSendFailureListener (ExtendedComponentContext context){
        this.context = context;
        
    }
    public void handleSendFailure(MessagingException error, MessageExchange mex){
        ExchangeTable exTable = context.getExchangeTable();
        ExchangeRecord record = exTable.getInOnly((InOnly)mex);
        String deployName = record.getOperator().getPlan().getInstanceId();
        DeploymentRecord dr = context.getDeploymentTable()
                .getRecordByDeployName(deployName);
        if (dr == null) {
            //SU is undeployed, ignore the Error
            return;
        }
        String msgId = (String) mex.getProperty(ServiceQuality.MESSAGE_ID);
        record.setErrorOnMessage(msgId);
        exTable.deleteInOnly((InOnly)mex);
    }
 }
