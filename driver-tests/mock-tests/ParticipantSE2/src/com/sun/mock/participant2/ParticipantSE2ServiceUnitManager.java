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
 * @(#)WorkflowSEServiceUnitManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.mock.participant2;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.xml.sax.EntityResolver;


import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;

public class ParticipantSE2ServiceUnitManager implements ServiceUnitManager {
    private static final Logger mLogger = Logger.getLogger(ParticipantSE2ServiceUnitManager.class.getName());

    private HashSet mDeployedId;
    private ComponentContext mContext;
    private Hashtable mWsdlMap = null;


    /** Creates a new instance of WorkflowSEDeployer */
    public ParticipantSE2ServiceUnitManager(Component component) {
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

/*
    void initialize(WorkflowMapEntryTable workflowMapEntryTable,
                    ComponentContext context,
                    DeliveryChannel channel,
                    StatusProviderHelper statusProviderHelper,
                    Hashtable wsdlMap) {
        mWorkflowMapEntryTable = workflowMapEntryTable;
        mDeployedId = new HashSet();
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mWsdlMap = wsdlMap;
    }
*/
    /**
     * Deploy a Service Unit to the component. This is called by the JBI 
     * implementation in order to deploy the given artifact to the implementing 
     * component. 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     * @param serviceUnitRootPath - the full path to the Service Unit artifact root directory
     */
     public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        mLogger.info("Deploying service unit : " + serviceUnitName + " from " + serviceUnitRootPath);
        String retMsg = createSuccessMessage("deploy", mContext.getComponentName());
        mLogger.info("Deployed service unit : " + serviceUnitName + " from " + serviceUnitRootPath + " successfully");
        return retMsg;
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
        mLogger.info("Undeploying service unit : " + serviceUnitName + " from " + serviceUnitRootPath);
        String retMsg = createSuccessMessage("undeploy", mContext.getComponentName());
        mLogger.info("Undeployed service unit : " + serviceUnitName + " from " + serviceUnitRootPath + " successfully");
        return retMsg;
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
        String taskName = "init";
        mLogger.info("Initializing service unit " + serviceUnitName + " serviceUnitRootPath: " + serviceUnitRootPath);
        if (mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "IepSeSum_Init_1", serviceUnitName, "Service Unit has already been deployed" , null);
            throw new DeploymentException(extMsg);
        }
        
        Vector listOfWSDLDefinitions = new Vector();
        /*
        try {
            // load task model ( Workflow model) 
            WorkflowModelManager wfm = new WorkflowModelManager();
            File deploydir = new File(serviceUnitRootPath);

            String[] files = deploydir.list();
            String workflowmapfile = null;
            String wsdlFile = null;
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                String fnCheck = filename.toLowerCase();
                if (fnCheck.endsWith("wf")) {
                    workflowmapfile = deploydir.getAbsolutePath() + File.separator + filename;
                    wfm.loadWorkFlowModel(new File(workflowmapfile));
                } else if (fnCheck.endsWith("wsdl")) {
                    wsdlFile = deploydir.getAbsolutePath() + File.separator + filename;
                    Definition wsdlDef = readWsdl(new File(wsdlFile), null);
                    listOfWSDLDefinitions.add(wsdlDef);
                } else if (fnCheck.endsWith("xhtml")) {
                    wfm.loadXform(new File (deploydir, files[i]));                    
                } else if (fnCheck.endsWith("instance.xml")) {
                    wfm.loadXFormInstance(new File (deploydir, files[i]));                    
                } 
            }

 
            // load SU jbi.xml
            SUDescriptorSupport sud = new SUDescriptorSupport(serviceUnitRootPath);
            
            // Iterator portmaps = PortMapReader.parse(new File(portmapfile));
      //      Iterator portmaps = PortMapReader.parse(sud);
            
//         
           //add provides entry
           Provides[] pds = sud.getProvides();
           for (int ix=0; ix<pds.length; ix++) {
               Provides p = pds[ix];

               QName serviceName = p.getServiceName();
               String endpoint = p.getEndpointName();
               QName interfce = p.getInterfaceName();      
                
                //Get the task Model from WorkflowModelManager
                Tasks tasksModel = wfm.getTaskModel(serviceName.getNamespaceURI(),serviceName.getLocalPart(),interfce.getNamespaceURI(), interfce.getLocalPart()  );
                WorkflowMapEntry wfentry = null;
                if (tasksModel != null ) {
                    wfentry = new WorkflowMapEntry(serviceName, 
                    							   interfce, 
                    							   endpoint, 
                    							   serviceUnitName, 
                    							   tasksModel,
                    							   WorkflowMapEntry.EntryType.ENTRY_PROVIDE);
                    
                    Task task = tasksModel.getTasks().get(0);
                    wfentry.setXform(wfm.getXform(task.getName()));
                    wfentry.setInputXformInstance(wfm.getInputXformInstance(task.getName()));
                    wfentry.setOutputXformInstance(wfm.getOutputXformInstance(task.getName()));                    
                } else {
                    for (int index =0; index < listOfWSDLDefinitions.size();index++) {
                        Definition wsdlDef =(Definition) listOfWSDLDefinitions.get(index);
                        if (wsdlDef.getTargetNamespace().equals(interfce.getNamespaceURI())) {
                            wfentry = new WorkflowMapEntry(serviceName, interfce, endpoint, serviceUnitName, wsdlDef);
                        }
                    }
                }
                if (wfentry != null) {
                    mWorkflowMapEntryTable.addEntry(wfentry);
                }
                List list = mWorkflowMapEntryTable.getEntryList();
                TaskManager tManager = TaskManagerFactory.getInstance().getTaskManager();
                for (int i = 0, I = list.size(); i < I; i++) {
                    WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list.get(i);
                    QName wrkIntfc = workflowMapEntry.getInterface();
                    if (interfce.equals(wrkIntfc)) {
                        ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(serviceName, endpoint);

                        workflowMapEntry.setService(serviceName);
                        workflowMapEntry.setServiceEndpoint(serviceEndpoint);

                        StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                        String statusId = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, endpoint);
                        reporting.addProvisioningEndpoint(statusId);
                        EndpointStatus endpointStatus = reporting.getEndpointStatus(statusId);
                        workflowMapEntry.setEndpointStatus(statusId, endpointStatus);
                        if(workflowMapEntry.getTaskModel() != null && workflowMapEntry.getTaskModel().getTasks() != null)
                        {
                            tManager.recover(workflowMapEntry.getTaskModel().getTasks().get(0));
                        }
                        
                        mLogger.info("Activated workflow (" 
                            + workflowMapEntry.getInterface() + ", "
                            + ") inbound service: " + serviceName);
                    }
                }
  
  
            }
           
           //add consume entry related
           processConsumes(sud, wfm, listOfWSDLDefinitions, serviceUnitName);
          
           
            // Add serviceUnitName to lookup table
            mDeployedId.add(serviceUnitName);
            wfm = null;
        } catch (Exception e) {
            String extMsg = "Failed to initialize service unit " + serviceUnitName + " serviceUnitRootPath: " + serviceUnitRootPath;
            mLogger.log(Level.WARNING, extMsg, e);
            throw new DeploymentException(extMsg, e);
        }
        */
        mLogger.info("Initialized service unit " + serviceUnitName + " serviceUnitRootPath: " + serviceUnitRootPath + " successfully.");
    }

    private void processConsumes(SUDescriptorSupport sud, 
    						
    							 Vector listOfWSDLDefinitions,
    							 String serviceUnitName) throws Exception {
    	Consumes[] cns = sud.getConsumes();
        for (int ix=0; ix<cns.length; ix++) {
            Consumes c = cns[ix];

            QName serviceName = c.getServiceName();
            String endpoint = c.getEndpointName();
            QName interfce = c.getInterfaceName();      
             
             //Get the task Model from WorkflowModelManager
             /*Tasks tasksModel = wfm.getTaskModel(serviceName.getNamespaceURI(),serviceName.getLocalPart(),interfce.getNamespaceURI(), interfce.getLocalPart()  );
             
             if (tasksModel != null ) {
            	 processTasks(wfm, tasksModel, serviceName, interfce, endpoint,serviceUnitName);                    
             } */

         }

    }
    
   
    /**
     * Shut down the deployment. This causes the deployment to return to the 
     * state it was in after deploy() and before init(). 
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        String taskName = "init";
        mLogger.info("Shutting down service unit " + serviceUnitName);
        if (!mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "WorkflowSeSum_ShutDown_1", serviceUnitName, "Service Unit has not been deployed" , null);
            throw new DeploymentException(extMsg);
        }
        /*
        try {
            // Remove serviceUnitName from lookup table
            mDeployedId.remove(serviceUnitName);
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            List list = mWorkflowMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
            for (int i = 0, I = list.size(); i < I; i++) {
                WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry)list.get(i);
                if(workflowMapEntry.getEntryType() == WorkflowMapEntry.EntryType.ENTRY_PROVIDE) {
	                
	                ServiceEndpoint serviceEndpoint = workflowMapEntry.getServiceEndpoint();
	                if (serviceEndpoint != null) {
	                    mContext.deactivateEndpoint(serviceEndpoint);
	                }
	                String endpointStatusId = workflowMapEntry.getEndpointStatusId();
	                if (endpointStatusId != null) {
	                    reporting.removeProvisioningEndpoints(new String[]{endpointStatusId});
	                }
                } else {
                 String outEndpointStatusId = workflowMapEntry.getEndpointStatusId();
	                if (outEndpointStatusId != null) {
	                    reporting.removeConsumingEndpoints(new String[]{outEndpointStatusId});
	                }
                }
                
                mWorkflowMapEntryTable.removeEntry(workflowMapEntry);
            }
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "WorkflowSeSum_ShutDown_2", serviceUnitName, "Service Unit shutDown error" , e);
            throw new DeploymentException(extMsg, e);
        }
        */
        mLogger.info("Shut down service unit " + serviceUnitName + " successfully.");
    }

    /**
     * Start the deployment. This is the second phase of a two-phase start, 
     * where the component can now initiate service requests related to the 
     * deployment.     
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void start(String serviceUnitName) throws DeploymentException {
        mLogger.info("Starting service unit " + serviceUnitName);
        /*List list = mWorkflowMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry)list.get(i);
            workflowMapEntry.setStarted(true);
        }*/
        mLogger.info("Started service unit " + serviceUnitName + " successfully.");
    }

    /**
     * Stop the deployment. This causes the component to cease generating 
     * service requests related to the deployment. This returns the deployment 
     * to a state equivalent to after init() was called    
     *
     * @param serviceUnitName - the name of the Service Unit being deployed.
     */
    public void stop(java.lang.String serviceUnitName) throws DeploymentException {
        mLogger.info("Stopping down service unit " + serviceUnitName);
        /*List list = mWorkflowMapEntryTable.getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry)list.get(i);
            workflowMapEntry.setStarted(false);
        }*/
        mLogger.info("Stopped service unit " + serviceUnitName + " successfully.");
    }

}
