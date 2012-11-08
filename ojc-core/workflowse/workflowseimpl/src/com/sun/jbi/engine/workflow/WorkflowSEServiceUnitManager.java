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

package com.sun.jbi.engine.workflow;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Task;


public class WorkflowSEServiceUnitManager implements ServiceUnitManager {
    private static final Logger mLogger = Logger
            .getLogger(WorkflowSEServiceUnitManager.class.getName());

    private HashSet mDeployedId;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private WorkflowMapEntryTable mWorkflowMapEntryTable;

    private Hashtable mWsdlMap;

    private WorkflowEngine mEngine;

    private Component mComponent;

    /** Creates a new instance of WorkflowSEDeployer */
    public WorkflowSEServiceUnitManager(Component component) {
        mComponent = (Component) component;
    }

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);

        return retMsg;
    }

    private String createExceptionMessage(String componentName,
            String taskName, String status, String locToken, String locParam,
            String locMessage, Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken,
                locMessage, locParam, exObj);

        return retMsg;
    }

    void initialize(WorkflowMapEntryTable workflowMapEntryTable,
            ComponentContext context, DeliveryChannel channel,
            StatusProviderHelper statusProviderHelper, Hashtable wsdlMap,
            WorkflowEngine engine) {
        mWorkflowMapEntryTable = workflowMapEntryTable;
        mDeployedId = new HashSet();
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mWsdlMap = wsdlMap;
        mEngine = engine;
    }

    /**
     * Deploy a Service Unit to the component. This is called by the JBI
     * implementation in order to deploy the given artifact to the implementing
     * component.
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public String deploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        mLogger.info(I18n.loc(
                "WLM-5011: Deploying service unit : {0} from {1}",
                serviceUnitName, serviceUnitRootPath));
        String retMsg = createSuccessMessage("deploy", mContext
                .getComponentName());
        mLogger.info(I18n.loc(
                "WLM-5012: Deployed service unit :  {0} from {1} successfully",
                serviceUnitName, serviceUnitRootPath));
        return retMsg;
    }

    /**
     * Undeploy a Service Unit from the component. This is called by the JBI
     * implementation in order to undeploy the given Service Unit from the
     * implementing component. The deployment must be shut down before it can be
     * undeployed
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        mLogger.info(I18n.loc(
                "WLM-5013: Undeploying service unit : {0} from {1}",
                serviceUnitName, serviceUnitRootPath));
        String retMsg = createSuccessMessage("undeploy", mContext
                .getComponentName());
        mLogger
                .info(I18n
                        .loc(
                                "WLM-5014: Undeployed service unit :  {0} from {1} successfully",
                                serviceUnitName, serviceUnitRootPath));
        return retMsg;
    }

    /**
     * Initialize the deployment. This is the first phase of a two-phase start,
     * where the component must prepare to receive service requests related to
     * the deployment (if any).
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     * @param serviceUnitRootPath -
     *            the full path to the Service Unit artifact root directory
     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        String taskName = "init";
        mLogger.info(I18n.loc(
                "WLM-5015: Initializing service unit : {0} from {1}",
                serviceUnitName, serviceUnitRootPath));
        if (mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "wlmseSum_Init_1", serviceUnitName,
                    "Service Unit has already been deployed", null);
            throw new DeploymentException(extMsg);
        }


        Vector listOfWSDLDefinitions = new Vector();
        /*Task -- WLMEntry*/
        Map<QName, WorkflowMapEntry> wlmMapEntryProvides = new HashMap<QName, WorkflowMapEntry> ();

        try {
            // load SU jbi.xml
            SUDescriptorSupport sud = new SUDescriptorSupport(
                    serviceUnitRootPath);

            //         
            // add provides entry
            Provides[] pds = sud.getProvides();
            Consumes[] cns = sud.getConsumes();            
            // load task model ( Workflow model)
            WorkflowModelManager wfm = new WorkflowModelManager();
            File deploydir = new File(serviceUnitRootPath);

            String[] files = deploydir.list();
            String workflowmapfile = null;
            String wsdlFile = null;
            //parse model, create wlmMapEntry
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                String fnCheck = filename.toLowerCase();
                if (fnCheck.endsWith("wf")) {
                    workflowmapfile = deploydir.getAbsolutePath()
                            + File.separator + filename;
                    Task task = wfm.loadWorkFlowModel(new File(workflowmapfile));
                    processProvider (task, pds, serviceUnitName, wlmMapEntryProvides, mWorkflowMapEntryTable);
                    processConsumer (task, cns, serviceUnitName, mWorkflowMapEntryTable);               
                } 
            }
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                String fnCheck = filename.toLowerCase();
                if (fnCheck.endsWith("wsdl")) {
                    wsdlFile = deploydir.getAbsolutePath() + File.separator
                            + filename;
                    Definition wsdlDef = readWsdl(deploydir, new File(wsdlFile));
                    listOfWSDLDefinitions.add(wsdlDef);
                } else if (fnCheck.endsWith("xhtml")) {
                    wfm.loadXform(new File(deploydir, files[i]), wlmMapEntryProvides);
                } else if (fnCheck.endsWith("instance.xml")) {
                    wfm.loadXFormInstance(new File(deploydir, files[i]), wlmMapEntryProvides);
                }
            }

            for (int ix = 0; ix < pds.length; ix++) {
                Provides p = pds[ix];
                if (pds[ix] == null) {
                    continue;
                }
                QName interfce = p.getInterfaceName();
                String endpoint = p.getEndpointName();
                QName serviceName = p.getServiceName();
 
                    for (int index = 0; index < listOfWSDLDefinitions.size(); index++) {
                        Definition wsdlDef = (Definition) listOfWSDLDefinitions
                                .get(index);
                        if (wsdlDef.getTargetNamespace().equals(
                                interfce.getNamespaceURI())) {
                            WorkflowMapEntry  wfentry = new WorkflowMapEntry(serviceName,
                                    interfce, endpoint, wsdlDef);
                            mWorkflowMapEntryTable.addEntry(wfentry, serviceUnitName);
                            ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(
                                    serviceName, endpoint);

                            wfentry.setService(serviceName);
                            wfentry.setServiceEndpoint(serviceEndpoint);

                            StatusReporting reporting = mStatusProviderHelper
                                    .getStatusReporter();
                            String statusId = mStatusProviderHelper
                                    .createProvisioningEndpointIdentifier(serviceName,
                                            endpoint);
                            reporting.addProvisioningEndpoint(statusId);
                            EndpointStatus endpointStatus = reporting
                                    .getEndpointStatus(statusId);
                            wfentry.setEndpointStatus(statusId, endpointStatus);                            
                        }
                    }
                }
          TaskManager tManager = TaskManagerFactory.getInstance()
          .getTaskManager();
            Collection<WorkflowMapEntry> entries = wlmMapEntryProvides.values();
            for (WorkflowMapEntry workflowMapEntry : entries) {
                

//                List list = mWorkflowMapEntryTable.getEntryList();
//                TaskManager tManager = TaskManagerFactory.getInstance()
//                        .getTaskManager();
//                for (int i = 0, I = list.size(); i < I; i++) {
//                    WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list
//                            .get(i);
//                    QName wrkIntfc = workflowMapEntry.getInterface();
//                    if (interfce.equals(wrkIntfc)) {
//                        ServiceEndpoint serviceEndpoint = mContext
//                                .activateEndpoint(serviceName, endpoint);
//
//                        workflowMapEntry.setService(serviceName);
//                        workflowMapEntry.setServiceEndpoint(serviceEndpoint);
//
//                        StatusReporting reporting = mStatusProviderHelper
//                                .getStatusReporter();
//                        String statusId = mStatusProviderHelper
//                                .createProvisioningEndpointIdentifier(
//                                        serviceName, endpoint);
//                        reporting.addProvisioningEndpoint(statusId);
//                        EndpointStatus endpointStatus = reporting
//                                .getEndpointStatus(statusId);
//                        workflowMapEntry.setEndpointStatus(statusId,
//                                endpointStatus);
                       if (workflowMapEntry.getTaskModel() != null) {
                            tManager.recover(workflowMapEntry.getTaskModel());
                        }

                        mLogger
                                .info(I18n
                                        .loc(
                                                "WLM-5016: Activated workflow {0}, inbound service: {1} ",
                                                workflowMapEntry.getInterface(),
                                                workflowMapEntry.getService()));

                    }

            // add consume entry related
//            processConsumes(sud, wfm, listOfWSDLDefinitions, serviceUnitName);

            // Add serviceUnitName to lookup table
            mDeployedId.add(serviceUnitName);
            wfm = null;

        } catch (ConnectionException e) {
            String extMsg = I18n
            .loc(
                    "WLM-6112: Failed to initialize service unit : {0} from {1} because of SQL Connection Exception",
                    serviceUnitName, serviceUnitRootPath);
                        mLogger.log(Level.WARNING, extMsg, e);
//                    throw new DeploymentException(extMsg, e);
        }   
        catch (Exception e) {
            String extMsg = I18n
                    .loc(
                            "WLM-6012: Failed to initialize service unit : {0} from {1}",
                            serviceUnitName, serviceUnitRootPath);
            mLogger.log(Level.WARNING, extMsg, e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info(I18n.loc(
                "WLM-5017: Initialized service unit {0} from {1} successfully",
                serviceUnitName, serviceUnitRootPath));
    }

    private void processConsumer(Task task, Consumes[] cns,
            String serviceUnitName, WorkflowMapEntryTable workflowMapEntryTable) {
        List<Notification> nList = task.getTaskNotifications();
        Iterator<Notification> it = nList.iterator();
        while (it.hasNext()) {

            EmailNotification n = (EmailNotification) it.next();
            QName portType = n.getPortTypeQName();
            for (int ix = 0; ix < cns.length; ix++) {
                Consumes c = cns[ix];
                QName intName = c.getInterfaceName();
                if (intName.getNamespaceURI()
                        .equals(portType.getNamespaceURI())
                        && intName.getLocalPart().equals(
                                portType.getLocalPart())) {
                    WorkflowMapEntry wfentry = new WorkflowMapEntry(c
                            .getServiceName(), intName, c.getEndpointName(),
                            task, WorkflowMapEntry.EntryType.ENTRY_CONSUME);

                    wfentry.setOperation(n.getOptName());
                    // wfentry.setXform(wfm.getXform(task.getName()));
                    // wfentry.setInputXformInstance(wfm.getInputXformInstance(task
                    // .getName()));
                    // wfentry.setOutputXformInstance(wfm.getOutputXformInstance(task
                    // .getName()));
                    workflowMapEntryTable.addEntry(wfentry, serviceUnitName);
                    StatusReporting reporting = mStatusProviderHelper
                            .getStatusReporter();
                    String statusId = mStatusProviderHelper
                            .createConsumingEndpointIdentifier(c
                                    .getServiceName(), c.getEndpointName());
                    reporting.addConsumingEndpoint(statusId);
                    EndpointStatus endpointStatus = reporting
                            .getEndpointStatus(statusId);
                    wfentry.setEndpointStatus(statusId, endpointStatus);

                    mLogger
                            .info(I18n
                                    .loc(
                                            "WLM-5018: Added consuming service {0}, outbound service: {1}",
                                            wfentry.getInterface(), c
                                                    .getServiceName()));

                    if (wfentry != null) {
                        mWorkflowMapEntryTable.addEntry(wfentry,
                                serviceUnitName);
                    }
                }

            }

        }
    }

    private void processProvider(Task task, Provides[] pds,
            String serviceUnitName,
            Map<QName, WorkflowMapEntry> wlmMapEntryProvides,
            WorkflowMapEntryTable workflowMapEntryTable) throws Exception{
        QName portType = task.getPortTypeQName();
        String optName = task.getOptName();
        for (int ix = 0; ix < pds.length; ix++) {
            Provides p = pds[ix];
            if (pds[ix] == null) {
                continue;
            }
            QName serviceName = p.getServiceName();
            String endpoint = p.getEndpointName();
            QName interfce = p.getInterfaceName();

            WorkflowMapEntry wfentry = null;
            if (interfce.getNamespaceURI().equals(portType.getNamespaceURI())
                    && interfce.getLocalPart().equals(portType.getLocalPart())
                    && endpoint.equals(optName)) {
                wfentry = new WorkflowMapEntry(serviceName, interfce, endpoint,
                        task, WorkflowMapEntry.EntryType.ENTRY_PROVIDE);
                wfentry.setOperation(optName);
                workflowMapEntryTable.addEntry(wfentry, serviceUnitName);
                wlmMapEntryProvides.put(task.getQName(), wfentry);

                ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(
                        serviceName, endpoint);

                wfentry.setService(serviceName);
                wfentry.setServiceEndpoint(serviceEndpoint);

                StatusReporting reporting = mStatusProviderHelper
                        .getStatusReporter();
                String statusId = mStatusProviderHelper
                        .createProvisioningEndpointIdentifier(serviceName,
                                endpoint);
                reporting.addProvisioningEndpoint(statusId);
                EndpointStatus endpointStatus = reporting
                        .getEndpointStatus(statusId);
                wfentry.setEndpointStatus(statusId, endpointStatus);
                pds[ix] = null;
                break;
            }
        }
    }
//
//    private void processConsumes(Task task,
//            Consumes[] cns2, String serviceUnitName,
//            WorkflowMapEntryTable workflowMapEntryTable) throws Exception {
//        Consumes[] cns = task.getConsumes();
//        for (int ix = 0; ix < cns.length; ix++) {
//            Consumes c = cns[ix];
//
//            QName serviceName = c.getServiceName();
//            String endpoint = c.getEndpointName();
//            QName interfce = c.getInterfaceName();
//
//            // Get the task Model from WorkflowModelManager
//            Task taskModel = cns2.getTaskModel(interfce,
//                    endpoint);
//
//            if (taskModel != null) {
//                processTaskNotification(cns2, taskModel, serviceName,
//                        interfce, endpoint, workflowMapEntryTable);
//            }
//
//        }
//
//    }
//
//
//
//    private void processTaskNotification(WorkflowModelManager wfm,
//            Task task, QName serviceName, QName interfce,
//            String endpoint, String serviceUnitName) throws Exception {
//        List<Notification> nList = task.getTaskNotifications();
//        Iterator<Notification> it = nList.iterator();
//        String ns = task.getTargetNamespace();
//        while (it.hasNext()) {
//            // find first notification which has matching partnerLinkType
//            // and end point
//            EmailNotification n = (EmailNotification) it.next();
//            QName portTypeQName = null;
//            PortType pt = n.getPortType();
//            if (pt != null) {
//                portTypeQName = pt.getQName();
//            }
//
//            String operationName =  n.getOptName();
//
//
//            if (portTypeQName.equals(interfce)
//                    && operationName.equals(endpoint)) {
//                WorkflowMapEntry wfentry = new WorkflowMapEntry(serviceName,
//                        interfce, endpoint, task,
//                        WorkflowMapEntry.EntryType.ENTRY_CONSUME);
//
//                // we are finding notification which is for this service end
//                // point
//                // so that we can set the operation name in WorkflowMapEntry
//                wfentry.setOperation(operationName);
//                wfentry.setXform(wfm.getXform(task.getName()));
//                wfentry.setInputXformInstance(wfm.getInputXformInstance(task
//                        .getName()));
//                wfentry.setOutputXformInstance(wfm.getOutputXformInstance(task
//                        .getName()));
//
//                // since status provider is based on service name and endpoint
//                // name
//                // so each portType operation should have a different service
//                // name and end point name
//                StatusReporting reporting = mStatusProviderHelper
//                        .getStatusReporter();
//                String statusId = mStatusProviderHelper
//                        .createConsumingEndpointIdentifier(serviceName,
//                                endpoint);
//                reporting.addConsumingEndpoint(statusId);
//                EndpointStatus endpointStatus = reporting
//                        .getEndpointStatus(statusId);
//                wfentry.setEndpointStatus(statusId, endpointStatus);
//
//                mLogger
//                        .info(I18n
//                                .loc(
//                                        "WLM-5018: Added consuming service {0}, inbound service: {1}",
//                                        wfentry.getInterface(), serviceName));
//
//                if (wfentry != null) {
//                    mWorkflowMapEntryTable.addEntry(wfentry, serviceUnitName);
//                }
//
//                break;
//            }
//        }
//
//    }

    // private void readAllDefinitions(File asaDir, String id, String taskName,
    // String locToken) throws DeploymentException {
    // /*
    // CatalogManager catalogManager = new CatalogManager();
    // catalogManager.setCatalogFiles(asaDir.getAbsolutePath() + File.separator
    // + "xml-catalog.xml");
    // catalogManager.setRelativeCatalogs(true);
    // EntityResolver resolver = new CatalogResolver(catalogManager);
    // */
    // EntityResolver resolver = null;
    // List wsdls = listWSDLFiles(asaDir);
    // File[] wsdlFiles = (File[]) wsdls.toArray(new File[0]);
    //
    // // read all wsdl files to see if
    // if (wsdlFiles != null ) {
    // for (int i = 0; i < wsdlFiles.length; i ++) {
    // try {
    // Definition def = readWsdl(wsdlFiles[i]);
    // mWsdlMap.put(def.getTargetNamespace(), def);
    // mLogger.fine(I18n.loc("WLM-3012: Added WSDL {0}, file: {1}",
    // def.getTargetNamespace() , wsdlFiles[i].getName()));
    // } catch (Exception e) {
    // String msg = "Unable to read WSDL file " + wsdlFiles[i] + " : " +
    // e.getMessage();
    // String exMsg = createExceptionMessage(mContext.getComponentName(),
    // taskName, "FAILED", locToken, id, msg , e);
    // throw new DeploymentException(exMsg, e);
    // }
    // }
    // }
    // }

    private List listWSDLFiles(File currentDir) {
        List cumulativeResults = new ArrayList();
        File[] filesInCurrentDir = currentDir.listFiles();
        for (int fileCount = 0; fileCount < filesInCurrentDir.length; fileCount++) {
            if (filesInCurrentDir[fileCount].isFile()) {
                if (filesInCurrentDir[fileCount].getName().toLowerCase()
                        .endsWith(".wsdl")) {
                    cumulativeResults.add(filesInCurrentDir[fileCount]);
                }
            } else if (filesInCurrentDir[fileCount].isDirectory()) {
                List wsdlsInSubDirectories = listWSDLFiles(filesInCurrentDir[fileCount]);
                cumulativeResults.addAll(wsdlsInSubDirectories);
            }
        }
        return cumulativeResults;
    }

    private Definition readWsdl(File root, File f)
            throws javax.wsdl.WSDLException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        WSDLReader reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader();
        Definition def = null;

        try {
            def = reader.readWSDL(f.toURI().toString());
            def.setDocumentBaseURI(f.getParentFile().toURI().toString());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new WSDLException("WLM-6104", I18n.loc(
                    "WLM-6104: Unable to read WSDL defintion {0}", f
                            .getAbsolutePath()), e);
        }
        return def;
    }

    /**
     * Shut down the deployment. This causes the deployment to return to the
     * state it was in after deploy() and before init().
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        String taskName = "init";
        mLogger.info(I18n.loc("WLM-5019: Shutting down service unit  : {0}",
                serviceUnitName));
        if (!mDeployedId.contains(serviceUnitName)) {
            String extMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "wlmseSum_ShutDown_1", serviceUnitName,
                    "Service Unit has not been deployed", null);
            throw new DeploymentException(extMsg);
        }
        try {
            // Remove serviceUnitName from lookup table
            mDeployedId.remove(serviceUnitName);
            StatusReporting reporting = mStatusProviderHelper
                    .getStatusReporter();
            List list = mWorkflowMapEntryTable
                    .getEntryListByServiceUnitName(serviceUnitName);
            for (int i = 0, I = list.size(); i < I; i++) {
                WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list
                        .get(i);
                if (workflowMapEntry.getEntryType() == WorkflowMapEntry.EntryType.ENTRY_PROVIDE) {
                    if (workflowMapEntry.getServiceUnitNames().size() == 1) {
                        ServiceEndpoint serviceEndpoint = workflowMapEntry
                                .getServiceEndpoint();
                        if (serviceEndpoint != null) {
                            mContext.deactivateEndpoint(serviceEndpoint);
                        }
                        String endpointStatusId = workflowMapEntry
                                .getEndpointStatusId();
                        if (endpointStatusId != null) {
                            reporting
                                    .removeProvisioningEndpoints(new String[] { endpointStatusId });
                        }
                    }
                } else {
                    if (workflowMapEntry.getServiceUnitNames().size() == 1) {
                        String outEndpointStatusId = workflowMapEntry
                                .getEndpointStatusId();
                        if (outEndpointStatusId != null
                                && workflowMapEntry.getServiceUnitNames()
                                        .size() == 1) {
                            reporting
                                    .removeConsumingEndpoints(new String[] { outEndpointStatusId });
                        }
                    }
                }

                Task task = workflowMapEntry.getTaskModel();
                if (task != null) {;
                    String taskDefName = task.getName();
                    QName qName = new QName(task.getTargetNamespace(),
                            taskDefName);
                    mEngine.removeTaskStateListeners(qName);
                }
                mWorkflowMapEntryTable.removeEntry(workflowMapEntry,
                        serviceUnitName);

            }
        } catch (Exception e) {
            String extMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "WorkflowSeSum_ShutDown_2",
                    serviceUnitName, "Service Unit shutDown error", e);
            throw new DeploymentException(extMsg, e);
        }
        mLogger.info(I18n.loc(
                "WLM-5020: Shut down service unit  : {0} successfully",
                serviceUnitName));
    }

    /**
     * Start the deployment. This is the second phase of a two-phase start,
     * where the component can now initiate service requests related to the
     * deployment.
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void start(String serviceUnitName) throws DeploymentException {
        mLogger.info(I18n.loc("WLM-5021: Starting service unit  : {0}",
                serviceUnitName));
//        List list = mWorkflowMapEntryTable
//                .getEntryListByServiceUnitName(serviceUnitName);
//        for (int i = 0, I = list.size(); i < I; i++) {
//            WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list.get(i);
//            workflowMapEntry.setStarted(true);
//        }
        // Test Component.getServiceDescription
//        try {
//            List<WorkflowMapEntry> entries = mWorkflowMapEntryTable
//                    .getEntryListByServiceUnitName(serviceUnitName);
//            for (WorkflowMapEntry entry : entries) {
//                if (entry.getEntryType() == EntryType.ENTRY_PROVIDE) {
//                    ServiceEndpoint se = entry.getServiceEndpoint();
//                    Document doc;
//                    doc = mContext.getEndpointDescriptor(se);
//                    String xml = XmlUtil.toXmlPretty(doc.getDocumentElement(),
//                            "UTF-8", true);
//                    mLogger.info("EndpointDescriptor:" + xml);
//                }
//            }
//        } catch (Exception e) {
//            throw new DeploymentException(e);
//        }
        mLogger.info(I18n.loc(
                "WLM-5022: Started service unit  : {0} successfully",
                serviceUnitName));
    }

    /**
     * Stop the deployment. This causes the component to cease generating
     * service requests related to the deployment. This returns the deployment
     * to a state equivalent to after init() was called
     * 
     * @param serviceUnitName -
     *            the name of the Service Unit being deployed.
     */
    public void stop(java.lang.String serviceUnitName)
            throws DeploymentException {
        mLogger.info(I18n.loc("WLM-5023: Stoping service unit  : {0}",
                serviceUnitName));
        List list = mWorkflowMapEntryTable
                .getEntryListByServiceUnitName(serviceUnitName);
        for (int i = 0, I = list.size(); i < I; i++) {
            WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list.get(i);
            if (workflowMapEntry.getServiceUnitNames().size() == 1) {
                workflowMapEntry.setStarted(false);
            }
        }
        mLogger.info(I18n.loc(
                "WLM-5024: Stopped service unit  : {0} successfully",
                serviceUnitName));
    }

    public WorkflowMapEntryTable getEntryTable() {
        return mWorkflowMapEntryTable;

    }

}
