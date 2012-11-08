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
 * @(#)$Id: WLMSEServiceUnitManager.java,v 1.1 2010/02/15 19:25:10 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;

import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.DefaultServiceUnitManager;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.WorkflowMapEntryTable;
import com.sun.jbi.engine.workflow.WorkflowModelManager;
import com.sun.jbi.engine.workflow.db.connection.ConnectionException;
import com.sun.jbi.engine.workflow.runtime.model.TaskManager;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerFactory;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.workflow.model.EmailNotification;
import com.sun.jbi.workflow.model.Notification;
import com.sun.jbi.workflow.model.Task;

public class WLMSEServiceUnitManager extends DefaultServiceUnitManager {

    private WorkflowEngine mEngine;

    public WLMSEServiceUnitManager(ManagerContext mgrCtx) {
        super(mgrCtx);
        // TODO Auto-generated constructor stub
    }

    public void setEngine(WorkflowEngine engine) {
        mEngine = engine;
    }

    protected ServiceUnit installServiceUnit(String serviceUnitName,
            String serviceUnitRootPath) throws DeploymentException {
        ServiceUnit srvcUnit = createServiceUnit(serviceUnitName,
                serviceUnitRootPath);
        getServiceUnits().register(serviceUnitName, srvcUnit);
        // getManagerContext().getEndpointManager().addServiceUnit(srvcUnit);
        return srvcUnit;
    }

    /**
     * @see javax.jbi.component.ServiceUnitManager#init(java.lang.String,
     *      java.lang.String)
     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
            throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine(
                    "COMPTK-3003: Initializing SU \"" + serviceUnitName
                            + "\" at root path: " + serviceUnitRootPath);
        }
        WorkflowMapEntryTable wlmEntryTable = getEntryTable();

        ServiceUnit srvcUnit = null;
        if (!isDeployed(serviceUnitName)) {
            /*
             * JBI javadoc says to throw exception. Does not account for AS
             * shutdown, since SUs are stored in memory and lost at that time.
             * For now, adding service unit instance to registry.
             */
            srvcUnit = installServiceUnit(serviceUnitName, serviceUnitRootPath);
        }

        try {
            getManagerContext().getMessagingChannel().installServiceQualities(
                    serviceUnitName, serviceUnitRootPath);

            if (srvcUnit == null)
                srvcUnit = getServiceUnits().lookup(serviceUnitName);

            // // Do model related
            Vector listOfWSDLDefinitions = new Vector();
            
            /* Task -- WLMEntry */
            Map<QName, WorkflowMapEntry> wlmMapEntryProvides = new HashMap<QName, WorkflowMapEntry>();

            EndpointInfo[] pds = srvcUnit.getServices().getProvides();
            EndpointInfo[] cns = srvcUnit.getServices().getConsumes();

            // load task model ( Workflow model)
            WorkflowModelManager wfm = new WorkflowModelManager();
            File deploydir = new File(serviceUnitRootPath);

            String[] files = deploydir.list();
            String workflowmapfile = null;
            String wsdlFile = null;
            // parse model, create wlmMapEntry
            for (int i = 0; i < files.length; i++) {
                String filename = files[i];
                String fnCheck = filename.toLowerCase();
                if (fnCheck.endsWith("wf")) {
                    workflowmapfile = deploydir.getAbsolutePath()
                            + File.separator + filename;
                    Task task = wfm
                            .loadWorkFlowModel(new File(workflowmapfile));
                    processProvider(task, pds, serviceUnitName,
                            wlmMapEntryProvides, wlmEntryTable);
                    processConsumer(task, cns, serviceUnitName, wlmEntryTable);
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
                    wfm.loadXform(new File(deploydir, files[i]),
                            wlmMapEntryProvides);
                } else if (fnCheck.endsWith("instance.xml")) {
                    wfm.loadXFormInstance(new File(deploydir, files[i]),
                            wlmMapEntryProvides);
                }
            }

            for (int ix = 0; ix < pds.length; ix++) {
                EndpointInfo p = pds[ix];
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
                        WorkflowMapEntry wfentry = new WorkflowMapEntry(
                                serviceName, interfce, endpoint, wsdlDef);
                        wlmEntryTable.addEntry(wfentry, serviceUnitName);

                        wfentry.setService(serviceName);
                    }
                }
            }
            TaskManager tManager = TaskManagerFactory.getInstance()
                    .getTaskManager();
            Collection<WorkflowMapEntry> entries = wlmMapEntryProvides.values();

            for (WorkflowMapEntry workflowMapEntry : entries) {

                if (workflowMapEntry.getTaskModel() != null) {
                    tManager.recover(workflowMapEntry.getTaskModel());
                }

                log()
                        .info(
                                I18n
                                        .loc(
                                                "WLM-5016: Activated workflow {0}, inbound service: {1} ",
                                                workflowMapEntry.getInterface(),
                                                workflowMapEntry.getService()));

            }

            wfm = null;

            /* START PROVISIONING */
            getManagerContext().getEndpointManager().getLifeCycle()
                    .startProvisioning(srvcUnit);

        } catch (ConnectionException e) {
            String extMsg = I18n
                    .loc(
                            "WLM-6112: Failed to initialize service unit : {0} from {1} because of SQL Connection Exception",
                            serviceUnitName, serviceUnitRootPath);
            log().log(Level.WARNING, extMsg, e);

        } catch (Exception e) {
            String extMsg = I18n
                    .loc(
                            "WLM-6012: Failed to initialize service unit : {0} from {1}",
                            serviceUnitName, serviceUnitRootPath);
            log().log(Level.WARNING, extMsg, e);
            throw new DeploymentException(extMsg, e);
        }

        log()
                .info(
                        I18n
                                .loc(
                                        "WLM-5017: Initialized service unit {0} from {1} successfully",
                                        serviceUnitName, serviceUnitRootPath));
    }

    /** @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String) */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log()
                    .fine(
                            "COMPTK-3005: Shutting down SU \""
                                    + serviceUnitName + "\"");
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(
                    I18n
                            .loc(
                                    "COMPTK-6019: shutDown failed for undeployed service unit \"{0}\"",
                                    serviceUnitName));
        }

        try {

            WorkflowMapEntryTable entryTable = getEntryTable();
            List list = entryTable
                    .getEntryListByServiceUnitName(serviceUnitName);

            for (int i = 0, I = list.size(); i < I; i++) {
                WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) list
                        .get(i);

                Task task = workflowMapEntry.getTaskModel();
                if (task != null) {
                    String taskDefName = task.getName();
                    QName qName = new QName(task.getTargetNamespace(),
                            taskDefName);
                    mEngine.removeTaskStateListeners(qName);
                }
                entryTable.removeEntry(workflowMapEntry, serviceUnitName);
            }

            ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
            getManagerContext().getMessagingChannel()
                    .uninstallServiceQualities(srvcUnit.getName());
            /* STOP PROVISIONING */
            if (hasEndpointLifeCycle()) {
                getManagerContext().getEndpointManager().getLifeCycle()
                        .stopProvisioning(srvcUnit);
            }
        } catch (Exception e) {
            throw new DeploymentException(createFailedMessage(INIT_TASK,
                    "COMPTK_INIT_1", e.getMessage()), e);
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine(
                    "COMPTK-3006: Shut down successful for SU-"
                            + serviceUnitName);
        }
    }

    private void processProvider(Task task, EndpointInfo[] pds,
            String serviceUnitName,
            Map<QName, WorkflowMapEntry> wlmMapEntryProvides,
            WorkflowMapEntryTable workflowMapEntryTable) throws Exception {
        QName portType = task.getPortTypeQName();
        String optName = task.getOptName();
        for (int ix = 0; ix < pds.length; ix++) {
            EndpointInfo p = pds[ix];
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
                pds[ix] = null;
                break;
            }
        }
    }

    private void processConsumer(Task task, EndpointInfo[] cns,
            String serviceUnitName, WorkflowMapEntryTable workflowMapEntryTable) {
        List<Notification> nList = task.getTaskNotifications();
        Iterator<Notification> it = nList.iterator();
        while (it.hasNext()) {

            EmailNotification n = (EmailNotification) it.next();
            QName portType = n.getPortTypeQName();
            for (int ix = 0; ix < cns.length; ix++) {
                EndpointInfo c = cns[ix];
                QName intName = c.getInterfaceName();
                if (intName.getNamespaceURI()
                        .equals(portType.getNamespaceURI())
                        && intName.getLocalPart().equals(
                                portType.getLocalPart())) {
                    WorkflowMapEntry wfentry = new WorkflowMapEntry(c
                            .getServiceName(), intName, c.getEndpointName(),
                            task, WorkflowMapEntry.EntryType.ENTRY_CONSUME);

                    wfentry.setOperation(n.getOptName());
                    workflowMapEntryTable.addEntry(wfentry, serviceUnitName);

                    log()
                            .info(
                                    I18n
                                            .loc(
                                                    "WLM-5018: Added consuming service {0}, outbound service: {1}",
                                                    wfentry.getInterface(), c
                                                            .getServiceName()));

                    if (wfentry != null) {
                        workflowMapEntryTable
                                .addEntry(wfentry, serviceUnitName);
                    }
                }

            }

        }
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

    private WorkflowMapEntryTable getEntryTable() {
        return ((WLMSEManagerContext) getManagerContext()).getEntryTable();
    }

}
