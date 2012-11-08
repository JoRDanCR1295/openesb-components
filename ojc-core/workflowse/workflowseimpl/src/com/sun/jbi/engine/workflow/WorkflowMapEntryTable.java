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
 * @(#)WorkflowMapEntryTable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import java.util.*;
import javax.xml.namespace.QName;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.engine.workflow.WorkflowMapEntry.EntryType;
import com.sun.jbi.workflow.model.Task;


public class WorkflowMapEntryTable {
    private List<WorkflowMapEntry> mEntryList;

    private Map<QName, Task> mTaskModelMap = new HashMap<QName, Task>();

    private Map<Task, WorkflowMapEntry> mTasksEntryMap = new HashMap<Task, WorkflowMapEntry>();

    /** Creates a new instance of WorkflowMapEntryTable */
    public WorkflowMapEntryTable() {
        mEntryList = new ArrayList();
    }

    public WorkflowMapEntry findWorkflowEntry(ServiceEndpoint se) {
        QName service = se.getServiceName();
        // Search by operation's fullname and service's fullname
        for (Iterator i = mEntryList.iterator(); i.hasNext();) {
            WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) i.next();
            if (workflowMapEntry.isStarted()) {
                // QName entryInterface = workflowMapEntry.getInterface();
//                ServiceEndpoint seWrk = workflowMapEntry.getServiceEndpoint();
                QName entryService = workflowMapEntry.getService();
                String endPoint = workflowMapEntry.getEndpoint();
                
                if (se.getEndpointName().equals(endPoint)
                            && entryService.getLocalPart().equals(
                                    service.getLocalPart())
                            && entryService.getNamespaceURI().equals(
                                    service.getNamespaceURI())) {
                        return workflowMapEntry;
                    }
                }
            }
        return null;
    }
    
    public WorkflowMapEntry findWorkflowEntry(QName service, String endPoint) {
        if (endPoint != null && service != null) {

            for (Iterator i = mEntryList.iterator(); i.hasNext();) {
                WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) i.next();
                QName svc = workflowMapEntry.getService();
                String ep = workflowMapEntry.getEndpoint() ;
                if (endPoint.equals(ep) 
                        && service.equals(svc)) {
                    return workflowMapEntry;
                }
            }
        }

        return null;
    }

    public WorkflowMapEntry findWorkflowEntry(Task task) {
        // Search by operation's fullname and service's fullname
        return mTasksEntryMap.get(task);
    }

    public WorkflowMapEntry findWorkflowEntry(String optName,
            QName portTypeQName) {
        if (optName != null && portTypeQName != null) {

            for (Iterator i = mEntryList.iterator(); i.hasNext();) {
                WorkflowMapEntry workflowMapEntry = (WorkflowMapEntry) i.next();
                QName entryInterface = workflowMapEntry.getInterface();
                String endPoint = workflowMapEntry.getEndpoint() ;
                String operation = workflowMapEntry.getOperation();
                String opt = operation != null ? operation : endPoint;
                if (optName.equals(opt) 
                        && portTypeQName.equals(entryInterface)) {
                    return workflowMapEntry;
                }
            }
        }

        return null;
    }

    public void addEntry(WorkflowMapEntry entry, String serviceUnitName) {
        // Loop through existing entries and
        // see if there is match

        if (!mEntryList.contains(entry)) {
            boolean found = false;
            for (WorkflowMapEntry myEntry : mEntryList) {
                if ((myEntry.getInterface() != null && myEntry.getInterface()
                        .equals(entry.getInterface()))
                        && (myEntry.getService() != null && myEntry
                                .getService().equals(entry.getService()))) {
                    myEntry.getServiceUnitNames().add(serviceUnitName);
                    found = true;
                    break;
                }
            }
            if (!found) {
                entry.getServiceUnitNames().add(serviceUnitName);
                mEntryList.add(entry);
            }
        }
        Task task = entry.getTaskModel();
        if (task != null && entry.getEntryType() == EntryType.ENTRY_PROVIDE) {
            String taskName = task.getName();
            QName qName = new QName(task.getTargetNamespace(), taskName);
            mTaskModelMap.put(qName, task);
            mTasksEntryMap.put(task, entry);
        }

    }

    public void removeEntry(WorkflowMapEntry entry, String serviceUnit) {
        if (entry.getServiceUnitNames().size() > 1) {
            entry.getServiceUnitNames().remove(serviceUnit);
        } else {
            mEntryList.remove(entry);
        }
        Task task = entry.getTaskModel();
        if (task != null) {
            String taskName = task.getName();
            QName qName = new QName(task.getTargetNamespace(), taskName);
            mTaskModelMap.remove(qName);
            mTasksEntryMap.remove(task);
        }

    }

    public List<WorkflowMapEntry> getEntryListByServiceUnitName(
            String serviceUnitName) {
        ArrayList list = new ArrayList();
        for (int i = 0, I = mEntryList.size(); i < I; i++) {
            WorkflowMapEntry entry = (WorkflowMapEntry) mEntryList.get(i);
            if (entry.getServiceUnitNames().contains(serviceUnitName)) {
                list.add(entry);
            }
        }
        return list;
    }

    public List getEntryList() {
        return new ArrayList(mEntryList);
    }

    /**
     * Returns Tasks model
     * 
     * @param taskQName
     *            {targetNamespace}taskName
     * @return
     */
    public Task getTasksModel(QName taskQName) {
        return mTaskModelMap.get(taskQName);
    }

    public void setTaskModelMap(Map<QName, Task> taskModelMap) {
        mTaskModelMap = taskModelMap;
    }

    public Map<QName, Task> getTaskModelMap() {
        return mTaskModelMap;
    }
}
