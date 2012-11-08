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
 * @(#)GetTaskListOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.opt.DBOperation;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskState;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.taskcommon.xmlbeans.QueryType;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskListDocument;

/**
 * 
 * @author radval
 */
public class GetTaskListOperation extends AbstractStaticOperation {

    private static final String TASK_LIST_PART = "taskList";

    private static final String PRINCIPAL_DELIMETER = ",";
    
    private static final Logger LOGGER = Logger.getLogger(GetTaskListOperation.class
            .getName());    

    /** Creates a new instance of GetTaskListOperation */
    public GetTaskListOperation(Operation operation, Element input, Subject subject,
            StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
//        long before = System.currentTimeMillis();
        StaticOperationReply reply = new StaticOperationReply ();     
        TasklistResult taskItems = null;
        // invoke getTaskList method
        try {
            Element input = getInput();
            
            QueryType query = ClientOperationsHelper.extractQuery(input, getWSDLOperation());
            int startIndex = ClientOperationsHelper.extractStartIndex (input, getWSDLOperation());
            int perPage = ClientOperationsHelper.extractPageSize (input, getWSDLOperation());
            
            if (query != null  && query.getType() != null && query.getType().toString().equals(QueryType.Type.FILTERED.toString())) {
                taskItems = getTaskManagementService ().getTaskFilteredList(query, getTaskSubject(), startIndex, perPage);
               // taskItems = getTaskManagementService ().getTaskList(query, startIndex,perPage);
            } else {          
                taskItems = getTaskManagementService().getTaskList(query, getTaskSubject(), startIndex, perPage);
            }

            // convert result (ie. taskItems) to xml element
            Element result = null;
            try {
                result = createResult(taskItems);
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            reply.setReply(result);
            // set output
            setOutput(reply);
//            long after = System.currentTimeMillis();
//            long duration = after - before;
//            LOGGER.log(Level.INFO, "GetTaskList Duration:" + duration);
        } catch (WorkflowException e) {
            // TODO Auto-generated catch block
            reply.setFaulted(true);
            reply.setFault(buildFault(e));
            setOutput(reply);
        }
    }

    private Element createResult(TasklistResult taskListResult) throws Exception {
        TaskListDocument taskdoc = TaskListDocument.Factory.newInstance();
        taskdoc.addNewTaskList();
        List<TaskItem> taskItems = taskListResult.getTaskList();
        com.sun.jbi.workflow.taskcommon.xmlbeans.TaskListType taskList  = taskdoc.getTaskList();
        if (taskItems != null && taskItems.size() > 0) {
            for (TaskItem taskItem : taskItems) {
                com.sun.jbi.workflow.taskcommon.xmlbeans.TaskType taskType = taskList
                        .addNewTask();
                taskType.setTaskId(taskItem.getId());
                taskType.setAssignedTo(taskItem.getAssignedTo());
                if (taskItem.getClaimedBy() != null) {
                    taskType.setClaimedBy(taskItem.getClaimedBy());
                }                
                taskType.setPriority(taskItem.getPriority());
                taskType.setStatus(makeStatus(taskItem.getState()));
                taskType.setSubmittedDate(taskItem.getCreatedDate());
                taskType.setTitle(taskItem.getDescription());
                if (taskItem.getKeywords() != null) {
                    taskType.setKeywords(taskItem.getKeywords());
                }                
                taskType.setTaskDefId(taskItem.getTaskDefId());
                if (taskItem.getDeadline() != null && ! Util.isInitialDat(taskItem.getDeadline())) {
                    taskType.setDeadline(taskItem.getDeadline());
                }
                if (taskItem.getCompletedBy() != null) {
                    taskType.setCompletedBy(taskItem.getCompletedBy());
                }
            }
        }
        taskList.setReturnedRecords(taskListResult.getReturnedRecords());
        taskList.setTotalRecords(taskListResult.getTotalRecords());
        //create a new document. this is because
        //XmlBeans implementation of Element/Node does not
        //support isSameNode api and throws exception and this is used in com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder
        //
        
        Element  element = (Element) taskdoc.getTaskList().getDomNode();
        Document doc = XmlUtil.createDocument(true);
        Element newElement = (Element) doc.importNode(element, true);
        doc.appendChild(newElement);
        
        Map<String, Element> partsMap = new HashMap<String, Element>();
        partsMap.put(TASK_LIST_PART, doc.getDocumentElement());
        return JBIMessageUtil.makeJBIMessage(partsMap, getWSDLOperation());

    }

    private com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.Enum makeStatus(TaskState state) {
        
        if (state == RuntimeTask.TaskState.UNASSIGNED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.UNASSIGNED;
        } else if (state == RuntimeTask.TaskState.ASSIGNED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ASSIGNED;
        } else if (state == RuntimeTask.TaskState.CLAIMED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.CLAIMED;
        } else if (state == RuntimeTask.TaskState.COMPLETED) {
            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.COMPLETED;
        }  else if(state == RuntimeTask.TaskState.EXPIRED) {
        	return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.EXPIRED;
        } else if(state == RuntimeTask.TaskState.ESCALATED) {
        	return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ESCALATED;
        } else if(state == RuntimeTask.TaskState.ABORTED) {
        	return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.ABORTED;
        } else if(state == RuntimeTask.TaskState.FAILED) {
        	return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskStatus.FAILED;
        }
        return null;
    }

//    private Enum makePriority(
//            com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskPriority priority) {
//        // TODO Auto-generated method stub
//        if (priority == RuntimeTask.TaskPriority.HIGH) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.HIGH;
//        } else if (priority == RuntimeTask.TaskPriority.MEDIUM) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.MEDIUM;
//        } else if (priority == RuntimeTask.TaskPriority.LOW) {
//            return com.sun.jbi.workflow.taskcommon.xmlbeans.TaskPriority.LOW;
//        }
//        return null;
//    }

    private String makeString(TaskPrincipal claimedBy) {
        // TODO Auto-generated method stub
        return claimedBy == null ? "": claimedBy.getName();
       }

    private String makeString(Collection<TaskPrincipal> assignedTo) {
        // TODO Auto-generated method stub
        String returnStr = "";
        if (assignedTo != null && assignedTo.size() > 0) {
            StringBuffer buffer = new StringBuffer();
            int i = 0;
            for (TaskPrincipal principal : assignedTo) {
                buffer.append(principal.getName());
                if (i < assignedTo.size() - 1) {
                    buffer.append(PRINCIPAL_DELIMETER);
                    i++;
                }
                
            }
            returnStr = buffer.toString();
        }
        return returnStr;
    }

}
