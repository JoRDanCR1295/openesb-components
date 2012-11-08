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
import java.util.Map;
import java.util.logging.Logger;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask.TaskState;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskDocument;
import com.sun.jbi.workflow.taskcommon.xmlbeans.TaskType;

/**
 * 
 * @author radval
 */
public class GetTaskOperation extends AbstractStaticOperation {

    private static final String TASK_ITEM_PART = "taskItem";

    private static final String PRINCIPAL_DELIMETER = ",";
    
    private static final Logger LOGGER = Logger.getLogger(GetTaskOperation.class
            .getName());    

    /** Creates a new instance of GetTaskListOperation */
    public GetTaskOperation(Operation operation, Element input, Subject subject,
            StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
//        long before = System.currentTimeMillis();
        StaticOperationReply reply = new StaticOperationReply ();     
        TaskItem taskItem = null;
        // invoke getTaskList method
        try {
            Element input = getInput();
            
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());

            taskItem =   getTaskManagementService ().getTask(taskid);
            

            // convert result (ie. taskItems) to xml element
            Element result = null;
            try {
                result = createResult(taskItem);
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

    private Element createResult(TaskItem taskItem) throws Exception {
        com.sun.jbi.workflow.taskcommon.xmlbeans.TaskDocument taskdoc = TaskDocument.Factory.newInstance();
        if (taskItem != null) {
        TaskType taskType = taskdoc.addNewTask();
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
        
        
        Node  element = taskdoc.getDomNode().getFirstChild();
        Document doc = XmlUtil.createDocument(true);
        Node newElement = doc.importNode(element, true);
        doc.appendChild(newElement);
        
        Map<String, Element> partsMap = new HashMap<String, Element>();
        partsMap.put(TASK_ITEM_PART, doc.getDocumentElement());
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
