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
 * @(#)TaskManagerHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.ClientApiException;
import com.sun.jbi.engine.workflow.clientapi.TaskItem;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.db.hibernate.TaskAssignee;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.workflow.model.Assignment;
import com.sun.jbi.workflow.model.Task;

public class TaskManagerHelper {
    private static final Logger LOGGER = Logger
            .getLogger(TaskManagerHelper.class.getName());

//    private static final String TASK_INPUT_WRAPPER = "taskInputWrapper";

    private static final String PRINCIPAL_DELIMETER = ",";

    /**
     * Given the task extract the input
     */
    public static Element extractTaskInputFromJBIWrapper(Task taskMeta,
            Element inputEl) throws TaskException {

        NodeList nodelist = null;
        Element returnEl = null;
        
        Element docRoot = null;

        if (inputEl == null) {
            throw new TaskException("TaskManagerHelper.No_Input_Is_Set", I18n
                    .loc("WLM-6091: No input data is set for task name : {0}",
                            taskMeta.getName()));
        }

        Operation wsdlOpt = null;
        Document doc = null;
        try {
            wsdlOpt = taskMeta.getWSDLOperation();
            nodelist = extractTaskInputFromTask(inputEl, wsdlOpt);
            doc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .newDocument();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc(
                    "WLM-6092: Failed to get input data for task name : {0}",
                    taskMeta.getName());
            LOGGER.log(Level.WARNING, msg, e);
            throw new TaskException("TaskManagerHelper.Fail_To_GetTaskInput",
                    msg, e);
        }
        Message msg = wsdlOpt.getInput().getMessage();
        Map partMap = msg.getParts();
        Iterator mapIter = partMap.values().iterator();
        Part part = (Part) mapIter.next();
        if (JBIMessageUtil.isElement(part)) {
            for (int i = 0; i < nodelist.getLength(); i++) {
                Node node = nodelist.item(i);
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    returnEl = (Element) node;
                }
            }
        } else {
            for (int i = 0; i < nodelist.getLength(); i++) {
                Node node = nodelist.item(i);
                if (docRoot == null) {
                    docRoot = (Element) doc.importNode(inputEl.getOwnerDocument().getDocumentElement(), false);
                } 
                docRoot.appendChild(doc.importNode(node, true));
            }    
        }
          if (docRoot == null) {
            docRoot = (Element) doc.importNode(returnEl, true);          
        }
         doc.appendChild(docRoot);
        return docRoot;

    }

    /**
     * Extract the content of the input message set from task, wrapp the content
     * within an element "content"
     * 
     * @param source
     * @param operation
     * @return
     * @throws ClientApiException
     */
    private static NodeList extractTaskInputFromTask(Element source,
            Operation operation) throws WorkflowException {
        Document normalDoc = source.getOwnerDocument();
        NodeList nodeList = null;

        try {
            Element normalRoot = normalDoc.getDocumentElement();
            Input input = operation.getInput();
            Message inputMessage = input.getMessage();

            WrapperParser wrapperParser = HelperFactory.createParser();

            wrapperParser.parse(normalDoc, inputMessage);

            String[] parts = wrapperParser.getPartNames();

            nodeList = wrapperParser.getPartNodes(parts[0]);

        } catch (Throwable th) {
            String msg = I18n.loc("WLM-6071: Failed to normalize the message");
            throw new WorkflowException("TaskManagerHelper.FailedToNormalize",
                    msg, th);
        }
        return nodeList;
    }

    public static Collection<TaskPrincipal> getTaskPrincipalsFromWorkflowDefinition(
            RuntimeTask task) throws TaskException {
        List<TaskPrincipal> principals = new ArrayList<TaskPrincipal>();

        try {
            Task taskMetaData = task.getTaskMeta();
            if (taskMetaData != null) {
                Assignment a = taskMetaData.getTaskAssignment();
                if (a != null) {

                    List<String> users = a.getUsers(task.getJXpathContext());
                    List<String> groups = a.getGroups(task.getJXpathContext());

                    if (users != null) {
                        Iterator<String> it = users.iterator();
                        while (it.hasNext()) {
                            String user = it.next();
                            TaskPrincipal principal = TaskModelFactory
                                    .getInstance().createPrincipal(user,
                                            TaskPrincipal.PrincipalType.User);
                            principals.add(principal);
                        }
                    }
                    if (groups != null) {
                        Iterator<String> it = groups.iterator();
                        while (it.hasNext()) {
                            String group = it.next();
                            TaskPrincipal principal = TaskModelFactory
                                    .getInstance().createPrincipal(group,
                                            TaskPrincipal.PrincipalType.Group);
                            principals.add(principal);
                        }
                    }

                    if (users == null && groups == null) {
                        throw new TaskException(
                                "TaskManagerHelper.No_valid_assignment_available",
                                I18n
                                        .loc(
                                                "WLM-6096: There is no valid assignment available in task definition for task name : {0}",
                                                taskMetaData.getName()));
                    }
                } else {
                    throw new TaskException(
                            "TaskManagerHelper.No_valid_assignment_available",
                            I18n
                                    .loc(
                                            "WLM-6096: There is no valid assignment available in task definition for task name : {0}",
                                            taskMetaData.getName()));
                }
            }

        } catch (Exception ex) {
            throw new TaskException(ex);
        }

        return principals;
    }

    public static Collection<TaskPrincipal> getExcludedTaskPrincipalsFromWorkflowDefinition(
            RuntimeTask task) throws TaskException {
        List<TaskPrincipal> principals = new ArrayList<TaskPrincipal>();

        try {
            Task taskMetaData = task.getTaskMeta();
            if (taskMetaData != null) {
                Assignment a = taskMetaData.getTaskAssignment();
                if (a != null) {

                    List<String> users = a.getExcludedUsers(task.getJXpathContext());
                    List<String> groups = a.getExcludedGroups(task.getJXpathContext());

                    if (users != null) {
                        Iterator<String> it = users.iterator();
                        while (it.hasNext()) {
                            String user = it.next();
                            TaskPrincipal principal = TaskModelFactory
                                    .getInstance().createPrincipal(user,
                                            TaskPrincipal.PrincipalType.User);
                            principals.add(principal);
                        }
                    }
                    if (groups != null) {
                        Iterator<String> it = groups.iterator();
                        while (it.hasNext()) {
                            String group = it.next();
                            TaskPrincipal principal = TaskModelFactory
                                    .getInstance().createPrincipal(group,
                                            TaskPrincipal.PrincipalType.Group);
                            principals.add(principal);
                        }
                    }

                    if (users == null && groups == null) {
                        throw new TaskException(
                                "TaskManagerHelper.No_valid_assignment_available",
                                I18n
                                        .loc(
                                                "WLM-6096: There is no valid assignment available in task definition for task name : {0}",
                                                taskMetaData.getName()));
                    }
                } else {
                    throw new TaskException(
                            "TaskManagerHelper.No_valid_assignment_available",
                            I18n
                                    .loc(
                                            "WLM-6096: There is no valid assignment available in task definition for task name : {0}",
                                            taskMetaData.getName()));
                }
            }

        } catch (Exception ex) {
            throw new TaskException(ex);
        }

        return principals;
    }    
    public static String makeString(TaskPrincipal claimedBy) {
        // TODO Auto-generated method stub
        return claimedBy == null ? "" : claimedBy.getName();
    }

    public static String makeString(Collection<TaskPrincipal> assignedTo) {
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
    
    public static String makeStringFromTaskAssignee(
            Collection<TaskAssignee> assignedToTaskAssignee) {
        // TODO Auto-generated method stub
        String returnStr = "";
        if (assignedToTaskAssignee != null && assignedToTaskAssignee.size() > 0) {
            StringBuffer buffer = new StringBuffer();
            int i = 0;
            for (TaskAssignee principal : assignedToTaskAssignee) {
                buffer.append(principal.getAssignee());
                if (i < assignedToTaskAssignee.size() - 1) {
                    buffer.append(PRINCIPAL_DELIMETER);
                    i++;
                }

            }
            returnStr = buffer.toString();
        }
        return returnStr;

    }

}
