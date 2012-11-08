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
 * @(#)ReassignTaskOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;

/**
 *
 * 
 */
public class ReassignTaskOperation extends AbstractStaticOperation {
    
    /** Creates a new instance of ReassignTaskOperation */
    public ReassignTaskOperation(Operation operation,
                              Element input, 
                              Subject subject,
                              StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
        StaticOperationReply reply = new StaticOperationReply ();        
        //extract input param to reassignTask operation of 
        //StaticTaskManagementService from input
        Element input = getInput();
        try {
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());
            
            Collection<TaskPrincipal> newPrincipals = extractNewTaskPrincipals(input);
            
            Collection<TaskPrincipal> excludedPrincipals = extractNewTaskExcludedPrincipals(input);
            
            //invoke reassignTask method
            StaticTaskManagementService.OperationStatus status = getTaskManagementService().reassignTask(taskid, getTaskSubject(), newPrincipals, excludedPrincipals);
            
            //convert result (ie. status) to xml element
            Element result = ClientOperationsHelper.converStatus(status, getWSDLOperation());
      
            reply.setReply(result);
            //set output
            setOutput(reply);
        } catch (WorkflowException e) {
            // TODO Auto-generated catch block
            reply.setFaulted(true);
            reply.setFault(buildFault(e));
            setOutput(reply);
        }
    }
    
    private Collection<TaskPrincipal> extractNewTaskPrincipals(Element input) throws WorkflowException {
        List<TaskPrincipal> newPrincipals = new ArrayList<TaskPrincipal>();
    	
        //extract new subject from input xml message
        //extract group/role
        String groupName = ClientOperationsHelper.extractRole(input, getWSDLOperation());
        if(groupName != null && !groupName.trim().equals("")) {
        	TaskPrincipal group = TaskModelFactory.getInstance().createPrincipal(groupName, TaskPrincipal.PrincipalType.Group);
        	newPrincipals.add(group);
        }
        
        //extract user
        String userName = ClientOperationsHelper.extractUser(input, getWSDLOperation());
        if(userName != null && !userName.trim().equals("")) {
        	TaskPrincipal user = TaskModelFactory.getInstance().createPrincipal(userName, TaskPrincipal.PrincipalType.User);
        	newPrincipals.add(user);
        }
        
        return newPrincipals;
        
    }
    private Collection<TaskPrincipal> extractNewTaskExcludedPrincipals(Element input) throws WorkflowException {
        List<TaskPrincipal> newPrincipals = new ArrayList<TaskPrincipal>();
        
        //extract new subject from input xml message
        //extract group/role
        String groupName = ClientOperationsHelper.extractExcludedGroup(input, getWSDLOperation());
        if(groupName != null && !groupName.trim().equals("")) {
            TaskPrincipal group = TaskModelFactory.getInstance().createPrincipal(groupName, TaskPrincipal.PrincipalType.Group);
            newPrincipals.add(group);
        }
        
        //extract user
        String userName = ClientOperationsHelper.extractExcludedUser(input, getWSDLOperation());
        if(userName != null && !userName.trim().equals("")) {
            TaskPrincipal user = TaskModelFactory.getInstance().createPrincipal(userName, TaskPrincipal.PrincipalType.User);
            newPrincipals.add(user);
        }
        
        return newPrincipals;
        
    }

}
