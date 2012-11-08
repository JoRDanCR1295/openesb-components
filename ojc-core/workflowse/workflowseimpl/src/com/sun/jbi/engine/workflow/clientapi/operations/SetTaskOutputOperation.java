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
 * @(#)SetTaskOutputOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import javax.security.auth.Subject;
import javax.wsdl.Operation;
import javax.xml.transform.Source;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.ClientApiException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;

/**
 *
 * @author radval
 */
public class SetTaskOutputOperation extends AbstractStaticOperation {
    
    /** Creates a new instance of SetTaskOutputOperation */
    public SetTaskOutputOperation(Operation operation,
                                Element input, 
                              Subject subject,
                              StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
        StaticOperationReply reply = new StaticOperationReply ();      
        
        //extract input param to setTaskOutpput operation of 
        //StaticTaskManagementService from input
        try {
            Element input = getInput();
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());
            
            Element taskOutputEl = ClientOperationsHelper.extractTaskOutputFromClient(input, getWSDLOperation());
            
            StaticTaskManagementService.OperationStatus status = getTaskManagementService().setTaskOutput(taskid, taskOutputEl, getTaskSubject());
            
            //convert result (ie. status) to xml element
            Element result = ClientOperationsHelper.converStatus(status, getWSDLOperation());
            
            reply.setReply(result);
            //set output
            setOutput(reply);
        } catch (ClientApiException e) {
            // TODO Auto-generated catch block
            reply.setFaulted(true);
            reply.setFault(buildFault(e));
            setOutput(reply);
        }
    }
    
    //convert output to normalized message form
    private Source convertOutput(TaskOutput output) {
        Source result = null;
        
        return result;
    }
    
    private TaskOutput extractTaskOutput() {
        TaskOutput output = null;
        Element input = getInput();
        //extract task output from input.
        
        
        return output;
    }
}
