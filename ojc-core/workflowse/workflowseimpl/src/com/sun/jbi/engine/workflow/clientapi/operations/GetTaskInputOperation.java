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
 * @(#)GetTaskInputOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.HashMap;
import java.util.Map;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;

/**
 * 
 * 
 */
public class GetTaskInputOperation extends AbstractStaticOperation {

    private String INPUT_PART_NAME = "inputMsg";

    /** Creates a new instance of GetTaskInputOperation */
    public GetTaskInputOperation(Operation operation, Element input, Subject subject,
            StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
        StaticOperationReply reply = new StaticOperationReply();
        // extract input param to getTaskInput operation of
        // StaticTaskManagementService from input
        try {
            Element input = getInput();
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());

            // invoke getTaskInput method
            Element taskInput = getTaskManagementService().getTaskInput(taskid);

            // convert result (ie. status) to xml element
            Map<String, Element> partsMap = new HashMap<String, Element>();
            partsMap.put(INPUT_PART_NAME, taskInput);

            Element result = JBIMessageUtil.makeJBIMessage(partsMap, getWSDLOperation());

            reply.setReply(result);
            // set output
            setOutput(reply);
        } catch (WorkflowException e) {
            // TODO Auto-generated catch block
            reply.setFaulted(true);
            reply.setFault(buildFault(e));
            setOutput(reply);
        }
    }

}
