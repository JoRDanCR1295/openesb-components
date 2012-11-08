/* 
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License. 
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License. 
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * If applicable add the following below this CDDL HEADER, 
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: Portions Copyright 
 * [year] [name of copyright owner] 
 */
/* 
 * $Id: GetTaskXformOperation.java,v 1.2 2010/02/15 19:22:52 fkieviet Exp $
 * 
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.HashMap;
import java.util.Map;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;
import com.sun.jbi.engine.workflow.util.Util;

public class GetTaskXformOperation extends AbstractStaticOperation {
    private static final String PARTNAME = "xForm";

    public GetTaskXformOperation(Operation operation, Element input, Subject subject,
            StaticTaskManagementService service) {
        // TODO Auto-generated constructor stub
        super(operation, input, subject, service);
    }

    public void execute() {
        // TODO Auto-generated method stub
        StaticOperationReply reply = new StaticOperationReply();
        // extract input param to getTaskInput operation of
        // StaticTaskManagementService from input
        try {
            Element result = null;
            Element input = getInput();
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());

            Element taskInputXform = getTaskManagementService().getTaskXform(taskid);

            // adding a wrapper for RPC to strip off
            taskInputXform = Util.wrapElement(taskInputXform, "html",
                    "http://www.w3.org/1999/xhtml");

            // convert result (ie. status) to xml element
            if (taskInputXform == null) {
                result = JBIMessageUtil.makeEmptyJBIMessage(getWSDLOperation());
            } else {
                // convert result (ie. status) to xml element
                Map<String, Element> partsMap = new HashMap<String, Element>();
                partsMap.put(PARTNAME, taskInputXform);

                result = JBIMessageUtil.makeJBIMessage(partsMap, getWSDLOperation());
            }
            reply.setReply(result);
            // set output
            setOutput(reply);
        } catch (WorkflowException e) {
            // TODO Auto-generated catch block
            reply.setFaulted(true);
            reply.setFault(buildFault(e));
            setOutput(reply);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            WorkflowException workex = new WorkflowException(e.getMessage(), e);
            reply.setFaulted(true);
            reply.setFault(buildFault(workex));
            setOutput(reply);
        }
    }

}
