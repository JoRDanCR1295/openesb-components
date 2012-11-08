/*
 * GetTaskOutputOperation.java
 *
 * Created on October 29, 2006, 7:58 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.HashMap;
import java.util.Map;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.util.JBIMessageUtil;

/**
 * 
 * 
 */
public class GetTaskOutputOperation extends AbstractStaticOperation {
    private String OUTPUT_PART_NAME = "outputMsg";

    /** Creates a new instance of GetTaskOutputOperation */
    public GetTaskOutputOperation(Operation operation, Element input, Subject subject,
            StaticTaskManagementService service) {
        super(operation, input, subject, service);
    }

    public void execute() {
        StaticOperationReply reply = new StaticOperationReply();
        // extract input param to getTaskInput operation of
        // StaticTaskManagementService from input
        try {
            Element result = null;
            Element input = getInput();
            Long taskid = ClientOperationsHelper.extractTaskId(input, getWSDLOperation());

            // invoke getTaskInput method
            Element taskOutput = getTaskManagementService().getTaskOutput(taskid);
            if (taskOutput == null) {
                taskOutput = getTaskManagementService().getTaskOutputXformInstance(taskid);
            }
            if (taskOutput == null) {
                result = JBIMessageUtil.makeEmptyJBIMessage(getWSDLOperation());
            } else {

                // convert result (ie. status) to xml element
                Map<String, Element> partsMap = new HashMap<String, Element>();
                partsMap.put(OUTPUT_PART_NAME, taskOutput);

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
        }
    }

    // convert output to normalized message form
    private Element convertOutput(TaskOutput output) {
        Element result = null;

        return result;
    }

}