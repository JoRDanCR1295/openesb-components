/*
 * StaticOperationFactory.java
 *
 * Created on October 25, 2006, 8:17 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.engine.workflow.clientapi;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.clientapi.operations.ClaimTaskOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.CompleteTaskOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.GetTaskInputOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.GetTaskListOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.GetTaskOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.GetTaskOutputOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.GetTaskXformOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.ReassignTaskOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.RevokeTaskOperation;
import com.sun.jbi.engine.workflow.clientapi.operations.SetTaskOutputOperation;

/**
 * Factory class to create various client operations.
 * 
 */
public class StaticOperationFactory {

    private static StaticOperationFactory mInstance;

    private StaticTaskManagementService mService;

    public static String OPERATION_getTaskList = "GetTaskList";

    public static String OPERATION_claimTask = "ClaimTask";

    public static String OPERATION_revokeTask = "RevokeTask";

    public static String OPERATION_completeTask = "CompleteTask";

    public static String OPERATION_reassignTask = "ReassignTask";

    public static String OPERATION_getTaskInput = "GetTaskInput";

    public static String OPERATION_setTaskOutput = "SetTaskOutput";

    public static String OPERATION_getTaskOutput = "GetTaskOutput";

    public static String OPERATION_getTaskXform = "GetTaskXForm";
    
    public static String OPERATION_getTask = "GetTask";
  

    /** Creates a new instance of StaticOperationFactory */
    public StaticOperationFactory() {
        this.mService = new StaticTaskManagementService();
    }

    public static synchronized StaticOperationFactory getInstance() {
        if (mInstance == null) {
            mInstance = new StaticOperationFactory();
        }

        return mInstance;
    }

    public StaticTaskManagementService getService() {
        return mService;
    }

    /**
     * create StaticOperation based on operation name. call execute to execute operation logic. call
     * getOutput to get result
     */
    public synchronized StaticOperation newOperation(Operation operation, Element input,
            Subject subject) {
        StaticOperation sOperation = null;
        String operationName = operation.getName();

        if (OPERATION_getTaskList.equals(operationName)) {
            sOperation = newGetTaskListOperation(operation, input, subject);
        } else if (OPERATION_claimTask.equals(operationName)) {
            sOperation = newClaimTaskOperation(operation, input, subject);
        } else if (OPERATION_completeTask.equals(operationName)) {
            sOperation = newCompleteTaskOperation(operation, input, subject);
        } else if (OPERATION_revokeTask.equals(operationName)) {
            sOperation = newRevokeTaskOperation(operation, input, subject);
        } else if (OPERATION_reassignTask.equals(operationName)) {
            sOperation = newReassignTaskOperation(operation, input, subject);
        } else if (OPERATION_getTask.equals(operationName)) {
            sOperation = newGetTaskOperation(operation, input, subject);
        } else if (OPERATION_getTaskInput.equals(operationName)) {
            sOperation = newGetTaskInputOperation(operation, input, subject);
        } else if (OPERATION_getTaskOutput.equals(operationName)) {
            sOperation = newGetTaskOutputOperation(operation, input, subject);
        } else if (OPERATION_setTaskOutput.equals(operationName)) {
            sOperation = newSetTaskOutputOperation(operation, input, subject);
        }  else if (OPERATION_getTaskXform.equalsIgnoreCase(operationName)) {
            sOperation = newGetTaskXformOperation(operation, input, subject);
        } else if (operationName.startsWith("Set") && operationName.endsWith("Output")) {
            sOperation = newSetTaskOutputOperation(operation, input, subject);
        } else if (operationName.startsWith("Get") && operationName.endsWith("Input")) {
            sOperation = newGetTaskInputOperation(operation, input, subject);
        } else if (operationName.startsWith("Get") && operationName.endsWith("Output")) {
            sOperation = newGetTaskOutputOperation(operation, input, subject);
        } 
        return sOperation;

    }


    StaticOperation newGetTaskListOperation(Operation operation, Element input, Subject subject) {
        return new GetTaskListOperation(operation, input, subject, this.mService);
    }

    StaticOperation newClaimTaskOperation(Operation operation, Element input, Subject subject) {
        return new ClaimTaskOperation(operation, input, subject, this.mService);
    }

    StaticOperation newRevokeTaskOperation(Operation operation, Element input, Subject subject) {
        return new RevokeTaskOperation(operation, input, subject, this.mService);
    }

    StaticOperation newCompleteTaskOperation(Operation operation, Element input, Subject subject) {
        return new CompleteTaskOperation(operation, input, subject, this.mService);
    }

    StaticOperation newReassignTaskOperation(Operation operation, Element input, Subject subject) {
        return new ReassignTaskOperation(operation, input, subject, this.mService);
    }

    StaticOperation newGetTaskInputOperation(Operation operation, Element input, Subject subject) {
        return new GetTaskInputOperation(operation, input, subject, this.mService);
    }

    StaticOperation newGetTaskOutputOperation(Operation operation, Element input, Subject subject) {
        return new GetTaskOutputOperation(operation, input, subject, this.mService);
    }

    StaticOperation newSetTaskOutputOperation(Operation operation, Element input, Subject subject) {
        return new SetTaskOutputOperation(operation, input, subject, this.mService);
    }

    

    StaticOperation newGetTaskXformOperation(Operation operation, Element input,
            Subject subject) {
        return new GetTaskXformOperation(operation, input, subject, this.mService);
    }

    StaticOperation newGetTaskOperation(Operation operation, Element input, Subject subject) {
        return new GetTaskOperation(operation, input, subject, this.mService);
    }

}
