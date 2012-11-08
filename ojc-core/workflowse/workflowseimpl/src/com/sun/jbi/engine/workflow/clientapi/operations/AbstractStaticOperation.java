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
 * @(#)AbstractStaticOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import javax.security.auth.Subject;
import javax.wsdl.Operation;

import org.w3c.dom.Element;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.StaticOperation;
import com.sun.jbi.engine.workflow.clientapi.StaticTaskManagementService;
import com.sun.jbi.engine.workflow.clientapi.TaskSubject;
import com.sun.jbi.workflow.taskcommon.xmlbeans.FaultDocument;
import com.sun.jbi.workflow.taskcommon.xmlbeans.FaultType;

/**
 *
 * @author radval
 */
public abstract class AbstractStaticOperation implements StaticOperation {
    
    private Operation mOperation;
    
    private Element mInput;
    
    private TaskSubject mSubject;
    
    private StaticTaskManagementService mService;
    
    private StaticOperationReply mOutput;
    
    
    /** Creates a new instance of AbstractStaticOperation */
    public AbstractStaticOperation(Operation operation,
                                   Element input, 
                                   Subject subject,
                                   StaticTaskManagementService service) {
                                   
        this.mOperation = operation;
        this.mInput = input;
        if (subject != null) {
            this.mSubject = new TaskSubject(subject);
        }
        this.mService = service;
    }

    public Operation getWSDLOperation() {
        return this.mOperation;
    }
    
    public Element getInput() {
        return this.mInput;
    }

    
    public TaskSubject getTaskSubject() {
        return this.mSubject;
    }
    
    public StaticTaskManagementService getTaskManagementService() {
        return this.mService;
    }
    
    public StaticOperationReply getOutput() {
        return this.mOutput;
    }
    
    public void setOutput(StaticOperationReply output) {
        this.mOutput = output;
    }
    
    public Element buildFault (WorkflowException e) {
        //@ToDo implement the buildFault
        FaultDocument faultDoc = FaultDocument.Factory.newInstance();
        FaultType fault = faultDoc.addNewFault();
        fault.setFaultCode(e.getFaultCode());
        fault.setFaultReason(e.getMessage());
        return (Element) fault.getDomNode();
    }

}
