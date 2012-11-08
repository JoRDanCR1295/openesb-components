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
 * @(#)OperationProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import javax.security.auth.Subject;
import javax.wsdl.Operation;
import javax.xml.transform.Source;

/**
 *
 * 
 */
public class OperationProcessor  {
    
    private Operation mOperation;
    
    private Subject mSubject;
    
    private Source mInput;
    
    private Source mOutput;
    
    public OperationProcessor(Operation operation, 
                              Subject subject, 
                              Source input) {
        
    }
    
    public void process() {
        
        //based on operation, invoke it on TaskManagementOperations.
        //also conver input to input parameter as expected by that operation
        //in TaskManagementOperations
        
    }

    public Source getOutput() {
        return this.mOutput;
    }
    
    
}
