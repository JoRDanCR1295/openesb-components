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
 * @(#)LoanApproverSvc.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.fs.approver;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "LoanApproverService", portName = "LoanApproverPort", endpointInterface = "com.fs.approver.LoanApprover", targetNamespace = "http://approver.fs.com/", wsdlLocation = "META-INF/wsdl/LoanApproverSvc/LoanApproverService.wsdl")
public class LoanApproverSvc implements com.fs.approver.LoanApprover {
    
    /** Creates a new instance of LoanApproverSvc */
    public LoanApproverSvc() {
    }

    public String getDecision(String report) {
        System.out.println("LoanApprover::getDecision start");
        String decision = "approved";
        System.out.println("LoanApprover::getDecision end. result: "+decision);
        return decision;
    }
    
}
