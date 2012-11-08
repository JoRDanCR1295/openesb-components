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
 * @(#)FaultWebService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.fault.JavaEEToBP;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author pVarghese
 */
@Stateless
@WebService(serviceName = "FaultService", portName = "FaultServicePort", endpointInterface = "org.netbeans.j2ee.wsdl.fault.FaultPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/Fault", wsdlLocation = "META-INF/wsdl/FaultWebService/Fault.wsdl")
public class FaultWebService implements org.netbeans.j2ee.wsdl.fault.FaultPortType {
    
    /** Creates a new instance of FaultWebService */
    public FaultWebService() {
    }

    public void faultOperation(javax.xml.ws.Holder<java.lang.String> inoutPart) throws org.netbeans.j2ee.wsdl.fault.FaultMsg {
        //throw new UnsupportedOperationException("Not yet implemented");
        String inMsg = inoutPart.value;
        String fMsg = inMsg + " value is not accepted:";
        String fInfo = "Test for EJB throwing fault";
        org.netbeans.j2ee.wsdl.fault.FaultMsg throwMsg = 
                    new org.netbeans.j2ee.wsdl.fault.FaultMsg(fMsg, fInfo);
        // explicitly throw the fault.
        throw throwMsg;
    }
    
}
