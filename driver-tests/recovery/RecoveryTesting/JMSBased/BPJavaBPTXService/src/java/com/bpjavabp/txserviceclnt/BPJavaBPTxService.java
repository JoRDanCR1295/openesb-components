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
 * @(#)BPJavaBPTxService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.bpjavabp.txserviceclnt;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author pvarghese
 */
@Stateless
@WebService(serviceName = "CallJavaInService", portName = "CallInJavaPort", endpointInterface = "org.netbeans.j2ee.wsdl.correlatedjavabp.CallJavaInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/CorrelatedJavaBP", wsdlLocation = "META-INF/wsdl/BPJavaBPTxService/CorrelatedJavaBP.wsdl")
public class BPJavaBPTxService implements org.netbeans.j2ee.wsdl.correlatedjavabp.CallJavaInPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/CorrelatedJavaBP/CorrelatedJavaBP.wsdl")
    private com.bpjavabp.clnt.txservice.JavaToPassBPService service;
    
    /** Creates a new instance of BPJavaBPTxService */
    public BPJavaBPTxService() {
    }

    @TransactionAttribute(TransactionAttributeType.SUPPORTS)
    public void callJavaInOperation(org.netbeans.xml.schema.correlationsample.CSInput inMessage) {
        //throw new UnsupportedOperationException("Not yet implemented");
        int counter = inMessage.getCounter();
        int id = inMessage.getId();
        String str = inMessage.getInMessage();
        System.out.println("Got the message params: Counter: " + counter + ": id: " + id + ": message: " + str);
        try { // Call Web Service Operation
            com.bpjavabp.clnt.txservice.JavaToPassBPPortType port = service.getJavaToPassBPPort();
            // TODO initialize WS operation arguments here
            com.bpjavabp.clnt.txservice.CSInput outMessage = new com.bpjavabp.clnt.txservice.CSInput();
            outMessage.setCounter(counter);
            outMessage.setId(id);
            outMessage.setInMessage(str);
            port.javaToPassBPPortTypeOper(outMessage);
        } catch (Exception ex) {
            // TODO handle custom exceptions here
            System.out.println("Error in the JAVAEE webservice: " + ex);
        }
    }
    
}
