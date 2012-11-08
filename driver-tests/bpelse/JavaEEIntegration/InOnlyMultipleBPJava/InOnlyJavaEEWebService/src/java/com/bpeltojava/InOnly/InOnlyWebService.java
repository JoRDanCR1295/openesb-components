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
 * @(#)InOnlyWebService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.bpeltojava.InOnly;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author pvarghese
 */
@Stateless
@WebService(serviceName = "BPToJavaEEService", portName = "BPToJavaEEPort", endpointInterface = "org.netbeans.j2ee.wsdl.bptojava.BPToJavaEEPT", targetNamespace = "http://j2ee.netbeans.org/wsdl/BPToJava", wsdlLocation = "META-INF/wsdl/InOnlyWebService/BPToJava.wsdl")
public class InOnlyWebService implements org.netbeans.j2ee.wsdl.bptojava.BPToJavaEEPT {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/BPToJava/BPToJava.wsdl")
    private com.javatobpel.client.JavaEEToBPService service;
    
    /** Creates a new instance of InOnlyWebService */
    public InOnlyWebService() {
    }

    public void bpToJavaEEOper(org.netbeans.xml.schema.bptojavaschema.MsgType msg) {
        //throw new UnsupportedOperationException("Not yet implemented");
        int corrId = msg.getIntElem();
        String strMsg = msg.getStrElem() + " Added in the JavaEE WS before calling Client BPWS:";
        try { // Call Web Service Operation
            com.javatobpel.client.JavaEEToBPPT port = service.getJavaEEToBPPort();
            // TODO initialize WS operation arguments here
            com.javatobpel.client.MsgType outMsg = new com.javatobpel.client.MsgType();
            outMsg.setIntElem(corrId);
            outMsg.setStrElem(strMsg);
            port.javaEEToBPOper(outMsg);
        } catch (Exception ex) {
            // TODO handle custom exceptions here
            System.out.println("Exception in the InOnly javaEE WS: " + ex);
        }
    }
    
}
