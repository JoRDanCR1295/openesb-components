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
 * @(#)EJBTestDriverWs.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.ws.ejb.td;

import com.sun.tst.wsc.FaultMsg;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "EJBTestDriverService", portName = "EJBTestDriverPort", endpointInterface = "org.netbeans.j2ee.wsdl.ejbtestdriver.EJBTestDriverPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/EJBTestDriver", wsdlLocation = "META-INF/wsdl/EJBTestDriverWs/EJBTestDriver.wsdl")
public class EJBTestDriverWs implements org.netbeans.j2ee.wsdl.ejbtestdriver.EJBTestDriverPortType {
    
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/greetService/greetService.wsdl")
    private com.sun.tst.wsc.SvcGreet service;
    
    // URL deployed will be http://localhost:8080/EJBTestDriverService/EJBTestDriverWs
    public EJBTestDriverWs() {
    }
    
    public void ejbTestDriverOperation(javax.xml.ws.Holder<java.lang.String> part1) {
        if (part1.value != null){
            try {
                if (part1 != null){
                    com.sun.tst.wsc.GreetServicePortType port = service.getSvcPort();
                    part1.value = port.greetServiceOperation(part1.value);
                }
            } catch (FaultMsg fm){
                part1.value = "GotFaultMessage" ;
            } catch (Exception ex){
                part1.value = "Exception:" + ex.getMessage();
            }
        }
    }
}
