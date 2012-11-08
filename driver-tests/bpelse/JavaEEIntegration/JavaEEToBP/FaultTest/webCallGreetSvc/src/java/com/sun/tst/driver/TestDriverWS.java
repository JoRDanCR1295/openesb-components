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
 * @(#)TestDriverWS.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.tst.driver;

import com.sun.tst.wsc.FaultMsg;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author gpatil
 */
@WebService(serviceName = "TestDriverService", portName = "TestDriverPort", endpointInterface = "org.netbeans.j2ee.wsdl.testdriver.TestDriverPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/TestDriver", wsdlLocation = "WEB-INF/wsdl/TestDriverWS/TestDriver.wsdl")
public class TestDriverWS implements org.netbeans.j2ee.wsdl.testdriver.TestDriverPortType {

    @WebServiceRef(wsdlLocation = "WEB-INF/wsdl/client/greetService/greetService.wsdl")
    private com.sun.tst.wsc.SvcGreet service;
    
    /** Creates a new instance of TestDriverWS */
    public TestDriverWS() {
    }

    public void testDriverOperation(javax.xml.ws.Holder<java.lang.String> part1) {
        if (part1.value != null){            
            try { 
                com.sun.tst.wsc.GreetServicePortType port = service.getSvcPort();
                java.lang.String result = port.greetServiceOperation(part1.value);
                part1.value = result;
            } catch (FaultMsg fm){
                part1.value = "GotFaultMessage";
            } catch (Exception ex) {
                part1.value = "Exception:" + ex.getMessage();
            }
        }
    }
    
}
