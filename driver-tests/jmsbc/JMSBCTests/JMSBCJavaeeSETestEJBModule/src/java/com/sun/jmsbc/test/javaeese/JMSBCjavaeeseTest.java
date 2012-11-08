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
 * @(#)JMSBCjavaeeseTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jmsbc.test.javaeese;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.xml.schema.infromjmsbc.InputCType;

/**
 *
 * @author jtran
 */
@Stateless
@WebService(serviceName = "JMSInService", portName = "JMSInPort", endpointInterface = "org.netbeans.j2ee.wsdl.jmsin.JMSInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/JMSIn", wsdlLocation = "META-INF/wsdl/JMSBCjavaeeseTest/JMSIn.wsdl")
public class JMSBCjavaeeseTest implements org.netbeans.j2ee.wsdl.jmsin.JMSInPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/JMSOut/JMSOut.wsdl")
    private com.sun.jmsbc.test.javaeese.JMSOutService service;
    
    /** Creates a new instance of JMSBCjavaeeseTest */
    public JMSBCjavaeeseTest() {
    }

    public void jmsInOperation(InputCType part1) {        
        try { // Call Web Service Operation
            com.sun.jmsbc.test.javaeese.JMSOutPortType port = service.getJMSOutPort();
            // TODO initialize WS operation arguments here
            com.sun.jmsbc.test.javaeese.OutputCType part2 = new com.sun.jmsbc.test.javaeese.OutputCType();
            part2.setId(part1.getId()); 
            part2.setMsg("JavaEE SE got message: " + part1.getMsg());
            System.out.println ("Sending to JMS out queue");
            port.jmsOutOperation(part2);
            System.out.println ("After sending to JMS out queue");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
