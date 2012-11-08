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
 * @(#)JMSBCXAjavaeeInOnlyTestService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jmsbc.test.javaeese;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author jtran
 */
@Stateless
@WebService(serviceName = "JMSBCjavaeeXAInOnlyTestService", portName = "JMSBCjavaeeXAInOnlyTestPortIn", endpointInterface = "org.netbeans.j2ee.wsdl.jmsbcjavaeese.jmsbcjavaeexainonlytest.JMSBCjavaeeXAInOnlyTestPortTypeIn", targetNamespace = "http://j2ee.netbeans.org/wsdl/jmsBCjavaeeSE/JMSBCjavaeeXAInOnlyTest", wsdlLocation = "META-INF/wsdl/JMSBCXAjavaeeInOnlyTestService/JMSBCjavaeeXAInOnlyTest.wsdl")
public class JMSBCXAjavaeeInOnlyTestService implements org.netbeans.j2ee.wsdl.jmsbcjavaeese.jmsbcjavaeexainonlytest.JMSBCjavaeeXAInOnlyTestPortTypeIn {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/JMSBCjavaeeXAInOnlyTest/JMSBCjavaeeXAInOnlyTest.wsdl")
    private com.sun.jmsbc.test.javaeese.JMSBCjavaeeXAInOnlyTestService service;
    
    /** Creates a new instance of JMSBCXAjavaeeInOnlyTestService */
    public JMSBCXAjavaeeInOnlyTestService() {
    }

    public void jmsBCjavaeeXAInOnlyTestOperationRecv(org.netbeans.xml.schema.jmsbcjavaeexainonlytest.AComplexType part1) {
        try { // Call Web Service Operation
            System.out.println ("JMSBC JavaEESE XA test; received msg [" + part1.getMsg() + "]");
            com.sun.jmsbc.test.javaeese.JMSBCjavaeeXAInOnlyTestPortTypeOut port = service.getJMSBCjavaeeXAInOnlyTestPortOut();
            // TODO initialize WS operation arguments here
            com.sun.jmsbc.test.javaeese.AComplexType partOut = new com.sun.jmsbc.test.javaeese.AComplexType();
            partOut.setMsg("JMSBC JavaEESE XA test; EJB got message [" + part1.getMsg() + "]");            
            System.out.println ("JMSBC JavaEESE XA test; before invoking JMS send");
            port.jmsBCjavaeeXAInOnlyTestOperationSend(partOut);
            System.out.println ("JMSBC JavaEESE XA test; after invoking JMS send");
            throw new RuntimeException ("Force XA transaction roll back");
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new RuntimeException (ex);
        } 
    }
    
}
