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
 * @(#)NewWebServiceFromWSDL.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.smtp.test;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author narayan
 */
@Stateless
@WebService(serviceName = "SmtpInboundTestService", portName = "SmtpInboundTestPort", endpointInterface = "org.netbeans.j2ee.wsdl.smtpinboundtest.SmtpInboundTestPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SmtpInboundTest", wsdlLocation = "META-INF/wsdl/NewWebServiceFromWSDL/SmtpInboundTest.wsdl")
public class NewWebServiceFromWSDL implements org.netbeans.j2ee.wsdl.smtpinboundtest.SmtpInboundTestPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SmtpInbound/SmtpInbound.wsdl")
    private org.netbeans.j2ee.xsd.tableschema.SmtpInboundService service;
    
    /** Creates a new instance of NewWebServiceFromWSDL */
    public NewWebServiceFromWSDL() {
    }

    public void smtpInboundTestOperation(org.netbeans.j2ee.xsd.tableschema.TEST part1) {
        System.out.println("Before invoking the SMTP incomin Operation/.... from the EJB ####");
        
        try { // Call Web Service Operation
            org.netbeans.j2ee.xsd.tableschema.SmtpInboundPortType port = service.getSmtpInboundPort();
            // TODO initialize WS operation arguments here
            //org.netbeans.j2ee.xsd.tableschema.TEST part1 = new org.netbeans.j2ee.xsd.tableschema.TEST();
            port.smtpInboundOperation(part1);
            System.out.println("After sending the mail and invoking the SMTP BC.... #########");
        } catch (Exception ex) {
            // TODO handle custom exceptions here
        }
        //throw new UnsupportedOperationException("Not yet implemented");
    }
    
}
