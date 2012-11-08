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
 * @(#)EE_SoapReqReplyInRPC.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.Stateless;
import javax.jws.WebService;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapReqReplyInRPCService", portName = "SoapReqReplyInRPCPort", endpointInterface = "org.netbeans.j2ee.wsdl.soapreqreplyinrpc.SoapReqReplyInRPCPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapReqReplyInRPC", wsdlLocation = "META-INF/wsdl/EE_SoapReqReplyInRPC/SoapReqReplyInRPC.wsdl")
public class EE_SoapReqReplyInRPC implements org.netbeans.j2ee.wsdl.soapreqreplyinrpc.SoapReqReplyInRPCPortType {
    
    /** Creates a new instance of EE_SoapReqReplyInRPC */
    public EE_SoapReqReplyInRPC() {
    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapReqReplyInRPCOperation(org.netbeans.xml.schema.typeschema.RequestType part1) {
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(part1.getRequest() + "\r\n... EE_SoapReqReplyInRPC.soapReqReplyInRPCOperation: reply back\r\n");
        return ret;
    }
    
}
