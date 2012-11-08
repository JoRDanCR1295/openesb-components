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
 * @(#)EE_SoapReqReplyOutDoc.java 
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
@WebService(serviceName = "SoapReqReplyOutDocService", portName = "SoapReqReplyOutDocPort", endpointInterface = "org.netbeans.j2ee.wsdl.soapreqreplyoutdoc.SoapReqReplyOutDocPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapReqReplyOutDoc", wsdlLocation = "META-INF/wsdl/EE_SoapReqReplyOutDoc/SoapReqReplyOutDoc.wsdl")
public class EE_SoapReqReplyOutDoc implements org.netbeans.j2ee.wsdl.soapreqreplyoutdoc.SoapReqReplyOutDocPortType {
    
    /** Creates a new instance of EE_SoapReqReplyOutDoc */
    public EE_SoapReqReplyOutDoc() {
    }

    public org.netbeans.xml.schema.elementschema.ResponseElement soapReqReplyOutDocOperation(org.netbeans.xml.schema.elementschema.RequestElement part1) {
        org.netbeans.xml.schema.elementschema.ResponseElement ret = new org.netbeans.xml.schema.elementschema.ResponseElement();
        ret.setResponse(part1.getRequest() + "\r\n... EE_SoapReqReplyOutDoc.soapReqReplyOutDocOperation: called\r\n");
        return ret;
    }
    
}
