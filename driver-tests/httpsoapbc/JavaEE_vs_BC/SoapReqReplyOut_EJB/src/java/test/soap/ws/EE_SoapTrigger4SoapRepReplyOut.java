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
 * @(#)EE_SoapTrigger4SoapRepReplyOut.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapTrigger4SoapRepReplyOutService", portName = "SoapTrigger4SoapRepReplyOutPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaptrigger4soaprepreplyout.SoapTrigger4SoapRepReplyOutPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapTrigger4SoapRepReplyOut", wsdlLocation = "META-INF/wsdl/EE_SoapTrigger4SoapRepReplyOut/SoapTrigger4SoapRepReplyOut.wsdl")
public class EE_SoapTrigger4SoapRepReplyOut implements org.netbeans.j2ee.wsdl.soaptrigger4soaprepreplyout.SoapTrigger4SoapRepReplyOutPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapReqReplyOutRPC/SoapReqReplyOutRPC.wsdl")
    private test.soap.ws.client.rpc.SoapReqReplyOutRPCService service_1;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapReqReplyOutDoc/SoapReqReplyOutDoc.wsdl")
    private test.soap.ws.client.doc.SoapReqReplyOutDocService service;
    
    /** Creates a new instance of EE_SoapTrigger4SoapRepReplyOut */
    public EE_SoapTrigger4SoapRepReplyOut() {
    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapRepReplyOutOperationDoc(org.netbeans.xml.schema.typeschema.RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.doc.SoapReqReplyOutDocPortType port = service.getSoapReqReplyOutDocPort();
            test.soap.ws.client.doc.RequestElement request = new test.soap.ws.client.doc.RequestElement();
            data = data + "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationDoc: calling soapReqReplyOutDocOperation\r\n";
            request.setRequest(data);
            test.soap.ws.client.doc.ResponseElement result = port.soapReqReplyOutDocOperation(request);
            System.out.println("Result = "+result);
            data = result.getResponse() + "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationDoc: called soapReqReplyOutDocOperation\r\n";
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationDoc: Failed with exception:\r\n" + ex.toString();
        }
        
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(data);
        return ret;
    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapRepReplyOutOperationRPC(org.netbeans.xml.schema.typeschema.RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.rpc.SoapReqReplyOutRPCPortType port = service_1.getSoapReqReplyOutRPCPort();
            test.soap.ws.client.rpc.RequestType request = new test.soap.ws.client.rpc.RequestType();
            data = data + "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationRPC: calling soapReqReplyOutRPCOperation\r\n";
            request.setRequest(data);
            test.soap.ws.client.rpc.ResponseType result = port.soapReqReplyOutRPCOperation(request);
            System.out.println("Result = "+result);
            data = result.getResponse() + "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationRPC: called soapReqReplyOutDocOperation\r\n";
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapTrigger4SoapRepReplyOut.soapTrigger4SoapRepReplyOutOperationRPC: Failed with exception:\r\n" + ex.toString();
        }
        
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(data);
        return ret;
    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapRepReplyOutOperationEncoded(org.netbeans.xml.schema.typeschema.RequestType part1) {
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(part1.getRequest() + "\r\n... soapTrigger4SoapRepReplyOutOperationEncoded: rpc/encoded wsdls are not supported in JAXWS 2.0");
        return ret;
    }
    
}
