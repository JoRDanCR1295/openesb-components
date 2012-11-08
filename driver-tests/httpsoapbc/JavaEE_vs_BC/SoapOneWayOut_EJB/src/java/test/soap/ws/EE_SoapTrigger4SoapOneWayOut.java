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
 * @(#)EE_SoapTrigger4SoapOneWayOut.java 
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
@WebService(serviceName = "SoapTrigger4SoapOneWayOutService", portName = "SoapTrigger4SoapOneWayOutPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaptrigger4soaponewayout.SoapTrigger4SoapOneWayOutPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapTrigger4SoapOneWayOut", wsdlLocation = "META-INF/wsdl/EE_SoapTrigger4SoapOneWayOut/SoapTrigger4SoapOneWayOut.wsdl")
public class EE_SoapTrigger4SoapOneWayOut implements org.netbeans.j2ee.wsdl.soaptrigger4soaponewayout.SoapTrigger4SoapOneWayOutPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapOneWayOutRPC/SoapOneWayOutRPC.wsdl")
    private test.soap.ws.client.rpc.SoapOneWayOutRPCService service_1;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapOneWayOutDoc/SoapOneWayOutDoc.wsdl")
    private test.soap.ws.client.doc.SoapOneWayOutDocService service;
    
    /** Creates a new instance of EE_SoapTrigger4SoapOneWayOut */
    public EE_SoapTrigger4SoapOneWayOut() {
    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapOneWayOutOperationDoc(org.netbeans.xml.schema.typeschema.RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.doc.SoapOneWayOutDocPortType port = service.getSoapOneWayOutDocPort();
            test.soap.ws.client.doc.RequestElement request = new test.soap.ws.client.doc.RequestElement();
            data = data + "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationDoc: calling soapOneWayOutDocOperation\r\n";
            request.setRequest(data);
            port.soapOneWayOutDocOperation(request);
            data = data + "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationDoc: called soapOneWayOutDocOperation\r\n";
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationDoc: Failed with exception:\r\n" + ex.toString();
        }
        
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(data);
        return ret;

    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapOneWayOutOperationRPC(org.netbeans.xml.schema.typeschema.RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.rpc.SoapOneWayOutRPCPortType port = service_1.getSoapOneWayOutRPCPort();
            test.soap.ws.client.rpc.RequestType request = new test.soap.ws.client.rpc.RequestType();
            data = data + "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationRPC: calling soapOneWayOutRPCOperation\r\n";
            request.setRequest(data);
            port.soapOneWayOutRPCOperation(request);
            data = data + "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationRPC: called soapOneWayOutRPCOperation\r\n";
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationRPC: Failed with exception:\r\n" + ex.toString();
        }
        
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(data);
        return ret;

    }

    public org.netbeans.xml.schema.typeschema.ResponseType soapTrigger4SoapOneWayOutOperationEncoded(org.netbeans.xml.schema.typeschema.RequestType part1) {
        org.netbeans.xml.schema.typeschema.ResponseType ret = new org.netbeans.xml.schema.typeschema.ResponseType();
        ret.setResponse(part1.getRequest() + "\r\n... EE_SoapTrigger4SoapOneWayOut.soapTrigger4SoapOneWayOutOperationEncoded: rpc/encoded wsdls are not supported in JAXWS 2.0");
        return ret;
    }
    
}
