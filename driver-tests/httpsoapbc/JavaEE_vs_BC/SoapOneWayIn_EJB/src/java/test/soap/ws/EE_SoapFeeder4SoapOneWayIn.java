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
 * @(#)EE_SoapFeeder4SoapOneWayIn.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package test.soap.ws;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.soapfeeder4soaponewayin.SoapFeeder4SoapOneWayInPortType;
import org.netbeans.xml.schema.typeschema.RequestType;
import org.netbeans.xml.schema.typeschema.ResponseType;
import test.soap.ejb.EJB_UtilLocal;
import test.soap.ws.client.doc.RequestElement;

/**
 *
 * @author harry.liu (harry.liu@sun.com)
 */
@Stateless
@WebService(serviceName = "SoapFeeder4SoapOneWayInService", portName = "SoapFeeder4SoapOneWayInPort", endpointInterface = "org.netbeans.j2ee.wsdl.soapfeeder4soaponewayin.SoapFeeder4SoapOneWayInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapFeeder4SoapOneWayIn", wsdlLocation = "META-INF/wsdl/EE_SoapFeeder4SoapOneWayIn/SoapFeeder4SoapOneWayIn.wsdl")
public class EE_SoapFeeder4SoapOneWayIn implements SoapFeeder4SoapOneWayInPortType {

    @EJB
    private EJB_UtilLocal eJB_UtilBean;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapOneWayInRPC/SoapOneWayInRPC.wsdl")
    private test.soap.ws.client.rpc.SoapOneWayInRPCService service_1;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SoapOneWayInDoc/SoapOneWayInDoc.wsdl")
    private test.soap.ws.client.doc.SoapOneWayInDocService service;
    
    /** Creates a new instance of EE_SoapFeeder4SoapOneWayIn */
    public EE_SoapFeeder4SoapOneWayIn() {
    }

    public ResponseType soapFeeder4SoapOneWayInOperationDoc(RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.doc.SoapOneWayInDocPortType port = service.getSoapOneWayInDocPort();
            RequestElement request = new RequestElement();
            data = data + "\r\n... EE_SoapFeeder4SoapOneWayIn.soapFeeder4SoapOneWayInOperationDoc: calling soapOneWayInDocOperation\r\n";
            request.setRequest(data);
            port.soapOneWayInDocOperation(request);
            data = data + this.getInboundTraceMsg();
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapFeeder4SoapOneWayIn.soapFeeder4SoapOneWayInOperationDoc: Failed with exception:\r\n" + ex.toString();
        }
        
        ResponseType ret = new ResponseType();
        ret.setResponse(data);
        return ret;
    }

    public ResponseType soapFeeder4SoapOneWayInOperationRPC(RequestType part1) {
        String data = part1.getRequest();
        
        try { // Call Web Service Operation
            test.soap.ws.client.rpc.SoapOneWayInRPCPortType port = service_1.getSoapOneWayInRPCPort();
            // TODO initialize WS operation arguments here
            test.soap.ws.client.rpc.RequestType request = new test.soap.ws.client.rpc.RequestType();
            data = data + "\r\n... EE_SoapFeeder4SoapOneWayIn.soapFeeder4SoapOneWayInOperationRPC: calling soapOneWayInRPCOperation\r\n";
            request.setRequest(data);
            port.soapOneWayInRPCOperation(request);
            data = data + this.getInboundTraceMsg();
        } catch (Exception ex) {
            ex.printStackTrace();
            data = "\r\n... EE_SoapFeeder4SoapOneWayIn.soapFeeder4SoapOneWayInOperationRPC: Failed with exception:\r\n" + ex.toString();
        }
        
        ResponseType ret = new ResponseType();
        ret.setResponse(data);
        return ret;
    }

    public ResponseType soapFeeder4SoapOneWayInOperationEncoded(RequestType part1) {
        ResponseType ret = new ResponseType();
        ret.setResponse(part1.getRequest() + "\r\n... EE_SoapFeeder4SoapOneWayIn.soapFeeder4SoapOneWayInOperationEncoded: rpc/encoded wsdls are not supported in JAXWS 2.0");
        return ret;
    }
    
    private String getInboundTraceMsg() {
        String ret = null;
        for (int i = 0; i < 20; i++) {
            ret = eJB_UtilBean.getTrace();
            if (null != ret) {
                break;
            }
            
            try {
                Thread.currentThread().sleep(1000);
            } catch (InterruptedException ex) {
                ;
            }
        }
        
        return ret;
    }
    
}
