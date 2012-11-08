/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package test.ws;

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.fileonewayout4filereqreplyin.FileOneWayOut4FileReqReplyInPortType;
import org.netbeans.j2ee.wsdl.fileonewayout4filereqreplyin.FileOneWayOut4FileReqReplyInService;
import org.netbeans.j2ee.wsdl.soapfeeder4filereqreplyin.SoapFeeder4FileReqReplyInPortType;
import org.netbeans.xml.schema.simpleschema.RequestType;
import org.netbeans.xml.schema.simpleschema.ResponseType;
import test.ejb.EJB_InboundTracerLocal;

/**
 *
 * @author jfu
 */
@WebService(serviceName = "SoapFeeder4FileReqReplyInService", portName = "SoapFeeder4FileReqReplyInPort", endpointInterface = "org.netbeans.j2ee.wsdl.soapfeeder4filereqreplyin.SoapFeeder4FileReqReplyInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapFeeder4FileReqReplyIn", wsdlLocation = "META-INF/wsdl/WS_SoapFeeder4FileReqReplyIn/SoapFeeder4FileReqReplyIn.wsdl")
@Stateless
public class WS_SoapFeeder4FileReqReplyIn implements SoapFeeder4FileReqReplyInPortType {
    @EJB
    private EJB_InboundTracerLocal eJB_InboundTracerBean;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/FileOneWayOut4FileReqReplyIn/FileOneWayOut4FileReqReplyIn.wsdl")
    private FileOneWayOut4FileReqReplyInService service;

    public org.netbeans.xml.schema.simpleschema.ResponseType soapFeeder4FileReqReplyInOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {
        String data = part1.getRequestElement();
        try { // Call Web Service Operation
            FileOneWayOut4FileReqReplyInPortType port = service.getFileOneWayOut4FileReqReplyInPort();
            RequestType file = new RequestType();
            file.setRequestElement(data + "\r\n... WS_SoapFeeder4FileReqReplyIn.soapFeeder4FileReqReplyInOperation: " + new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS").format(new Date()));
            port.fileOneWayOut4FileReqReplyInOperation(file);
            data = data + "\r\n... WS_SoapFeeder4FileReqReplyIn.soapFeeder4FileReqReplyInOperation: Created a trigger file using fileOneWayOut4FileReqReplyInOperation ...\r\n";
            data = data + this.getInboundTraceMsg();
        } catch (Exception ex) {
            ex.printStackTrace();
            data = data + "\r\n... WS_SoapFeeder4FileReqReplyIn.soapFeeder4FileReqReplyInOperation: Failed with exception:\r\n" + ex.toString() + "\r\n";
        }

        ResponseType ret = new ResponseType();
        ret.setResponseElement(data);
        return ret;
    }

    private String getInboundTraceMsg() {
        String ret = null;
        for (int i = 0; i < 20; i++) {
            ret = eJB_InboundTracerBean.getTrace();
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
