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
import org.netbeans.j2ee.wsdl.fileonewayout4fileonewayin.FileOneWayOut4FileOneWayInPortType;
import org.netbeans.j2ee.wsdl.fileonewayout4fileonewayin.FileOneWayOut4FileOneWayInService;
import org.netbeans.j2ee.wsdl.soapfeeder4fileonewayin.SoapFeeder4FileOneWayInPortType;
import org.netbeans.xml.schema.simpleschema.ResponseType;
import test.ejb.EJB_InboundTracerLocal;

/**
 *
 * @author jfu
 */
@WebService(serviceName = "SoapFeeder4FileOneWayInService", portName = "SoapFeeder4FileOneWayInPort", endpointInterface = "org.netbeans.j2ee.wsdl.soapfeeder4fileonewayin.SoapFeeder4FileOneWayInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapFeeder4FileOneWayIn", wsdlLocation = "META-INF/wsdl/WS_SOAPFeeder4FileOneWayIn/SoapFeeder4FileOneWayIn.wsdl")
@Stateless
public class WS_SOAPFeeder4FileOneWayIn implements SoapFeeder4FileOneWayInPortType {
    @EJB
    private EJB_InboundTracerLocal eJB_InboundTracerBean;

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/FileOneWayOut4FileOneWayIn/FileOneWayOut4FileOneWayIn.wsdl")
    private FileOneWayOut4FileOneWayInService service;

    public org.netbeans.xml.schema.simpleschema.ResponseType soapFeeder4FileOneWayInOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {
        //TODO implement this method
        String data = part1.getRequestElement();
        try { // Call Web Service Operation
            FileOneWayOut4FileOneWayInPortType port = service.getFileOneWayOut4FileOneWayInPort();
            org.netbeans.xml.schema.simpleschema.RequestType file = new org.netbeans.xml.schema.simpleschema.RequestType();
            file.setRequestElement(data + "\r\n... WS_SoapFeeder4FileOneWayIn.soapFeeder4FileOneWayInOperation: \r\n" + new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS").format(new Date()));
            port.fileOneWayOut4FileOneWayInOperation(file);
            data = data + "\r\n... WS_SoapFeeder4FileOneWayIn.soapFeeder4FileOneWayInOperation: Created a trigger file using fileOneWayOut4FileOneWayInOperation ...\r\n";
            data = data + this.getInboundTraceMsg();
        } catch (Exception ex) {
            ex.printStackTrace();
            data = data + "\r\n... WS_SoapFeeder4FileOneWayIn.soapFeeder4FileOneWayInOperation: Failed with exception:\r\n" + ex.toString() + "\r\n";
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
