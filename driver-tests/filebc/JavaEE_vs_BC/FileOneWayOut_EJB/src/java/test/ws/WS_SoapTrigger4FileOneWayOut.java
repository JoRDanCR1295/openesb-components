/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package test.ws;

import java.text.SimpleDateFormat;
import java.util.Date;
import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.fileonewayout.FileOneWayOutPortType;
import org.netbeans.j2ee.wsdl.fileonewayout.FileOneWayOutService;
import org.netbeans.xml.schema.simpleschema.RequestType;

/**
 *
 * @author jfu
 */
@Stateless
@WebService(serviceName = "SoapTrigger4FileOneWayOutService", portName = "SoapTrigger4FileOneWayOutPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaptrigger4fileonewayout.SoapTrigger4FileOneWayOutPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapTrigger4FileOneWayOut", wsdlLocation = "META-INF/wsdl/WS_SoapTrigger4FileOneWayOut/SoapTrigger4FileOneWayOut.wsdl")

public class WS_SoapTrigger4FileOneWayOut implements org.netbeans.j2ee.wsdl.soaptrigger4fileonewayout.SoapTrigger4FileOneWayOutPortType {
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/FileOneWayOut/FileOneWayOut.wsdl")
    private FileOneWayOutService service;

    /** Creates a new instance of WS_SoapTrigger4FileOneWayOut */
    public WS_SoapTrigger4FileOneWayOut() {
    }

    public org.netbeans.xml.schema.simpleschema.ResponseType soapTrigger4FileOneWayOutOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {

        org.netbeans.xml.schema.simpleschema.ResponseType ret = new org.netbeans.xml.schema.simpleschema.ResponseType();
        String data = part1.getRequestElement();
        try { // Call Web Service Operation
            FileOneWayOutPortType port = service.getFileOneWayOutPort();
            RequestType file = new RequestType();
            file.setRequestElement(data + "\r\n... WS_SoapTrigger4FileOneWayOut.soapTrigger4FileOneWayOutOperation: " + new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS").format(new Date()));
            port.fileOneWayOutOperation(file);
            data = data + "\r\n... WS_SoapTrigger4FileOneWayOut.soapTrigger4FileOneWayOutOperation: Created a file using fileOneWayOutOperation ...\r\n";
        } catch (Exception ex) {
            ex.printStackTrace();
            data = data + "\r\n... WS_SoapTrigger4FileOneWayOut.soapTrigger4FileOneWayOutOperation: Failed with exception:\r\n" + ex.toString() + "\r\n";
        }
        ret.setResponseElement(data);
        return ret;
    }
    
}
