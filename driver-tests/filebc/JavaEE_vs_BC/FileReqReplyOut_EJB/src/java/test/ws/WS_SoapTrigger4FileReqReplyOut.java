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
import org.netbeans.j2ee.wsdl.filereqreplyout.FileReqReplyOutPortType;
import org.netbeans.j2ee.wsdl.filereqreplyout.FileReqReplyOutService;
import org.netbeans.xml.schema.simpleschema.RequestType;
import org.netbeans.xml.schema.simpleschema.ResponseType;

/**
 *
 * @author jfu
 */
@WebService(serviceName = "SoapTrigger4FileReqReplyOutService", portName = "SoapTrigger4FileReqReplyOutPort", endpointInterface = "org.netbeans.j2ee.wsdl.soaptrigger4filereqreplyout.SoapTrigger4FileReqReplyOutPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/SoapTrigger4FileReqReplyOut", wsdlLocation = "META-INF/wsdl/WS_SoapTrigger4FileReqReplyOut/SoapTrigger4FileReqReplyOut.wsdl")
@Stateless

public class WS_SoapTrigger4FileReqReplyOut {
    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/FileReqReplyOut/FileReqReplyOut.wsdl")
    private FileReqReplyOutService service;

    public org.netbeans.xml.schema.simpleschema.ResponseType soapTrigger4FileReqReplyOutOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {

        String data = part1.getRequestElement();
        try { // Call Web Service Operation
            FileReqReplyOutPortType port = service.getFileReqReplyOutPort();
            // TODO initialize WS operation arguments here
            RequestType file = new RequestType();
            file.setRequestElement(data + "\r\n... WS_SoapTrigger4FileReqReplyOut.soapTrigger4FileReqReplyOutOperation: " + new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS").format(new Date()));
            // according to the current semantics of filebc wsdl
            // this OUT-IN, only applies to solicit read - in this case - result in a empty payload
            // this might not be the intention of the original test creater, but keep it this way
            // as a flavor of test;-)
            
            ResponseType fileRet = port.fileReqReplyOutOperation(file);
            data = data + "\r\n... WS_SoapTrigger4FileReqReplyOut.soapTrigger4FileReqReplyOutOperation: Created a file using fileReqReplyOutOperation ...\r\n";
            data = data + "\r\n... WS_SoapTrigger4FileReqReplyOut.soapTrigger4FileReqReplyOutOperation: fileReqReplyOutOperation's response message content is: " + fileRet.getResponseElement();
        } catch (Exception ex) {
            ex.printStackTrace();
            data = data + "\r\n... WS_SoapTrigger4FileReqReplyOut.soapTrigger4FileReqReplyOutOperation: Failed with exception:\r\n" + ex.toString() + "\r\n";
        }

        org.netbeans.xml.schema.simpleschema.ResponseType ret = new org.netbeans.xml.schema.simpleschema.ResponseType();
        ret.setResponseElement(data);
        return ret;

    }
}
