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
import org.netbeans.j2ee.wsdl.filereqreplyin.FileReqReplyInPortType;
import org.netbeans.xml.schema.simpleschema.ResponseType;
import test.ejb.EJB_InboundTracerLocal;

/**
 *
 * @author jfu
 */
@WebService(serviceName = "FileReqReplyInService", portName = "FileReqReplyInPort", endpointInterface = "org.netbeans.j2ee.wsdl.filereqreplyin.FileReqReplyInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/FileReqReplyIn", wsdlLocation = "META-INF/wsdl/WS_FileReqReplyIn/FileReqReplyIn.wsdl")
@Stateless
public class WS_FileReqReplyIn implements FileReqReplyInPortType {
    @EJB
    private EJB_InboundTracerLocal eJB_InboundTracerBean;

    public org.netbeans.xml.schema.simpleschema.ResponseType fileReqReplyInOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {
        eJB_InboundTracerBean.setTrace("\r\n... WS_FileReqReplyIn.fileReqReplyInOperation: file was read in and processed, output file is being created\r\n");

        ResponseType ret = new ResponseType();
        ret.setResponseElement(part1.getRequestElement() + "\r\n... WS_FileReqReplyIn.fileReqReplyInOperation: Creating file at time: " + new SimpleDateFormat("yyyyMMdd-HH-mm-ss-SSS").format(new Date()) + "\r\n");
        return ret;
    }

}
