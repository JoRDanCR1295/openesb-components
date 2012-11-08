/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package test.ws;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.fileonewayin.FileOneWayInPortType;
import test.ejb.EJB_InboundTracerLocal;

/**
 *
 * @author jfu
 */
@WebService(serviceName = "FileOneWayInService", portName = "FileOneWayInPort", endpointInterface = "org.netbeans.j2ee.wsdl.fileonewayin.FileOneWayInPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/FileOneWayIn", wsdlLocation = "META-INF/wsdl/WS_FileOneWayIn/FileOneWayIn.wsdl")
@Stateless
public class WS_FileOneWayIn implements FileOneWayInPortType {

    @EJB
    private EJB_InboundTracerLocal eJB_InboundTracerBean;

    public void fileOneWayInOperation(org.netbeans.xml.schema.simpleschema.RequestType part1) {
        //TODO implement this method
        String log = "\r\n... WS_FileOneWayIn.fileOneWayInOperation: File was read in and processed\r\n";
        eJB_InboundTracerBean.setTrace(log);

        System.out.println(log + "\r\n... File content is: " + part1.getRequestElement());
    }

}
