/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst.dl;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.FaultMessage;
import org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.OutMessage;
import org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.SimpleDocLiteralOperationFaultMessage1;

/**
 *
 * @author gpatil
 */
@Stateless
@WebService(serviceName = "SimpleDocLiteralService", portName = "SimpleDocLiteralPortTypeBindingPort", endpointInterface = "org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.SimpleDocLiteralPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleDocLiteral", wsdlLocation = "META-INF/wsdl/SimpleDocLitWS/SimpleDocLiteral.wsdl")
public class SimpleDocLitWS {

    public org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.OutMessage simpleDocLiteralOperation(org.netbeans.j2ee.wsdl.cafaulttest.simpledocliteral.InMessage part1) throws SimpleDocLiteralOperationFaultMessage1 {
        OutMessage om = null;
        if ("throw".equalsIgnoreCase(part1.getMsg())){
            FaultMessage fm = new FaultMessage();
            fm.setMsg("application fault message");
            SimpleDocLiteralOperationFaultMessage1 f = new SimpleDocLiteralOperationFaultMessage1("testing", fm);
            throw f;
        } else {
            om = new OutMessage();
            om.setMsg(part1.getMsg());
        }

        return om;
    }

}
