/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.tst;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.cafaulttest.simplerpcliteral.SimpleRPCLiteralOperationFault;
import org.netbeans.j2ee.wsdl.cafaulttest.simplerpcliteral.TheFault;

/**
 *
 * @author gpatil
 */
@WebService(serviceName = "SimpleRPCLiteralService", portName = "SimpleRPCLiteralPort", endpointInterface = "org.netbeans.j2ee.wsdl.cafaulttest.simplerpcliteral.SimpleRPCLiteralPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/caFaultTest/SimpleRPCLiteral", wsdlLocation = "META-INF/wsdl/RPCLiteral/SimpleRPCLiteral.wsdl")
@Stateless
public class RPCLiteral {

    public void simpleRPCLiteralOperation(javax.xml.ws.Holder<java.lang.String> part1) throws SimpleRPCLiteralOperationFault {
        if (part1.value.equalsIgnoreCase("ThrowFault")){
            TheFault tf = new TheFault();
            tf.setAppMsg("appMessage is appmessage");
            SimpleRPCLiteralOperationFault f = new SimpleRPCLiteralOperationFault("testFault", tf);
            throw f;
            //throw new UnsupportedOperationException("Not implemented yet.");
        } else {
            //echo same message.
        }
    }

}
