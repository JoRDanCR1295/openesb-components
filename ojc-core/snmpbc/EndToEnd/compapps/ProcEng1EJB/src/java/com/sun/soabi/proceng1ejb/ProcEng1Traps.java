/*
 * ProcEng1Traps.java
 * 
 * Created on Apr 14, 2007, 11:40:15 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.proceng1ejb;

import com.sun.soabi.snmpbc.traps.Trap;
import com.sun.soabi.snmpbc.traps.VarBinding;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.proceng1wsdl.ProcEng1WSDLPortType;

/**
 *
 * @author echou
 */
@WebService(serviceName = "ProcEng1WSDLService", portName = "ProcEng1WSDLPort", endpointInterface = "org.netbeans.j2ee.wsdl.proceng1wsdl.ProcEng1WSDLPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/ProcEng1WSDL", wsdlLocation = "META-INF/wsdl/ProcEng1Traps/ProcEng1WSDL.wsdl")
@Stateless
public class ProcEng1Traps implements ProcEng1WSDLPortType {

    public ProcEng1Traps() {
    }

    public void procEng1WSDLOperation(com.sun.soabi.snmpbc.traps.Traps traps) {
        StatsSvc.sStats.update(1, traps.getTrap().size());
//        for (Trap t: traps.getTrap()) {
//            sLog.log(Level.INFO, "Trap: "
//                + t.getUDPSource() + "/" 
//                + t.getUDPPort() + "/" 
//                + t.getType() + "/" 
//                + t.getVersion() + "/" 
//                + t.getRequestID() + "/" 
//                + t.getV1AgentAddress() + "/"
//                + t.getV1EnterpriseOID() + "/"
//                + t.getV1GenericTrap() + "/"
//                + t.getV1SpecificTrap() + "/"
//                + t.getV1Timestamp()
//            );
//            for (VarBinding b : t.getValues()) {
//                sLog.log(Level.INFO, "binding: " + b.getOID() + "/" + b.getValue() + "/" + b.getType());
//                
//            }
//        }
    }

    Logger sLog = Logger.getLogger("MySNMP");
}
