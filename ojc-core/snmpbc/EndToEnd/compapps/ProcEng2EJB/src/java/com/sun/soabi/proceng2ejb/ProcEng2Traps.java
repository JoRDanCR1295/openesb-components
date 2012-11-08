/*
 * ProcEng2Traps.java
 * 
 * Created on May 21, 2007, 5:55:35 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.soabi.proceng2ejb;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.proceng2wsdl.ProcEng2WSDLPortType;

/**
 *
 * @author echou
 */
@WebService(serviceName = "ProcEng2WSDLService", portName = "ProcEng2WSDLPort", endpointInterface = "org.netbeans.j2ee.wsdl.proceng2wsdl.ProcEng2WSDLPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/ProcEng2WSDL", wsdlLocation = "META-INF/wsdl/ProcEng2Traps/ProcEng2WSDL.wsdl")
@Stateless
public class ProcEng2Traps implements ProcEng2WSDLPortType {

    public ProcEng2Traps() {
    }

    public void procEng2WSDLOperation(com.sun.soabi.snmpbc.traps.Traps traps) {
        StatsSvc2.sStats.update(1, traps.getTrap().size());
    }

}
