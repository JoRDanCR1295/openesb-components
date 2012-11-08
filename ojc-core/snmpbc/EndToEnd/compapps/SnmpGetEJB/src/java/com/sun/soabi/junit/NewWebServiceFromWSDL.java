/*
 * NewWebServiceFromWSDL.java
 * 
 * Created on Apr 20, 2007, 2:44:58 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.junit;

import javax.ejb.Stateless;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;
import org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLPortType;

/**
 *
 * @author echou
 */
@WebService(serviceName = "junitWSDLService", portName = "junitWSDLPort", endpointInterface = "org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLPortType", targetNamespace = "http://j2ee.netbeans.org/wsdl/junitWSDL", wsdlLocation = "META-INF/wsdl/NewWebServiceFromWSDL/junitWSDL.wsdl")
@Stateless
public class NewWebServiceFromWSDL implements JunitWSDLPortType {

    @WebServiceRef(wsdlLocation = "META-INF/wsdl/client/SnmpGetWSDL/SnmpGetWSDL.wsdl")
    private org.netbeans.j2ee.wsdl.snmpgetwsdl.SnmpGetWSDLService service;
    
    public NewWebServiceFromWSDL() {
    }
    
    public com.sun.soabi.snmpbc.getresponses.GetResponses junitWSDLOperation(com.sun.soabi.snmpbc.getrequests.GetRequests requests) {
        try {
            org.netbeans.j2ee.wsdl.snmpgetwsdl.SnmpGetWSDLPortType port = service.getSnmpGetWSDLPort();
            
            return port.snmpGetWSDLOperation(requests);
            
        } catch (Exception e) {
            
        }
        
        return null;
    }

}
