/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.inout;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.inouttxstart.callinoutservice.CallInoutServicePT;

import org.netbeans.j2ee.xsd.tableschema.DETAIL;

/**
 *
 * @author pVarghese
 */
@WebService(serviceName = "CallInoutServiceService", portName = "CallInoutServicePort", endpointInterface = "org.netbeans.j2ee.wsdl.inouttxstart.callinoutservice.CallInoutServicePT", targetNamespace = "http://j2ee.netbeans.org/wsdl/InOutTxStart/CallInoutService", wsdlLocation = "META-INF/wsdl/InoutCallService/CallInoutService.wsdl")
@Stateless
@TransactionAttribute(TransactionAttributeType.NOT_SUPPORTED)
public class InoutCallService implements CallInoutServicePT {

    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    public void callInoutServiceOper(javax.xml.ws.Holder<org.netbeans.j2ee.xsd.tableschema.DETAIL> part1) {
        DETAIL detail = part1.value;
        int id = detail.getDETAILRecord().get(0).getID().getValue();
        if (id == 2) {
            throw new RuntimeException("Not implemented yet.");
        }
        part1.value = detail;
    }

}
