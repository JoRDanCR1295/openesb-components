/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.inonly;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.inonlytxstart.callservice.InOnlyPT;

/**
 *
 * @author pVarghese
 */
@WebService(serviceName = "InOnlyService", portName = "InOnlyServicePort", endpointInterface = "org.netbeans.j2ee.wsdl.inonlytxstart.callservice.InOnlyPT", targetNamespace = "http://j2ee.netbeans.org/wsdl/InOnlyTxStart/CallService", wsdlLocation = "META-INF/wsdl/InOnlyTxService/CallService.wsdl")
@Stateless
@TransactionAttribute(TransactionAttributeType.NOT_SUPPORTED)
public class InOnlyTxService implements InOnlyPT {

    @TransactionAttribute(TransactionAttributeType.REQUIRED)
    public void inOnlyOper(org.netbeans.j2ee.xsd.tableschema.DETAIL part1) {
        int id = part1.getDETAILRecord().get(0).getID().getValue();
        if (id == 2) {
            throw new RuntimeException("Not implemented yet.");

        }
        return; 
    }

}
