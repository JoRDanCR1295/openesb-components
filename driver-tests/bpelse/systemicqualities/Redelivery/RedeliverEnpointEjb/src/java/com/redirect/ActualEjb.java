/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.redirect;

import javax.ejb.Stateless;
import javax.jws.WebService;
import org.netbeans.j2ee.wsdl.redirectonfailure.ActualPartnerPT;

/**
 *
 * @author pVarghese
 */
@WebService(serviceName = "ActualPartnerService", portName = "ActualPartnerServiceBindingPort", 
            endpointInterface = "org.netbeans.j2ee.wsdl.redirectonfailure.ActualPartnerPT", 
            targetNamespace = "http://j2ee.netbeans.org/wsdl/RedirectOnFailure", 
            wsdlLocation = "META-INF/wsdl/ActualEjb/RedirectOnFailure.wsdl")
@Stateless
public class ActualEjb implements ActualPartnerPT {

    public void actualPartnerOper(org.netbeans.j2ee.wsdl.redirectonfailure.CompMsg testPart) {
        //TODO implement this method
        throw new RuntimeException("Not implemented yet.");
    }

}
