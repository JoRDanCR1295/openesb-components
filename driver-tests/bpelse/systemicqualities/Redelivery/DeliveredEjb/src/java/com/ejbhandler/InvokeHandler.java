/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.ejbhandler;

import javax.ejb.Stateless;
import javax.jws.WebService;
import localhost.multidepbpel.benchmark.*;

/**
 *
 * @author pVarghese
 */
@WebService(serviceName = "benchMarkSubBPService", portName = "benchMarkPort", 
            endpointInterface = "localhost.multidepbpel.benchmark.BenchMarkSubBPPortType", 
            targetNamespace = "http://localhost/MultiDepBpel/benchMark", 
            wsdlLocation = "META-INF/wsdl/InvokeHandler/benchMark.wsdl")
@Stateless
public class InvokeHandler implements BenchMarkSubBPPortType {

    public void benchMarkSubBPOperation1
            (javax.xml.ws.Holder<localhost.multidepbpel.benchmark.BenchMarkType> benchMarkPart) 
            throws BenchMarkMessage {

        BenchMarkType type = benchMarkPart.value;
        String val = type.getString();
        if(!val.equals("Invoke2")) {
            type.setString("Response");
        }
            
        benchMarkPart.value = type;
    }

}
