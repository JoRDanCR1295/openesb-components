/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb;

import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.res.Context;


/**
 *
 * @author gpatil
 */

@Provider 
public class CallBPEL {
    // logger
    private static final Logger logger = Logger.getLogger(CallBPEL.class.getName());
    
    /**
     * Constructor
     */
    public CallBPEL() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://openesb.glassfish.org/CallBPEL/}CallBPELOperationResponse")
    public String receive(String input) {
        String inputMessage = input;
        try {
            String outputMsg = (String) sep1.sendSynchInOut(inputMessage, Consumer.MessageObjectType.String);
            return "Hello from POJO: " + outputMsg;    
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return "Hello from POJO: " + input;        
    }

    @Resource
    private Context ctx;
    @ConsumerEndpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/bplCreditReport/echo}ServiceEndpoint1226Service", inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bplCreditReport/echo}echoOperationRequest", operationQN = "{http://j2ee.netbeans.org/wsdl/bplCreditReport/echo}echoOperation", name = "ServiceEndpoint1226", interfaceQN = "{http://j2ee.netbeans.org/wsdl/bplCreditReport/echo}echoPortType")
    private Consumer sep1;
}
