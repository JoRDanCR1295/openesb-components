/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import org.glassfish.openesb.pojose.api.res.POJOContext;

/**
 * @author gpatil
 */
@POJO
public class EchoME2Void {
     @POJOResource
     POJOContext ctx;    
     
    @Operation(outMessageType="EchoME2VoidOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoME2Void/")
    public void receive(MessageExchange me) {
        NormalizedMessage in = null;
            in = me.getMessage("in");
            System.out.println("Got message:" + in.getContent());
    }
}
