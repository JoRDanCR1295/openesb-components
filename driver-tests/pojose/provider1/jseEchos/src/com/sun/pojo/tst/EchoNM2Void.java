/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import javax.jbi.messaging.NormalizedMessage;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.annotation.POJO;
import org.glassfish.openesb.pojose.api.annotation.POJOResource;
import org.glassfish.openesb.pojose.api.res.POJOContext;

/**
 *
 * @author gpatil
 */
 @POJO
public class EchoNM2Void {
     @POJOResource
     POJOContext ctx;
             
    @Operation(outMessageType="EchoNM2VoidOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoNM2Void/")
    public void  receive(NormalizedMessage input) {
        System.out.println("Got in message:" + input.getContent());
    }
}
