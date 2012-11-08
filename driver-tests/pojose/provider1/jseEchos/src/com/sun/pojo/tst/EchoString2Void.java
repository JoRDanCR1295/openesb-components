/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;

/**
 *
 * @author gpatil
 */

@POJO
public class EchoString2Void {
    
    private static final Logger logger = Logger.getLogger(EchoString2String.class.getName());

    public EchoString2Void() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoString2VoidOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoString2Void/")
    public void receive(String input) {
        System.out.println("Got input message:" + input);
    }
}