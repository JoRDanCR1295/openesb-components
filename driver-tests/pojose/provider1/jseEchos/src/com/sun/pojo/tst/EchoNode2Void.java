/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import org.w3c.dom.Node;

/**
 *
 * @author gpatil
 */

@POJO
public class EchoNode2Void {
    
    private static final Logger logger = Logger.getLogger(EchoNode2Node.class.getName());

    public EchoNode2Void() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoME2MEOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoME2ME/")
    public void receive(Node input) {
        System.out.println("Got input:" + input);
    }
}