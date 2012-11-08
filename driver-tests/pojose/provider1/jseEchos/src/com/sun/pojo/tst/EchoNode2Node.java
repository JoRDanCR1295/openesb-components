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
public class EchoNode2Node {
    
    private static final Logger logger = Logger.getLogger(EchoNode2Node.class.getName());

    public EchoNode2Node() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoNode2NodeOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoNode2Node/")
    public Node receive(Node input) {
        	return input;        
    }
}