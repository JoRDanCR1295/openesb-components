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
public class EchoString2String {
    
    private static final Logger logger = Logger.getLogger(EchoString2String.class.getName());

    public EchoString2String() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoString2StringOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoString2String/")
    public String receive(String input) {
        	return "Hello from POJO: in:" + input;        
    }
}