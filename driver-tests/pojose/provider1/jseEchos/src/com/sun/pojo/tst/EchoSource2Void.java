/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import javax.xml.transform.Source;

/**
 *
 * @author gpatil
 */

@POJO
public class EchoSource2Void {
    
    private static final Logger logger = Logger.getLogger(EchoSource2Source.class.getName());

    public EchoSource2Void() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoSource2VoidOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoSource2Void/")
    public void receive(Source input) {
        System.out.println("Got input:" + input);
    }
}