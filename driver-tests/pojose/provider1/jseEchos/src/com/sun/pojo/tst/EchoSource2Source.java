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
public class EchoSource2Source {
    
    private static final Logger logger = Logger.getLogger(EchoSource2Source.class.getName());

    public EchoSource2Source() {
    }
    
    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageType="EchoSource2SourceOperationResponse",outMessageTypeNS="http://tst.pojo.sun.com/EchoSource2Source/")
    public Source receive(Source input) {
        	return input;        
    }
}