/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.pojo.tst;

import java.util.logging.Logger;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.w3c.dom.Document;

/**
 *
 * @author gpatil
 */

@Provider
public class EchoDoc2Doc {

    private static final Logger logger = Logger.getLogger(EchoNode2Node.class.getName());

    public EchoDoc2Doc() {
    }

    /**
     * POJO Operation
     *
     * @param message the message passed to the listener
     */
    @Operation(outMessageTypeQN="{http://tst.pojo.sun.com/EchoDoc2Doc/}EchoDoc2DocOperationResponse")
    public Document receive(Document input) {
        return input;
    }
}