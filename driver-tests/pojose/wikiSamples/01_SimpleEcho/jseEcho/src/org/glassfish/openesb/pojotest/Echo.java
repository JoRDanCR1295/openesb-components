/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.glassfish.openesb.pojotest;

import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;

/**
 *
 * @author gpatil
 */
@Provider 
public class Echo {
    
    /**
     * Constructor
     */
    public Echo() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://pojotest.openesb.glassfish.org/Echo/}EchoOperationResponse")
    public String receive(String input) {
        return input;        
    }

    // Logger
    private static final Logger logger = Logger.getLogger(Echo.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}