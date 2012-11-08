/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.pojose.mngmntmsg;

import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;

/**
 *
 * @author gpatil
 */
@Provider 
public class UndeployableSvc {
    // Logger
    private static final Logger logger = Logger.getLogger(UndeployableSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;

    @ConsumerEndpoint(name="Hello", serviceQN="World", interfaceQN="{non-existent}ServiceInterface")
    private Consumer cons1;
    /**
     * Constructor
     */
    public UndeployableSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://mngmntmsg.pojose.tst.sun.com/UndeployableSvc/}UndeployableSvcOperationResponse")
    public String receive(String input) {
        return input;        
    }
}