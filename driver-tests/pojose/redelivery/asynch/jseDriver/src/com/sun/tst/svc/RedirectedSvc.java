/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.svc;

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
public class RedirectedSvc {
    
    /**
     * Constructor
     */
    public RedirectedSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://svc.tst.sun.com/RedirectedSvc/}RedirectedSvcOperationResponse")
    public void receive(String input) {
        Counter.CalledRedirectedSvc.set(true);
    }

    // Logger
    private static final Logger logger = Logger.getLogger(RedirectedSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}