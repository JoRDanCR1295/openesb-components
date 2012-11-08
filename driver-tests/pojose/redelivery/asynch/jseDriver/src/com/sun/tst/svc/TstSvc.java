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
import org.glassfish.openesb.pojose.api.ErrorMessage;

/**
 *
 * @author gpatil
 */
@Provider 
public class TstSvc {
    
    /**
     * Constructor
     */
    public TstSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     */
    @Operation 
    public void receive(String input) throws ErrorMessage {
        if (Counter.TestDeleteStr.equalsIgnoreCase(input)){
            Counter.TestDeleteCounter.addAndGet(1);
        } else if (Counter.TestErrorStr.equalsIgnoreCase(input)){
            Counter.TestErrorCounter.addAndGet(1);
        }  else if (Counter.TestRedirectStr.equalsIgnoreCase(input)){
            Counter.TestRedirectCounter.addAndGet(1);
        }
        
        throw new ErrorMessage("Testing Redelivery.");
    }

    // Logger
    private static final Logger logger = Logger.getLogger(TstSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}