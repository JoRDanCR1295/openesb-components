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
public class TstInOutSvc {
    
    /**
     * Constructor
     */
    public TstInOutSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://svc.tst.sun.com/TstInOutSvc/}TstInOutSvcOperationResponse")
    public String doTheTest(String input) throws ErrorMessage {
        if (Counter.TestErrorInOutStr.equalsIgnoreCase(input)){
            Counter.TestErrorCounter.addAndGet(1);
        }  else if (Counter.TestRedirectInOutStr.equalsIgnoreCase(input)){
            Counter.TestRedirectCounter.addAndGet(1);
        }

        throw new ErrorMessage("Testing Redelivery.");       
    }

    // Logger
    private static final Logger logger = Logger.getLogger(TstInOutSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}