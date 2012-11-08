/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.svc;

import com.sun.tst.Counter;
import java.util.Date;
import java.util.logging.Level;
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
public class ThrottledSvc {

    private final int WAIT_FOR = (int) (0.5 * 1000); // .5 Sec
    
    /**
     * Constructor
     */
    public ThrottledSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://svc.sun.com/ThrottledSvc/}ThrottledSvcOperationResponse")
    public String doRun(String input) {
        logger.info("Throttled SVC waiting, got:" + input + ":" + new Date());
        try {
            Counter.CallRcvd.incrementAndGet();
            int concurrents = Counter.ConcurrentCallInProcess.incrementAndGet();
            
            synchronized(Counter.MaxConcurrentCallRcvd){
                if (Counter.MaxConcurrentCallRcvd.get() < concurrents){
                    Counter.MaxConcurrentCallRcvd.set(concurrents);
                }
            }
            
            Thread.sleep(WAIT_FOR);
            
        } catch (InterruptedException ex) {
            Logger.getLogger(ThrottledSvc.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            Counter.ConcurrentCallInProcess.decrementAndGet();
        }
        logger.info("Throttled SVC finished, returning after waiting. Msg:" + input + ":" + new Date());
        return input;        
    }

    // Logger
    private static final Logger logger = Logger.getLogger(ThrottledSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
}