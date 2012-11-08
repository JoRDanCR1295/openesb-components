/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.glassfish.openesb.pojo.scheduler;

import java.util.Date;
import org.glassfish.openesb.pojose.api.annotation.*;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.api.res.POJOContext;

/**
 *
 * @author gpatil
 */
@POJO (name="EveryMinuteJob",interfaceQN="{http://j2ee.netbeans.org/wsdl/jseScheduledJob/EveryMinuteJob}TriggerPortType",serviceQN="{http://j2ee.netbeans.org/wsdl/jseScheduledJob/EveryMinuteJob}TriggerPortTypeService")
public class EveryMinuteJob {
    
    /**
     * Constructor
     */
    public EveryMinuteJob() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     */
    @Operation
    public void FireTriggerOperation(String input) {
        System.out.println("Hello @" + new Date() + ":" + input);
    }
    // logger
    private static final Logger logger = Logger.getLogger(EveryMinuteJob.class.getName());
    //POJO Context
    @POJOResource
    private POJOContext jbiCtx;
}