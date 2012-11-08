/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst;

import java.util.List;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.OnDone;
import org.glassfish.openesb.pojose.api.annotation.OnReply;

/**
 *
 * @author gpatil
 */
@Provider 
public class POJOThrottler {
    // Logger
    private static final Logger logger = Logger.getLogger(POJOThrottler.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;

    private static int MAX_CONCURRENT_MSGS  = 5;
    private int totalMsgsSent = 3;
    private volatile AtomicInteger rcvd = new AtomicInteger(0);
    private List<String> yetToBeAckSentMsgs = new Vector<String>();
    private static enum TestType {SerialThrottle, Throttle};
    private TestType testType = TestType.SerialThrottle;

    @ConsumerEndpoint(name="ThrottledSvcConsumer", serviceQN="{ts}ThrottledSvc",
       interfaceQN="{http://svc.sun.com/ThrottledSvc/}ThrottledSvcInterface")
    private Consumer seriallyThrottledSvc ;

    @ConsumerEndpoint(name="ConcurrentlyThrottledSvcConsumer", serviceQN="{ts}ConcurrentlyThrottledSvc",
       interfaceQN="{http://svc.sun.com/ThrottledSvc/}ThrottledSvcInterface")
    private Consumer concurrentlyThrottledSvc ;

    
    /**
     * Constructor
     */
    public POJOThrottler() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://tst.sun.com/POJOThrottler/}POJOThrottlerOperationResponse")
    public void doThrottle(String input) {
        int concurrentRequests = 10;
        try {
            //int concurrentRequests = Integer.parseInt(input);
            this.totalMsgsSent = concurrentRequests;
        } catch (Exception ex){
            this.totalMsgsSent = 5;
        }

        logger.info("#####Sending " + this.totalMsgsSent + " messages...");

        String inputMessage = "Hello ";
        String nMsg = null;

        if (Counter.TestSerialThrottleStr.equalsIgnoreCase(input)){
            testType = TestType.SerialThrottle;
            Counter.CallRcvd.set(0);
            Counter.ConcurrentCallInProcess.set(0);
            Counter.MaxConcurrentCallRcvd.set(0);
        } else if (Counter.TestThrottleStr.equalsIgnoreCase(input)){
            testType = TestType.Throttle;
            Counter.CallRcvd.set(0);
            Counter.ConcurrentCallInProcess.set(0);
            Counter.MaxConcurrentCallRcvd.set(0);
        }

        try {
            for (int i=0; i< totalMsgsSent;i++){
                nMsg = inputMessage + i;
                this.yetToBeAckSentMsgs.add(nMsg);
                if (Counter.TestSerialThrottleStr.equalsIgnoreCase(input)){
                    seriallyThrottledSvc.sendInOut(nMsg);
                } else  if (Counter.TestThrottleStr.equalsIgnoreCase(input)){
                    concurrentlyThrottledSvc.sendInOut(nMsg);
                }
                logger.info("Sent:" + nMsg);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    @OnReply
    public void replies(ServiceEndpoint se, String param) {
        logger.info("Received: " + param);
        int rcvd = this.rcvd.incrementAndGet();
        logger.info("Received " + rcvd + " of " + this.totalMsgsSent);
        this.yetToBeAckSentMsgs.remove(param);
    }

    @OnDone
    public String done(){
        boolean retOk = false;
        logger.info("In OnDone, rcvd so far:" + this.rcvd.get());
        logger.info("Received Messages " + (this.totalMsgsSent - this.yetToBeAckSentMsgs.size()));
        logger.info("Max concurrency:" + Counter.MaxConcurrentCallRcvd.get());

        if (this.testType.equals(TestType.SerialThrottle)){
            if ((this.yetToBeAckSentMsgs.size() == 0) &&
                (this.rcvd.get() == this.totalMsgsSent) &&
                (Counter.CallRcvd.get() == this.totalMsgsSent) &&
                (Counter.MaxConcurrentCallRcvd.get() <= 1 )){
                return "" + Counter.TestSerialThrottleStr + ": Ok" ;
            } else {
                return "" + Counter.TestSerialThrottleStr + ": NOk" ;
            }
        } else if (this.testType.equals(TestType.Throttle)){
            if ((this.yetToBeAckSentMsgs.size() == 0) &&
                (this.rcvd.get() == this.totalMsgsSent) &&
                (Counter.CallRcvd.get() == this.totalMsgsSent) &&
                (Counter.MaxConcurrentCallRcvd.get() <= MAX_CONCURRENT_MSGS )){
                return "" + Counter.TestThrottleStr + ": Ok" ;
            } else {
                return "" + Counter.TestThrottleStr + ": NOk" ;
            }
        }

        return (retOk ? "Ok" : "NOk");
    }
}