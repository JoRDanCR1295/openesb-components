/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.asynch.inouts;

import java.util.List;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.annotation.OnReply;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.OnDone;

/**
 *
 * @author gpatil
 */
@Provider 
public class ASynchInOutSvc {
    // Logger
    private static final Logger logger = Logger.getLogger(ASynchInOutSvc.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;
    
    @ConsumerEndpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/bplEcho/echo}epEchoPortTypeEchoOperatioService", 
    interfaceQN = "{http://j2ee.netbeans.org/wsdl/bplEcho/echo}echoPortType",
    name = "epEchoPortTypeEchoOperatio",
    operationQN = "{http://j2ee.netbeans.org/wsdl/bplEcho/echo}echoOperation",
    inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bplEcho/echo}echoOperationRequest")
    private Consumer bpelSvc;

    private int totalMsgsSent = 3;
    private volatile AtomicInteger rcvd = new AtomicInteger(0);
    private List<String> sentMsgs = new Vector<String>();
    /**
     * Constructor
     */
    public ASynchInOutSvc() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://inouts.asynch.tst.sun.com/ASynchInOutSvc/}ASynchInOutSvcOperationResponse")
    public String theOperation(String input) {
        try {
            int j = Integer.parseInt(input);
            this.totalMsgsSent = j;
        } catch (Exception ex){
            //
            this.totalMsgsSent = 5;
        }

        System.out.println("#####Sending " + this.totalMsgsSent + " messages...");
        /* Consumer Invoke - Begin */ {
            String inputMessage = "Hello ";
            String nMsg = null;
            try {
                for (int i=0; i< totalMsgsSent;i++){
                    nMsg = inputMessage + i;
                    this.sentMsgs.add(nMsg);
                    bpelSvc.sendInOut(nMsg);
                    System.out.println("Sent:" + nMsg);
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } /* Consumer Invoke - End */

        return input;        
    }

    @OnReply
    public void replies(ServiceEndpoint se, String param) {
        System.out.println("Received: " + param);
        int rcvdSoFar = this.rcvd.incrementAndGet();
        System.out.println("Received " + rcvdSoFar + " of " + this.totalMsgsSent);
        this.sentMsgs.remove(param);
    }

    @OnDone
    public String done(){
        System.out.println("In OnDone, rcvd so far:" + this.rcvd.get());
        return "Received Messages " + (totalMsgsSent - this.sentMsgs.size());
    }
}