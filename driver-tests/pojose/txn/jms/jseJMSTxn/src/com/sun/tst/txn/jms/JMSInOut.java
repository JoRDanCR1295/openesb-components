/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.txn.jms;

import com.sun.tst.txn.driver.Helper;
import java.util.logging.Level;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.Consumer;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.annotation.ConsumerEndpoint;
import org.glassfish.openesb.pojose.api.annotation.OnDone;
import org.glassfish.openesb.pojose.api.annotation.OnReply;

/**
 *
 * @author gpatil
 */
@Provider (name="JMSInOut",interfaceQN="{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSInOut}JMSPortType",
           serviceQN="{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSInOut}JMSPortTypeService")
public class JMSInOut {
    // Logger
    private static final Logger logger = Logger.getLogger(JMSInOut.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;

    @ConsumerEndpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSOut}epJMSOutPortTypService",
        interfaceQN = "{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSOut}JMSOutPortType",
        name = "epJMSOutPortTyp",
        operationQN = "{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSOut}JMSOutOperation",
        inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSOut}JMSInputMessage")
    private Consumer sepJMSOutPortType;

    @ConsumerEndpoint(serviceQN = "{http://j2ee.netbeans.org/wsdl/bpl2JMS/bplInOut}epBplInOutPortTypService",
        interfaceQN = "{http://j2ee.netbeans.org/wsdl/bpl2JMS/bplInOut}bplInOutPortType",
        name = "epBplInOutPortTyp",
        operationQN = "{http://j2ee.netbeans.org/wsdl/bpl2JMS/bplInOut}bplInOutOperation",
        inMessageTypeQN = "{http://j2ee.netbeans.org/wsdl/bpl2JMS/bplInOut}bplInOutOperationRequest")
    private Consumer sepBplInOutPortType;

    private String inp = null;
    /**
     * Constructor
     */
    public JMSInOut() {
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://j2ee.netbeans.org/wsdl/jseJMSTxn/JMSInOut}JMSOutputMessage")
    public void testJMSTxnOperation(String input) throws ErrorMessage{
        this.inp = input;
        try {
            sepJMSOutPortType.sendSynchInOnly(this.inp + "1");
            sepBplInOutPortType.sendInOut(this.inp, true);
            System.out.println("#####Sent:" + this.inp + "1");
            System.out.println("#####Sent2BPEL:" + this.inp);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        if (this.inp.equals(Helper.TEST_IN_OUT_TXN_NEGATIVE_OP_ERROR)){
            try {
                // Give sometime for BPEL to resume and suspend txn.
                Thread.sleep(1 * 500);
            } catch (InterruptedException ex) {
                Logger.getLogger(JMSInOut.class.getName()).log(Level.SEVERE, null, ex);
            }
            throw new ErrorMessage("Testing rollback for:" + this.inp);
        }
    }

    @OnReply
    public void onReply(ServiceEndpoint se, String response) throws ErrorMessage{
        if ((response == null) || (!response.equals(this.inp + "-BPELOutput"))){
            throw new ErrorMessage("Unexpected Output from BPEL");
        }

        sepJMSOutPortType.sendSynchInOnly(this.inp + "2");
        sepJMSOutPortType.sendInOnly(this.inp + "3", true);

        System.out.println("#####Sent:" + this.inp + "2");
        System.out.println("#####Sent:" + this.inp + "3");

        if (this.inp.equals(Helper.TEST_IN_OUT_TXN_NEGATIVE_OR_ERROR)){
            throw new ErrorMessage("Testing rollback for:" + this.inp);
        }
    }

    @OnDone
    public String onDone() throws ErrorMessage{
        String ret = this.inp + "-Ok";
        sepJMSOutPortType.sendSynchInOnly(this.inp + "4");
        System.out.println("#####Sent:" + this.inp + "4");
        if (this.inp.equals(Helper.TEST_IN_OUT_TXN_NEGATIVE_OD_ERROR)){
            throw new ErrorMessage("Testing rollback for:" + this.inp);
        }

        if (this.inp.equals(Helper.TEST_IN_OUT_TXN_NEGATIVE_OD_AS_ERROR)){
            sepJMSOutPortType.sendInOnly(this.inp + "99999", true);
        }

        try {
            // Give sometime for JMS BC broker to get messages back from messaging server.
            Thread.sleep(1 * 500);
        } catch (InterruptedException ex) {
            Logger.getLogger(JMSInOut.class.getName()).log(Level.SEVERE, null, ex);
        }

        return ret;
    }
}