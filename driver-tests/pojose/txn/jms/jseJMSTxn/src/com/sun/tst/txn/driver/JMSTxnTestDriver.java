/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.tst.txn.driver;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import javax.jms.JMSException;
import org.glassfish.openesb.pojose.api.annotation.Provider;
import org.glassfish.openesb.pojose.api.annotation.Resource;
import org.glassfish.openesb.pojose.api.annotation.Operation;
import org.glassfish.openesb.pojose.api.res.Context;
import java.util.logging.Logger;
import javax.jms.ConnectionFactory;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;
import org.glassfish.openesb.pojose.api.ErrorMessage;

/**
 *
 * @author gpatil
 */
@Provider 
public class JMSTxnTestDriver {
    // Logger
    private static final Logger logger = Logger.getLogger(JMSTxnTestDriver.class.getName());
    // POJO Context
    @Resource
    private Context jbiCtx;

    private ConnectionFactory cf;
    private Session session;
    private javax.jms.Connection jmsConn;

    private String input = null;
    /**
     * Constructor
     */
    public JMSTxnTestDriver() {
    }

    private void initInternal(){
        try {
            Helper.clearPropCache();
            this.cf = (ConnectionFactory) this.jbiCtx.getNamingContext().lookup(Helper.getJMSJndiPath());
            this.jmsConn = cf.createConnection();
            this.session = jmsConn.createSession(false, Session.AUTO_ACKNOWLEDGE);
            this.jmsConn.start();
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void close(Object closable){
        try {
            Method m = closable.getClass().getMethod("close", null);
            m.invoke(closable, null);
        } catch (Exception ex){
            //ign
        }
    }

    private void closeResouces(){
        try {
            this.jmsConn.stop();
        } catch (Throwable ex){
            //ign
        }
        close(this.session);
        close(this.jmsConn);
    }

    private Queue sendMessage(Queue queue, Queue replyQ, String msg) throws JMSException{
        TextMessage tm = session.createTextMessage();
        tm.setText(msg);

        if (replyQ != null){
            tm.setJMSReplyTo(replyQ);
        }
        
        MessageProducer mp = session.createProducer(queue);
        mp.send(tm);
        mp.close();
        return replyQ;
    }

    private List<String> getMessages(String queue) throws ErrorMessage{
        try {
            Queue q = session.createQueue(queue);
            return getMessages(q);
        } catch (JMSException ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }
    private List<String> getMessages(Queue queue) throws ErrorMessage{
        MessageConsumer cons = null;
        try {
            List<String> msgs = new ArrayList<String>();
            cons = this.session.createConsumer(queue);
            Message msg = cons.receive(500);
            TextMessage tm = null;
            while (msg != null){
                if (msg instanceof TextMessage){
                    tm = (TextMessage) msg;
                    msgs.add(tm.getText());
                    //msg.acknowledge();
                }
                msg = cons.receiveNoWait();
            }
            return msgs;
        } catch (JMSException ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        } finally {
            if (cons != null){
                try {
                    cons.close();
                } catch (Exception ex){
                    //
                }
            }
        }
    }

    private boolean verifyMessages(List<String> actual, String[] expected){
        boolean ret = false;

        if (expected != null){
            if (expected.length == actual.size()){
                ret = true;
                
                for(String s: expected){
                    if (!actual.contains(s)){
                        logger.severe("Actual list does not contain expected message:" + s);
                        ret = false;
                        break;
                    }
                }
            } else{
                
                logger.severe("Actual and expected messages size did not match: actual ="
                        + actual.size() + " expected = " + expected.length);

                StringBuilder sb = new StringBuilder();
                for (String a: expected){
                    sb.append(":");
                    sb.append(a);
                }
                logger.severe("Expected msgs:" + sb.toString());

                sb = new StringBuilder();
                for (String a: actual){
                    sb.append(":");
                    sb.append(a);
                }
                logger.severe("Actual msgs:" + sb.toString());
                ret = false;
            }
        }

        return ret;
    }

    private boolean verifyMessages(List<String> actual, String[] expected, String[] optionalExpected){
        boolean ret = false;

        if (expected != null){
            if ((expected.length == actual.size()) ||
                    ((expected.length + optionalExpected.length)) >= actual.size()){
                ret = true;

                List<String> allexpexted = new ArrayList<String>();
                allexpexted.addAll(Arrays.asList(expected));
                allexpexted.addAll(Arrays.asList(optionalExpected));
                for(String s: actual){
                    if (!allexpexted.contains(s)){
                        logger.severe("Unexpected actual message:" + s);
                        ret = false;
                        break;
                    }
                }

                if (ret){
                    for(String s: optionalExpected){
                        if (actual.contains(s)){
                            //just for infor and debugging.
                            logger.severe("Actual contained optionally expected message:" + s);
                            ret = false;
                            break;
                        }
                    }
                }
            } else{
                logger.severe("Actual and expected messages size did not match: actual ="
                        + actual.size() + " expected = " + expected.length);

                StringBuilder sb = new StringBuilder();
                for (String a: expected){
                    sb.append(":");
                    sb.append(a);
                }
                logger.severe("Expected msgs:" + sb.toString());

                sb = new StringBuilder();
                for (String a: actual){
                    sb.append(":");
                    sb.append(a);
                }
                logger.severe("Actual msgs:" + sb.toString());
                ret = false;
            }
        }

        return ret;
    }

    private String testInOutTxnPositive(String input) throws ErrorMessage{
        try {
            String ret = input;

            Queue tq = session.createQueue("POJOSETest1TriggerQueue");
            Queue respQ = session.createQueue("POJOSETest1TriggerResponseQueue");
            Queue outQ = session.createQueue("POJOSETest1OutQueue");

            //Flushout old values
            getMessages(tq);
            getMessages(respQ);
            getMessages(outQ);

            //Send message to Queue POJOSETest1TriggerQueue
            sendMessage(tq, respQ, input);
            //Wait for 3 secs
            Thread.sleep(500);
            //Get response from POJOSETest1TriggerResponseQueue
            List<String> respMsgs = getMessages(respQ);
            //Get Msgs from POJOSETest1OutQueue
            List<String> outMsgs = getMessages(outQ);

            String[] expectedOutput = {"TestInOutTxnPositive1", "TestInOutTxnPositive2", "TestInOutTxnPositive3", "TestInOutTxnPositive4", "TestInOutTxnPositive-BPEL2JMS"};
            String[] expectedResp = {"TestInOutTxnPositive-Ok"};
            
            if (verifyMessages(outMsgs, expectedOutput) && verifyMessages(respMsgs, expectedResp)){
                ret = input + "-Ok" ;
            } else {
                ret = input + "-NOk" ;
            }
            return ret;
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }

    private String testInOutTxnNegOpError(String input) throws ErrorMessage{
        try {
            String ret = input;

            Queue tq = session.createQueue("POJOSETest1TriggerQueue");
            Queue respQ = session.createQueue("POJOSETest1TriggerResponseQueue");
            Queue outQ = session.createQueue("POJOSETest1OutQueue");

            //Flushout old values
            getMessages(tq);
            getMessages(respQ);
            getMessages(outQ);

            //Send message to Queue POJOSETest1TriggerQueue
            sendMessage(tq, respQ, input);
            //Wait for 3 secs
            Thread.sleep(3 * 1000);
            //Get response from POJOSETest1TriggerResponseQueue
            List<String> respMsgs = getMessages(respQ);
            //Get Msgs from POJOSETest1OutQueue
            List<String> outMsgs = getMessages(outQ);

            String[] expectedOutput = {};
            String[] optExpectedOutput = {"TestInOutTxnNegativeOpError-BPEL2JMS"}; // Actually an error in JMS BC via BPEL?
            String[] expectedResp = {};
            if (verifyMessages(outMsgs, expectedOutput, optExpectedOutput) && verifyMessages(respMsgs, expectedResp)){
                ret = input + "-Ok" ;
            } else {
                ret = input + "-NOk" ;
            }
            return ret;
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }

    private String testInOutTxnNegOrError(String input) throws ErrorMessage{
        try {
            String ret = input;

            Queue tq = session.createQueue("POJOSETest1TriggerQueue");
            Queue respQ = session.createQueue("POJOSETest1TriggerResponseQueue");
            Queue outQ = session.createQueue("POJOSETest1OutQueue");

            //Flushout old values
            getMessages(tq);
            getMessages(respQ);
            getMessages(outQ);

            //Send message to Queue POJOSETest1TriggerQueue
            sendMessage(tq, respQ, input);
            //Wait for 3 secs
            Thread.sleep(3 * 1000);
            //Get response from POJOSETest1TriggerResponseQueue
            List<String> respMsgs = getMessages(respQ);
            //Get Msgs from POJOSETest1OutQueue
            List<String> outMsgs = getMessages(outQ);

            String[] expectedOutput = {};
            String[] expectedResp = {};
            if (verifyMessages(outMsgs, expectedOutput) && verifyMessages(respMsgs, expectedResp)){
                ret = input + "-Ok" ;
            } else {
                ret = input + "-NOk" ;
            }
            return ret;
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }

    private String testInOutTxnNegOdError(String input) throws ErrorMessage{
        try {
            String ret = input;

            Queue tq = session.createQueue("POJOSETest1TriggerQueue");
            Queue respQ = session.createQueue("POJOSETest1TriggerResponseQueue");
            Queue outQ = session.createQueue("POJOSETest1OutQueue");

            //Flushout old values
            getMessages(tq);
            getMessages(respQ);
            getMessages(outQ);

            //Send message to Queue POJOSETest1TriggerQueue
            sendMessage(tq, respQ, input);
            //Wait for 3 secs
            Thread.sleep(3 * 1000);
            //Get response from POJOSETest1TriggerResponseQueue
            List<String> respMsgs = getMessages(respQ);
            //Get Msgs from POJOSETest1OutQueue
            List<String> outMsgs = getMessages(outQ);

            String[] expectedOutput = {};
            String[] expectedResp = {};
            if (verifyMessages(outMsgs, expectedOutput) && verifyMessages(respMsgs, expectedResp)){
                ret = input + "-Ok" ;
            } else {
                ret = input + "-NOk" ;
            }
            return ret;
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }

    private String testInOutTxnNegOdAsError(String input) throws ErrorMessage{
        try {
            String ret = input;

            Queue tq = session.createQueue("POJOSETest1TriggerQueue");
            Queue respQ = session.createQueue("POJOSETest1TriggerResponseQueue");
            Queue outQ = session.createQueue("POJOSETest1OutQueue");

            //Flushout old values
            getMessages(tq);
            getMessages(respQ);
            getMessages(outQ);

            //Send message to Queue POJOSETest1TriggerQueue
            sendMessage(tq, respQ, input);
            //Wait for 3 secs
            Thread.sleep(3 * 1000);
            //Get response from POJOSETest1TriggerResponseQueue
            List<String> respMsgs = getMessages(respQ);
            //Get Msgs from POJOSETest1OutQueue
            List<String> outMsgs = getMessages(outQ);

            String[] expectedOutput = {};
            String[] expectedResp = {};
            if (verifyMessages(outMsgs, expectedOutput) && verifyMessages(respMsgs, expectedResp)){
                ret = input + "-Ok" ;
            } else {
                ret = input + "-NOk" ;
            }
            return ret;
        } catch (Exception ex) {
            Logger.getLogger(JMSTxnTestDriver.class.getName()).log(Level.SEVERE, null, ex);
            throw new ErrorMessage("Exception while testing test " + input, ex);
        }
    }
    
    /**
     * POJO Operation
     *
     * @param input input of type String input
     * @return String
     */
    @Operation (outMessageTypeQN="{http://driver.txn.tst.sun.com/JMSTxnTestDriver/}JMSTxnTestDriverOperationResponse")
    public String receive(String inp) throws ErrorMessage{
        this.input = inp;
        String ret = inp;
        try {
            initInternal();
            if (Helper.TEST_IN_OUT_TXN_POSITIVE.equals(inp)){
                ret = testInOutTxnPositive(inp);
            } else if (Helper.TEST_IN_OUT_TXN_NEGATIVE_OP_ERROR.equals(inp)){
                ret = testInOutTxnNegOpError(inp);
            }else if (Helper.TEST_IN_OUT_TXN_NEGATIVE_OR_ERROR.equals(inp)){
                ret = testInOutTxnNegOrError(inp);
            }else if (Helper.TEST_IN_OUT_TXN_NEGATIVE_OD_ERROR.equals(inp)){
                ret = testInOutTxnNegOdError(inp);
            }else if (Helper.TEST_IN_OUT_TXN_NEGATIVE_OD_AS_ERROR.equals(inp)){
                ret = testInOutTxnNegOdAsError(inp);
            }
        } finally {
            closeResouces();
        }
        return ret;
    }
}