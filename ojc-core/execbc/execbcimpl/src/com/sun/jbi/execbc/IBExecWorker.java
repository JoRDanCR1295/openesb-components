/*
 * IBExecWorker.java
 * 
 * Created on May 11, 2007, 9:51:24 AM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.execbc;

import java.io.File;
import javax.xml.namespace.QName;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import com.sun.jbi.internationalization.Messages;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.TimeUnit;
import org.xml.sax.SAXException;

import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.util.ExecUtil;
import com.sun.jbi.execbc.Endpoint.EndpointMessageType;

/**
 * IBExecWorker processes one file at a time - normalize 
 * the content of the file as one or more (when file contains multiple records)
 * messages and push the messages into NMR, also carry out
 * post message send operations, i.e., labeling & archiving the processed
 * files.
 * 
 * IBExecWorker is created by InboundMessageProcessor and it waits on 
 * <code>queue</code> of type LinkedBlockingQueue which contains files
 * to be processed, InboundMessageProcessor is responsible to pump File objects
 * into the queue.
 * 
 * @author jfu
 */
public class IBExecWorker implements Runnable {
    private static final Messages mMessages =
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(IBExecWorker.class);

    private ExecNormalizer mNormalizer;
    
    private InboundMessageProcessor mIBProcessor;
    private String mMEP;
    private ExecMessage mExecMessage;

    public IBExecWorker(
        InboundMessageProcessor ibProc, 
        String mep, 
        ExecMessage execMessage) throws Exception {
        mIBProcessor = ibProc;
        mMEP = mep;
        mExecMessage = execMessage;
        // allocate a normalizer for each worker
        // since, e.g. transformer in the normalizer could not be used concurrently
        // by more than one thread;
        mNormalizer = new ExecNormalizer();
    }

    public void run() {

        while ( true ) {
            CommandResult cmdResult = null;
            try {
                cmdResult =
                    (CommandResult) mIBProcessor.getInputFileQueue().poll(
                            5000, TimeUnit.MILLISECONDS);
            } catch (InterruptedException ex) {
                // interrupted - continue after a pause
                try {
                    Thread.sleep(1000);
                } catch (Exception ex2) {
                    // ignore
                }
            }
            if (cmdResult != null ) {
                if ( mLogger.isLoggable(Level.FINEST)) {
                    mLogger.log(Level.FINEST, "Process message: "
                            + new String(cmdResult.getResult()));
                }
                // send single message:
                try {
                    processMessage(mMEP, mExecMessage, cmdResult);
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE, mMessages.getString("IMP_Failed_processing_result", GregorianCalendar.getInstance()), ex);
                }
            } else {
                // timeout - no work to do - check if the inbound processor is stopping
                if ( mIBProcessor.isStopped() ) {
                    // stop when inbound processor stopped
                    if ( mLogger.isLoggable(Level.INFO) ) {
                        mLogger.log(Level.INFO, "Shutdown since inbound processor has been shutdown...");
                    }
                    break;
                }
            }
        }
    }

    private void processMessage(String mep, ExecMessage execMessage,
            CommandResult cmdResult)
            throws MessagingException, SAXException, IOException, Exception {
        MessageExchange exchange = null;
        String exchangeId = null;
        if (cmdResult == null) {
            throw new Exception(mMessages.getString("IMP_Invalid_Data"));
        }

        Exception err = null;
        try {
            if (mep.equals(EndpointMessageType.IN_ONLY)) {
                exchange = mIBProcessor.getMsgExchangeFactory().createInOnlyExchange();
            } else {
                throw new UnsupportedOperationException(
                        "IN_OUT is not supported for Exec BC.");
            }
//            } else if (mep.equals(EndpointMessageType.IN_OUT)) {
//                exchange = mIBProcessor.getMsgExchangeFactory().createInOutExchange();
//            }
            exchangeId = exchange.getExchangeId();
            if (exchangeId != null) {
                mIBProcessor.getInboundReplyIds().put(exchangeId, cmdResult.getID());
            }
            NormalizedMessage inMsg = mNormalizer.normalize(exchange,
                    mIBProcessor.getOperationName(),
                    mIBProcessor.getEndpoint(),
                    execMessage,
                    cmdResult.getResult());
            exchange.setEndpoint(mIBProcessor.getServiceEndpoint());
            exchange.setOperation(mIBProcessor.getOperationName());
            exchange.setMessage(inMsg, InboundMessageProcessor.IN_MSG);
            exchangeId = exchange.getExchangeId();
            mIBProcessor.getInboundExchanges().put(exchangeId, new ListenerMeta(System.currentTimeMillis(), mIBProcessor));
            mIBProcessor.getDelivaryChannel().send(exchange);
            mIBProcessor.getEndpoint().getEndpointStatus().incrementSentRequests();
        } catch(MessagingException ex) {
            err = ex;
        } catch (SAXException ex) {
            err = ex;
        } catch(IOException ex) {
            err = ex;
        } catch (Exception ex) {
            err = ex;
        }
        if ( err != null ) {
            if (exchangeId != null) {
                mIBProcessor.getInboundExchanges().remove(exchangeId);
            }
            throw err;
        }
    }

    public void setNormalizer(ExecNormalizer normlzer) {
        mNormalizer = normlzer;
    }

    public ExecNormalizer getNormalizer() {
        return mNormalizer;
    }
}
