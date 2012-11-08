/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * MessageExchangeReceiver.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;import it.imolinfo.jbi4ejb.LoggerFactory;import it.imolinfo.jbi4ejb.jbi.Messages;import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * This main purpose of this class is to manage receiving the MessageExchange
 * object from the DeliveryChannel and process them by delegating the processing
 * of the MessageExchange object to MessageExchangeHandlers configured for the
 * component. It also provides the controller methods to start and stop
 * processing the MessageExchange objects which will be used when the component
 * lifecycle is controlled.
 *
 * This class creates a single thread to receive the MessageExchange objects
 * from the delivery channel and then finds the appropriate message exchange
 * handler to process the message exchange. Each message exchange handler will
 * be executed in a separate thread from a pool of handler threads.
 *
 * @author Sun Microsystems, Inc.
 */
public class MessageExchangeReceiver {
	/** The logger. */	private static final Logger LOG = LoggerFactory.getLogger(MessageExchangeReceiver.class);        private static final Messages MESSAGES = Messages.getMessages(MessageExchangeReceiver.class);   
    /** delivery channel accept time out */
    private final static long DC_ACCEPT_TIME_OUT = 3000; // milliseconds
    
    /** receiver thread wait time before polling for messages after woke up **/
    private final static long RECEIVER_WAIT_TIME = 2000; // milliseconds
    
    /** receiver thread wait time before force shutdown */
    private final static long RECEIVER_SHUTDOWN_WAIT_TIME = 10; // seconds
    
    /** handler threads wait time before forced shutdown  */
    private final static long HANDLERS_SHUTDOWN_WAIT_TIME = 30; // seconds
    /** handler thread pool size */
    private final static int HANDLER_THREAD_POOL_SIZE = 5;
    /** receiver thread accept message exchange condition */
    private Boolean mCanAccept = false;
    /** receiver thread termination condition */
    private Boolean mContinue = true;
    /** receiver thread executor service */
    private ExecutorService mReceiverThreadMgr;
    
    /** handler thread executor service */
    private ExecutorService mHandlerThreadPool;
    
    /** no default constructor for extended classes */
    public MessageExchangeReceiver() throws Exception {
    }
    
    public final void initReceiver() throws Exception {
        
        this.mHandlerThreadPool = Executors.newFixedThreadPool(HANDLER_THREAD_POOL_SIZE);
        this.mReceiverThreadMgr = Executors.newSingleThreadExecutor();
        
        this.mReceiverThreadMgr.execute(new Runnable() {
            public void run() {
                Thread t = Thread.currentThread();
                while ( mContinue ) {
                    if (mCanAccept) {
                        receiveAndProcessMessageExchange();
                    } else {
                        try {
                            t.sleep(RECEIVER_WAIT_TIME);
                        } catch (InterruptedException interruptException) {
                            // someone must have interrupted this thread
                            // do nothing
                            RuntimeHelper.logDebug("Interrupted the MessageReceiverThread in Sleep");
                        }
                    }
                }
            }
        });
    }
    
    public final void shutdownReceiver() throws Exception {
        
        synchronized ( mContinue ) {
            mContinue = false;
        }
        boolean terminated = false;
        try {
            this.mReceiverThreadMgr.shutdown();
            terminated = this.mReceiverThreadMgr.awaitTermination(
                RECEIVER_SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            RuntimeHelper.logDebug(ex);
        } finally {
            if ( !terminated ) {
                RuntimeHelper.logDebug("Message Receiver not shutdown. Forcing shutdown");
                this.mReceiverThreadMgr.shutdownNow();
            }
        }
        shutdownHandlers();
    }
    
    private final void shutdownHandlers() throws Exception {
        
        boolean terminated = false;
        try {
            this.mHandlerThreadPool.shutdown();
            terminated = this.mHandlerThreadPool.awaitTermination(
                HANDLERS_SHUTDOWN_WAIT_TIME, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            RuntimeHelper.logDebug(ex);
        } finally {
            if ( !terminated ) {
                RuntimeHelper.logDebug("Handler threads not shutdown. Forcing shutdown");
                this.mHandlerThreadPool.shutdownNow();
            }
        }
    }
    
    public final void startProcessing() throws Exception {
        
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = true;
        }
    }
    
    public final void stopProcessing() throws Exception {
        
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = false;
        }
    }
    
    private void receiveAndProcessMessageExchange() {
        
        try {
            
            DeliveryChannel channel = RuntimeHelper.getDeliveryChannel();
            MessageExchange msgExchange = null;
            
            if ( channel == null ) {
                RuntimeHelper.logDebug("DeliveryChannel Not Opened for receiving messages");
                return;
            }
            
            msgExchange = channel.accept(DC_ACCEPT_TIME_OUT);
            
            if ( msgExchange == null ) {
                // RuntimeHelper.logDebug("DeliveryChannel returned null message exchange from accept");
                return;
            }
            
            MessageExchangeHandler handler = findMessageExchangeHandler(msgExchange);
            
            if ( handler == null ) {            	LOG.error("EJB000224_Message_ExchangeHandler_NULL", new Object[] {msgExchange});                
                return;
            }
            // process message exchange. This could be done in a separate thread.
            handler.setMessageExchange(msgExchange);
            //  handler.processMessageExchange();
            // try using the pool
            this.mHandlerThreadPool.execute(handler);
            
        } catch (MessagingException ex) {        	LOG.info("EJB000225_Warning_in_preceive_and_process_message_exchange", new Object[] {ex});
            ex.printStackTrace();
        }
    }
    
    private MessageExchangeHandler findMessageExchangeHandler(MessageExchange msgExchange) {
        MessageExchangeHandler handler = null;
        handler = RuntimeContext.getInstance().newMessageExchangeHandler(msgExchange);
        return handler;
    }
    
}
