/*
 * DefaultMessageExchangeReceiver.java
 *
 */

package net.openesb.component.ServiceEngine-archetype.common;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * This class implements MessageExchangeReceiver interface. This is a default implementation that
 * demonstrates the multi-threaded environment to receive and process message exchanges from the 
 * delivery channel. It uses a main thread to receive message exchanges from the delivery channel
 * and then processes the received message exchanges in a individual threads from the thread pool.
 * The controller methods of MessageExchangeReceiver controls the main thread and the thread pool
 * execution.
 *
 * @author chikkala
 */
public class DefaultMessageExchangeReceiver implements MessageExchangeReceiver {
    
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
    public DefaultMessageExchangeReceiver() {
    }
    /**
     * this method is called from the ComponentLifecyle.init method of the AbstractComponentLifecycle
     * to initialize the message exchange receiving resources such as threads and thread pools
     * throws JBIException on error
     */    
    public final void initReceiver() throws JBIException {
        
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
    /**
     * this method is called from the ComponentLifecyle.shutdown method of the AbstractComponentLifecycle
     * to cleanup the message exchange receiving resources such as threads and thread pools.
     * throws JBIException on error
     */        
    public final void shutdownReceiver() throws JBIException {
        
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
    /**
     * shutdown all the working threads from the thread pool.
     */
    private final void shutdownHandlers() throws JBIException {
        
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
    /**
     * this method is called from the ComponentLifecyle.start method of the AbstractComponentLifecycle
     * to start receiving the message exchanges from the delivery channel and process them.
     * throws JBIException on error
     */            
    public final void startProcessing() throws JBIException {
        
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = true;
        }
    }
    /**
     * this method is called from the ComponentLifecyle.stop method of the AbstractComponentLifecycle
     * to stop receiving the message exchanges from the delivery channel.
     * throws JBIException on error
     */        
    public final void stopProcessing() throws JBIException {
        
        synchronized ( this.mCanAccept ) {
            this.mCanAccept = false;
        }
    }
    
    protected MessageExchangeSupport getMessageExchangeSupport() {
        return RuntimeHelper.getMessageExchangeSupport();
    }
    
    private void receiveAndProcessMessageExchange() {
        try {
            DeliveryChannel channel = RuntimeHelper.getDeliveryChannel();
            if ( channel == null ) {
                RuntimeHelper.logDebug("DeliveryChannel Not Opened for receiving messages");
                return;
            }
            final MessageExchange msgExchange = channel.accept(DC_ACCEPT_TIME_OUT);
            if ( msgExchange == null ) {
                // delivery channel timeout occurred. do nothing.
                // RuntimeHelper.logDebug("DeliveryChannel returned null message exchange from accept");
                return;
            }
            final ExchangeStatus status = msgExchange.getStatus();
            // process message exchange in a separate thread
            this.mHandlerThreadPool.execute(new Runnable() {
                public void run() {
                    getMessageExchangeSupport().fireMessageExchangeReceived(status, msgExchange);
                }
            });
            
        } catch (MessagingException ex) {
            RuntimeHelper.logWarning(ex);
            ex.printStackTrace();
        }
    }
}
