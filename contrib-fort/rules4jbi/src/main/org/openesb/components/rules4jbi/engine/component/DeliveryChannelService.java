/*
 * @(#)DeliveryChannelService.java        $Revision: 1.3 $ $Date: 2008/07/14 16:30:26 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import com.google.inject.Inject;
import com.google.inject.name.Named;

import org.openesb.components.rules4jbi.shared.logging.Logger;

import static org.openesb.components.rules4jbi.engine.util.ConcurrencyUtils.shutdownExecutorService;

/**
 * This class encapsulates the interaction with the <code>DeliveryChannel</code>.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @since 0.1
 */
public class DeliveryChannelService {
    
    private static final long SEND_SYNC_TIMEOUT = 60000;
    
    @Inject @Named("DeliveryChannelService")
    private Logger logger;
    
    @Inject
    private ComponentContext componentContext;
    
    private DeliveryChannel deliveryChannel;

    private volatile boolean isStarted = false;
    
    private LinkedBlockingQueue<MessageExchange> inboundQueue;
    
    private LinkedBlockingQueue<MessageExchange> outboundQueue;

    private ExecutorService receiver;

    private ExecutorService sender;
    
    private Future<?> receiverTask;

    public void init() throws MessagingException {
        logger.entering(this.getClass(), "init");
        
        isStarted = false;
    
        inboundQueue = new LinkedBlockingQueue<MessageExchange>();
        outboundQueue = new LinkedBlockingQueue<MessageExchange>();
        
        receiver = Executors.newSingleThreadExecutor();
        sender = Executors.newSingleThreadExecutor();
        
        logger.fine("Opening delivery chanel");
        deliveryChannel = componentContext.getDeliveryChannel();
    }
    
    public void start() {
        logger.entering(this.getClass(), "start");
        
        isStarted = true;
        
        sender.execute(new SenderTask());
        receiverTask = receiver.submit(new ReceiverTask());
    }
    
    public void stop() {
        logger.entering(this.getClass(), "stop");
        
        isStarted = false;
        
        receiverTask.cancel(true);
        
        logger.fine("Putting terminal message into the inbound queue");
        inboundQueue.add(new EmptyMessageExchange());
    }
    
    public void shutDown() throws MessagingException {
        logger.entering(this.getClass(), "shutDown");
        
        if (isStarted) {
            throw new IllegalStateException("Shutdown can be called only after the service has been stopped.");
        }
        
        shutdownExecutorService(logger, "Receiver", receiver);
        shutdownExecutorService(logger, "Sender", sender);
        
        try {
            if (deliveryChannel != null) {
                deliveryChannel.close();
            }
        } catch (MessagingException e) {
            logger.severe("Exception occured while closing the delivery channel", e);
            throw e;
            
        } finally {
            deliveryChannel = null;
        }
    }
    
    public MessageExchange receive() throws InterruptedException {
        return inboundQueue.take();
    }
    
    public void send(MessageExchange messageExchange) {
        outboundQueue.add(messageExchange);
    }
    
    /** Task responsible for listening on the delivery channel for incoming messages from the NMR. */
    class ReceiverTask implements Runnable {

        public void run() {
            while (isStarted) {
                try {
                    
                    /*
                     * Note that according to the JBI spec, page 158, the method DeliveryChannel.accept()
                     * is interruptable, and throws the MessagingException when the calling thread is interrupted.
                     */ 
                    MessageExchange me = deliveryChannel.accept();
                    
                    if (me != null) {
                        inboundQueue.add(me);
                    }

                } catch (MessagingException e) {
                    if (!isStarted) {
                        logger.fine("Receiver received interruption request");

                    } else {
                        logger.severe("Unknown error occured", e);
                    }
                }
            }
            
            logger.fine("Receiver has terminated");
        }
    }

    /** Task responsible for sending the messages to the NMR through the delivery channel. */
    class SenderTask implements Runnable {

        public void run() {
            while (isStarted || outboundQueue.size() > 0) {
                try {
                    MessageExchange me = outboundQueue.take();
                    
                    if (me instanceof EmptyMessageExchange) {
                        logger.fine("Sender received terminal message");
                        logger.fine("Sender status: started=%b, queue_size=%d", isStarted, outboundQueue.size());
                        
                        break;
                    }

                    logger.fine("Got message exchange to send with status: %s", me.getStatus());

                    try {
                        deliveryChannel.sendSync(me, SEND_SYNC_TIMEOUT);

                    } catch (MessagingException e) {
                        logger.severe("Unable to send the message exchange: %s", me);
                    }

                    logger.fine("The status of the sent message is: %s", me.getStatus());
                    
                } catch (InterruptedException e) {
                    logger.fine("Sender received interruption request");
                }
            }
            
            logger.fine("Sender has terminated");
        }
    }
}
