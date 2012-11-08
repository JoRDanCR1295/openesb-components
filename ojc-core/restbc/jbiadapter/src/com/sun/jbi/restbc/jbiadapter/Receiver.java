package com.sun.jbi.restbc.jbiadapter;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.restbc.jbiadapter.mbeans.RuntimeConfig;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Receiver.java
 * 
 * @author Edward Chou
 */             
public class Receiver implements Runnable /*, NotificationListener */ {
    
    /*
     * 61-70
     */
    private static final Logger logger = Logger.getLogger(Receiver.class.getName());
    
    private RestComponent component;
    private ComponentContext context;
    private RestSUManager suManager;
    private RuntimeConfig runtimeConfig;
    
    // <exchangeId, msgExchangeContext>
    private ConcurrentMap<String, OutstandingMsgExchangeContext> outstandingMsgExchangeContexts = 
        new ConcurrentHashMap<String, OutstandingMsgExchangeContext> ();
    
    private Thread thisThread = null;
    private ThreadPoolExecutor pool;
    
    public Receiver(RestComponent component,
            ComponentContext context,
            RestSUManager suManager,
            RuntimeConfig runtimeConfig) {
        this.component = component;
        this.context = context;
        this.suManager = suManager;
        this.runtimeConfig = runtimeConfig;
        
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1061: creating NMR Receiver: threadPoolSize={0}, maxTheadPoolSize={1}, maxWaitMillis={2}, ", 
                    runtimeConfig.getNmrThreadPoolSize(), runtimeConfig.getNmrMaxThreadPoolSize(), 60000);//NOI18N
            logger.finest(msg);
        }
        
        pool = new ThreadPoolExecutor(runtimeConfig.getNmrThreadPoolSize(),
                runtimeConfig.getNmrMaxThreadPoolSize(),
                60000,
                TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable> ());
        
        // Subscribe for changes to the configuration
        //runtimeConfig.addNotificationListener(this, null, null);
    }
    
    
    public void startReceiving() {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1062: starting NMR Receiver");//NOI18N
            logger.finest(msg);
        }
        
        ThreadFactory factory = Executors.defaultThreadFactory();
        thisThread = factory.newThread(this);
        thisThread.start();
    }
    
    /**
     * Stop the receiver thread.
     */
    public void stopReceiving() {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1063: stopping NMR Receiver");//NOI18N
            logger.finest(msg);
        }
        
        thisThread.interrupt();
        
        /*
        try {
            runtimeConfig.removeNotificationListener(this);
        } catch (Exception e) {
            logger.log(Level.WARNING, "exception when remove Receiver as listener:", e);
        }
        */
    }

    public void run() {
        try {
            while (true) {
                MessageExchange msgExchange = context.getDeliveryChannel().accept();
                if (msgExchange != null) {
                    if (logger.isLoggable(Level.FINEST)) {
                        String msg = I18n.lf("RESTBC-1064: received a MessageExchange: id={0}", msgExchange.getExchangeId());//NOI18N
                        logger.finest(msg);
                    }
                    
                    pool.execute(new MessageProcessor(this, component, msgExchange, context, suManager));
                }
            }
        } catch (MessagingException me) {
            // assumes JBI framework will wrap InterruptedException inside MessagingException
            if (me.getCause() instanceof InterruptedException) {
                // because JBI framework does not call Thread.interrupted() method
                Thread.interrupted();
                //mLog.log(Level.INFO, "caught InterruptedException");
            } else {
                String msg = I18n.loc("RESTBC-7061: Error during DeliveryChannel.accept() {0}", me);//NOI18N
                logger.severe(msg);
            }
        } catch (Exception e) {
            String msg = I18n.loc("RESTBC-7062: Error during Receiver.run() {0}", e);//NOI18N
            logger.severe(msg);
        } finally {
            pool.shutdown();
            try {
                pool.awaitTermination(5000L, TimeUnit.MILLISECONDS);
            } catch (InterruptedException ie) {
                // do nothing
            }
        }
    }
    
    public OutstandingMsgExchangeContext removeOutstandingMsgExchangeContext(String exchangeId) {
        return outstandingMsgExchangeContexts.remove(exchangeId);
    }
    
    public boolean putOutstandingMsgExchangeContext(String exchangeId, OutstandingMsgExchangeContext outstandingMsgExchangeContext) {
        Object prevValue = outstandingMsgExchangeContexts.putIfAbsent(exchangeId, outstandingMsgExchangeContext);
        if (prevValue != null) {
            return false;
        }
        return true;
    }

    /*
    public void handleNotification(Notification notification, Object handback) {
        
        if (notification instanceof AttributeChangeNotification) {
            AttributeChangeNotification acn =(AttributeChangeNotification) notification;
            String attrName = acn.getAttributeName();
            if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_MAX_SIZE.equals(attrName)) {
                int newVal = (Integer) acn.getNewValue();
                pool.setMaximumPoolSize(newVal);
            } else if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_CORE_SIZE.equals(attrName)) {
                int newVal = (Integer) acn.getNewValue();
                pool.setCorePoolSize(newVal);
            } else if (RuntimeConfigurationImpl.PROP_OUTBOUND_THREAD_POOL_KEEP_ALIVE_IN_MILLIS.equals(attrName)) {
                long newVal = (Long) acn.getNewValue();
                pool.setKeepAliveTime(newVal, TimeUnit.MILLISECONDS);
            }
        }
        
    }
    */
    
}
