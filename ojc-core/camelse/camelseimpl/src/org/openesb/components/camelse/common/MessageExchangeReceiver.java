/*
 * MessageExchangeReceiver.java
 *
 */

package org.openesb.components.camelse.common;

import javax.jbi.JBIException;

/**
 * This interface provides the methods can controls the receiving and processing of the message 
 * exchange objects from the delivery channel during the component lifecycle operations.
 * The implementation of this interface can use multi-threaded environment to receive and process
 * message exchanges from the delivery channel during the component lifecycle.
 * @see com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver
 * @author chikkala
 */
public interface MessageExchangeReceiver {
    /**
     * this method is called from the ComponentLifecyle.init method of the AbstractComponentLifecycle
     * to initialize the message exchange receiving resources such as threads and thread pools
     * throws JBIException on error
     */
    void initReceiver() throws JBIException;
    /**
     * this method is called from the ComponentLifecyle.start method of the AbstractComponentLifecycle
     * to start receiving the message exchanges from the delivery channel and process them.
     * throws JBIException on error
     */    
    void startProcessing() throws JBIException;    
    /**
     * this method is called from the ComponentLifecyle.stop method of the AbstractComponentLifecycle
     * to stop receiving the message exchanges from the delivery channel.
     * throws JBIException on error
     */    
    void stopProcessing() throws JBIException;
    /**
     * this method is called from the ComponentLifecyle.shutdown method of the AbstractComponentLifecycle
     * to cleanup the message exchange receiving resources such as threads and thread pools.
     * throws JBIException on error
     */    
    void shutdownReceiver() throws JBIException;    
    
}
