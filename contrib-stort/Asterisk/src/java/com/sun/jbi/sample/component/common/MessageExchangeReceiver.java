/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: MessageExchangeReceiver.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common;

import javax.jbi.JBIException;

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
