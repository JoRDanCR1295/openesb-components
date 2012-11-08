/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskProviderEndpoint.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.stortsystems.openesb.bc.asterisk.wsdlext.PortExt;
import com.stortsystems.openesb.bc.asterisk.wsdlext.WSDLExtHelper;
import com.sun.jbi.sample.component.common.MessageExchangeHandler;
import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.MessageExchangeListener;
import com.sun.jbi.sample.component.common.deployment.ProviderEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * This class extends the ProviderEndpoint to implement component specific endpoint configuration.
 * It implements the MessageExchangeListener to receive message exchange notifications from the
 * MessageExchangeSupport fired by MessageExchangeReceiver and process it using a message exchange
 * handler to show how an asynchronous processing of message exchange can be done.
 *
 * This endpoint uses the JMXBindingProviderProxy as a message exchange handler that can invoke
 * the external provider to send a message and receive the response in a InOut message exchange.
 *
 * @author chikkala
 */
public class AsteriskProviderEndpoint extends ProviderEndpoint implements MessageExchangeListener {
    
    /** Creates a new instance of XSLTProviderEndpoint */
    public AsteriskProviderEndpoint(SUDescriptor.Provides provides, Definition wsdlDef, ServiceUnit su) {
        super(provides, wsdlDef, su);
    }
    
    @Override
    public void doInit() throws JBIException {
        RuntimeHelper.getLogger().info("JMXBindingProviderEndpoint:doInit called");
    
    }
    
    @Override
    public MessageExchangeHandler createMessageExchangeHandler() {
        return null;
    }
    
    @Override
    protected MessageExchangeListener createMessageExchangeListener() {
        return this;
    }
    
    public void messageExchangeReceived(ExchangeStatus status, MessageExchange me) {
        try {
            RuntimeHelper.getLogger().fine("MXListener.MessageExchangeReceived: with Status: " + status);
            processMessageExchangeWithHandler(status, me);
            // processMessageExchange(me);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
