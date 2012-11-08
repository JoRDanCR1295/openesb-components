 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.component.runtime;

import java.io.StringReader;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * This class is an abstract implemenation of the MessageExchangeHandler which
 * provides the base implemenation of the ME processing and provides hooks to
 * extended classes to implement component specific processing.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public abstract class AbstractMessageExchangeHandler implements MessageExchangeHandler {
    
    public static String IN_MESSAGE = "in";
    public static String OUT_MESSAGE = "out";
    
    private MessageExchange mMessageExchange;
    
    /** Creates a new instance of AbstractMessageExchangeHandler */
    protected AbstractMessageExchangeHandler() {
        this.mMessageExchange = null;
    }
    
    protected final DeliveryChannel getDeliveryChannel() {
        return RuntimeHelper.getDeliveryChannel();
    }
    
    public final MessageExchange getMessageExchange() {
        return this.mMessageExchange;
    }
    
    public final void setMessageExchange(MessageExchange msgExchange) {
        this.mMessageExchange = msgExchange;
    }
    
    protected final void send() throws MessagingException {
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected final void sendDone() throws MessagingException {
        this.mMessageExchange.setStatus(ExchangeStatus.DONE);
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected final void sendFault() throws MessagingException {
        Fault fault = this.mMessageExchange.createFault();
        sendFault(fault);
    }
    
    protected final void sendFault(Exception ex) throws MessagingException {
        Fault fault = this.mMessageExchange.createFault();
        if ( ex != null ) {
            String xmlText = RuntimeHelper.getExceptionAsXmlText(ex);
            fault.setContent(RuntimeHelper.createDOMSource(new StringReader(xmlText)));
        }
        sendFault(fault);
    }
    
    protected void sendFault(Fault fault) throws MessagingException {
        this.mMessageExchange.setFault(fault);
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    
    protected final void sendError(Exception ex) {
        try {
            this.mMessageExchange.setError(ex);
            this.getDeliveryChannel().send(this.mMessageExchange);
        } catch (MessagingException msgEx) {
            msgEx.printStackTrace();
        }
    }
    
    protected abstract void validateMessageExchange() throws MessagingException;
    protected abstract void processError(Exception ex);
    protected abstract void processDone();
    protected abstract void processMessage();
    protected abstract void processFault(Fault fault);
    
    private void processActive() {
        Fault fault = this.getMessageExchange().getFault();
        if ( fault != null ) {
            processFault(fault);
        } else {
            processMessage();
        }
    }
    
    public final void processMessageExchange() {
        try {
            validateMessageExchange();
        } catch (MessagingException ex) {
            ex.printStackTrace();
            if ( this.getMessageExchange() != null ) {
                sendError(ex);
            }
            return;
        }
        
        MessageExchange msgExchange = this.getMessageExchange();
        ExchangeStatus status = msgExchange.getStatus();
        
        if (ExchangeStatus.ACTIVE.equals(status) ) {
            processActive();
        } else if (ExchangeStatus.DONE.equals(status) ) {
            processDone();
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            processError(msgExchange.getError());
        }
    }
    
    public final void run() {
        try {
            this.processMessageExchange();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
}
