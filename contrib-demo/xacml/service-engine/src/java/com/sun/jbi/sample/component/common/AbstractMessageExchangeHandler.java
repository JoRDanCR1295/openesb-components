/*
 * AbstractMessageExchangeHandler.java
 *
 */

package com.sun.jbi.sample.component.common;

import java.io.StringReader;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;

/**
 * This class is an abstract implementation of the MessageExchangeHandler which
 * provides the base implementation of the ME processing and provides hooks to
 * extended classes to implement component specific processing.
 *
 * @author chikkala
 */
public abstract class AbstractMessageExchangeHandler implements MessageExchangeHandler {
    
    public static String IN_MESSAGE = "in";
    public static String OUT_MESSAGE = "out";
    
    private MessageExchange mMessageExchange;
    private ExchangeStatus mStatus;
    /** Creates a new instance of AbstractMessageExchangeHandler */
    protected AbstractMessageExchangeHandler() {
        this.mMessageExchange = null;
    }
    
    protected abstract Logger getLogger();
    protected abstract DeliveryChannel getDeliveryChannel();
    protected abstract void validateMessageExchange() throws MessagingException;
    protected abstract void processError(Exception ex);
    protected abstract void processDone();
    protected abstract void processMessage();
    protected abstract void processFault(Fault fault);
    
    public final MessageExchange getMessageExchange() {
        return this.mMessageExchange;
    }
    
    public final void setMessageExchange(MessageExchange msgExchange) {
        this.mMessageExchange = msgExchange;
    }
    
    public final ExchangeStatus getMessageExchangeStatus() {
        if ( this.mStatus != null ) {
            return this.mStatus;
        } else if ( this.mMessageExchange != null ) {
            return this.mMessageExchange.getStatus();
        } else {
            return null;
        }
    }
    
    public final void setMessageExchangeStatus(ExchangeStatus status) {
        this.mStatus = status;
    }
    
    protected void send() throws MessagingException {
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected boolean sendSync(long timeout) throws MessagingException {
        return this.getDeliveryChannel().sendSync(this.mMessageExchange, timeout);
    }
    
    protected void sendDone() throws MessagingException {
        this.mMessageExchange.setStatus(ExchangeStatus.DONE);
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected void sendError(Exception ex) throws MessagingException {
        this.mMessageExchange.setError(ex);
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected void sendFault(Exception ex, QName type, String name) throws MessagingException {
        Fault fault = this.mMessageExchange.createFault();
        if ( ex != null ) {
            String xmlText = RuntimeHelper.getExceptionAsXmlText(ex);
            fault.setContent(RuntimeHelper.createDOMSource(new StringReader(xmlText)));
        }
        this.mMessageExchange.setFault(fault);
        this.getDeliveryChannel().send(this.mMessageExchange);
    }
    
    protected void processActive() {
        Fault fault = this.getMessageExchange().getFault();
        if ( fault != null ) {
            processFault(fault);
        } else {
            processMessage();
        }
    }
    /**
     * implementation of the MessageExchangeHandler#processMessageExchange method.
     */
    @Override
    public void processMessageExchange(ExchangeStatus status, MessageExchange msgEx) {
        
        getLogger().fine("MessageExchangeHandler.processMessageExchange:status: " + status );
        
        this.setMessageExchangeStatus(status);
        this.setMessageExchange(msgEx);        
        
        try {
            validateMessageExchange();
        } catch (MessagingException ex) {
            getLogger().log(Level.FINE, "Invalid message exchange for processing ", ex);
            if ( this.getMessageExchange() != null ) {
                try {
                    sendError(ex);
                } catch (MessagingException errEx) {
                    getLogger().log(Level.FINE, "Can not send invalid message exchange error", errEx);
                }
            }
            return;
        }
        
        MessageExchange msgExchange = this.getMessageExchange();
        
        if (ExchangeStatus.ACTIVE.equals(status) ) {
            processActive();
        } else if (ExchangeStatus.DONE.equals(status) ) {
            processDone();
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            processError(msgExchange.getError());
        }
    }
    
}
