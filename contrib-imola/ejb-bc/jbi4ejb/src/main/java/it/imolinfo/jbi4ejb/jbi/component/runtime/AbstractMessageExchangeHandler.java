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
 * AbstractMessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

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
 * @author Sun Microsystems, Inc.
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
