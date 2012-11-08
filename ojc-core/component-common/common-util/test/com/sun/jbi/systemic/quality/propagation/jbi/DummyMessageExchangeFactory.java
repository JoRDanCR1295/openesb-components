/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.jbi;

import java.net.URI;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.RobustInOnly;
import javax.xml.namespace.QName;

/**
 *
 * @author radval
 */
public class DummyMessageExchangeFactory implements MessageExchangeFactory {

    public MessageExchange createExchange(QName arg0, QName arg1) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public MessageExchange createExchange(URI arg0) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public InOnly createInOnlyExchange() throws MessagingException {
        return new DummyInOnly();
    }

    public InOptionalOut createInOptionalOutExchange() throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public InOut createInOutExchange() throws MessagingException {
        return new DummyInOut();
    }

    public RobustInOnly createRobustInOnlyExchange() throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
