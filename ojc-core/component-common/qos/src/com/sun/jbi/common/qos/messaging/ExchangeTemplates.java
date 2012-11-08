package com.sun.jbi.common.qos.messaging;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

public interface ExchangeTemplates {
    public MessageExchange createExchange() throws MessagingException;
    public String getUniqueId();
}
