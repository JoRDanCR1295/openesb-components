/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.binding.email.protocol.receive;

import com.sun.jbi.common.qos.messaging.BaseExchangeTemplates;
import com.sun.jbi.common.qos.messaging.ExchangeTemplates;
import java.util.Map;
import java.util.Map.Entry;
import javax.activation.DataHandler;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

/**
 *
 * @author skini
 */
public class EmailBCExchangeTemplates extends BaseExchangeTemplates implements ExchangeTemplates {

    private Map<String, DataHandler> nmAttachments;

    public EmailBCExchangeTemplates(ServiceEndpoint endpoint,
            QName operation,
            boolean oneWay,
            String groupId,
            String msgId,
            Source input,
            MessageExchangeFactory exchangeFactory) {
        super(endpoint, operation, oneWay, groupId, msgId, input, exchangeFactory);
    }

    @Override
    public MessageExchange createExchange() throws MessagingException {
        MessageExchange mex = super.createExchange();
        NormalizedMessage inMessage = ((InOnly) mex).getInMessage();
        for (Entry<String, DataHandler> entry : nmAttachments.entrySet()) {
            inMessage.addAttachment(entry.getKey(), entry.getValue());
        }
        return mex;
    }

    public void setNMAttachments(Map<String, DataHandler> nmAttachments) {
        this.nmAttachments = nmAttachments;
    }

    public Map<String, DataHandler> getNMAttachments() {
        return nmAttachments;
    }
}
