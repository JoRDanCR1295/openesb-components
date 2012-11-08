/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.ftpbc.qos.messaging;

import com.sun.jbi.common.qos.messaging.BaseExchangeTemplates;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import javax.activation.DataHandler;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

/**
 * 
 * @author jfu
 */
public class MessageExchangeTemplatesWithAttachments extends BaseExchangeTemplates {
    private Map<String, DataHandler> mAttachments;
    public MessageExchangeTemplatesWithAttachments(
            ServiceEndpoint endpoint, 
            QName operation,
            boolean oneWay,
            String groupId,
            String msgId,
            Source input,
            Map attachments,
            MessageExchangeFactory exchangeFactory ){
        super(endpoint, operation, oneWay, groupId, msgId, input, exchangeFactory);
        mAttachments = attachments;
    }

    public MessageExchange createExchange() throws MessagingException {
        MessageExchange mex = super.createExchange();
        if ( mAttachments != null && mAttachments.size() > 0 ) {
            NormalizedMessage nmsg = null;
            if ( mex instanceof InOnly ) {
                nmsg = ((InOnly)mex).getInMessage();
            }
            else if ( mex instanceof InOut ) {
                nmsg = ((InOut)mex).getInMessage();
            }
            else {
                throw new IllegalArgumentException("Invalid MessageExchange instance, has to be InOnly or InOut...");
            }
            Set ks = mAttachments.keySet();
            Iterator it = ks.iterator();
            while ( it.hasNext() ) {
                String key = (String)it.next();
                nmsg.addAttachment(key, mAttachments.get(key));
            }
        }
        return mex;
    }
}
