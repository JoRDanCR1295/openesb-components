/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 * MockDeliveryChannelBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import java.util.Vector;


public class MockDeliveryChannelBasic implements MockDeliveryChannel {
    private MessageExchangeFactory messageExchangeFactory;
    private Vector sendMeList = new Vector();

    /**
     * Used for InOut MessageExchanges. Should be set
     * using the setOutMessage before your component calls any
     * send methods. This message is set in all the send methods.
     */
    private Vector outNmList = new Vector();

    public MessageExchangeFactory getMessageExchangeFactory() {
        return messageExchangeFactory;
    }

    public void setMessageExchangeFactory(
        MessageExchangeFactory messageExchangeFactory) {
        this.messageExchangeFactory = messageExchangeFactory;
    }

    public void close() throws MessagingException {
    }

    public MessageExchangeFactory createExchangeFactory() {
        return messageExchangeFactory;
    }

    public MessageExchangeFactory createExchangeFactory(QName qName) {
        return messageExchangeFactory;
    }

    public MessageExchangeFactory createExchangeFactoryForService(QName qName) {
        return messageExchangeFactory;
    }

    public MessageExchangeFactory createExchangeFactory(
        ServiceEndpoint serviceEndpoint) {
        return messageExchangeFactory;
    }

    /**
     * No mock support yet. See ServiceEngineExample.start() comments.
     * @return
     * @throws MessagingException
     */
    public MessageExchange accept() throws MessagingException {
        return null;
    }

    public MessageExchange accept(long l) throws MessagingException {
        return null;
    }

    /**
     * Upon calling the send method set the instance variable
     * this.sendMessageExchange to the MessageExchange that is passed in.
     * To retreive this value call getSendMessageExchange.
     * @param messageExchange The MessageExchange to send to the NMR
     * @throws MessagingException
     */
    public void send(MessageExchange messageExchange) throws MessagingException {
        if ((!outNmList.isEmpty()) && (!(messageExchange instanceof InOnly))) {
            messageExchange.setMessage((NormalizedMessage) outNmList.remove(0),
                "out");
        }

        sendMeList.addElement(messageExchange);
    }

    /**
     * Upon calling the send method set the instance variable
     * this.sendMessageExchange to the MessageExchange that is passed in.
     * To retreive this value call getSendMessageExchange.
     * @param messageExchange The MessageExchange to send to the NMR
     * @return True
     * @throws MessagingException
     */
    public boolean sendSync(MessageExchange messageExchange)
        throws MessagingException {
        sendSync(messageExchange, 0);

        return true;
    }

    /**
     * A mock implementation of the sendSync() method. The outbound
     * MessageExchange is preserved for later retrieval by unit tests.
     * The MessageExchange is saved in a vector to support multiple calls
     * to send() methods.
     * <p>
     * To retreive these MessageExchanges, unit tests should call
     * getSendMessageExchange() repeatedly to retrieve all saved messages.
     *
     * @param messageExchange  - MessageExchange to send to the NMR
     * @return True
     * @throws MessagingException
     */
    public boolean sendSync(MessageExchange messageExchange, long l)
        throws MessagingException {
        if ((!outNmList.isEmpty()) && (!(messageExchange instanceof InOnly))) {
            messageExchange.setMessage((NormalizedMessage) outNmList.remove(0),
                "out");
        }

        sendMeList.addElement(messageExchange);

        return true;
    }

    /**
     * Gets the next MessageExchange object. Should be called after the
     * component calls any send method. Also should contain the out Message
     * for InOut MEPs.

     * @return oldest
     */
    public MessageExchange getSendMessageExchange() {
        MessageExchange me;

        if (sendMeList.isEmpty()) {
            me = null;
        } else {
            me = (MessageExchange) sendMeList.remove(0);
        }

        return me;
    }

    /**
     * For InOut MessageExchanges set the Out Message.
     * Should be called before your component calls any of the
     * send methods.
     * @param outMessage
     */
    public void setOutMessage(NormalizedMessage outMessage) {
        this.outNmList.addElement(outMessage);
    }
}
