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
 * @(#)InboundReceiver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.Endpoint.EndpointType;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.Mailbox;
import com.sun.jbi.smtpbc.extservice.server.AddressBook;
import com.sun.jbi.smtpbc.extservice.server.EmailCallback;
import com.sun.jbi.smtpbc.extservice.server.EmailListener;
import com.sun.jbi.smtpbc.extservice.server.EmailServer;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class InboundReceiver implements EndpointChangeListener {
    private static final Messages mMessages =
        Messages.getMessages(InboundReceiver.class);
    private static final Logger mLogger =
        Messages.getLogger(InboundReceiver.class);

    private final EmailServer mServer;
    private final ComponentContext mContext;
    private final DeliveryChannel mChannel;
    private final MessageStore mMessageStore;

    public InboundReceiver(final ComponentContext context,
                           final DeliveryChannel channel,
                           final MessageStore messageStore) {
        mContext = context;
        mChannel = channel;
        mMessageStore = messageStore;
        mServer = new EmailServer();
    }
    
    public void stopReceiving() throws JBIException {
        try {
            mServer.stopAllServices();
        } catch (final Exception ex) {
             InboundReceiver.mLogger.log(Level.SEVERE, "IR_Stop_Receiving_Failed");
             throw new JBIException(InboundReceiver.mMessages.getString("IR_Stop_Receiving_Failed"), ex);
        }
    }

    public void endpointInitialized(final Endpoint endpoint) {}

    public void endpointActivated(final Endpoint endpoint) throws EndpointChangeException {
        String name = null;
        int port = 0;
        try {
            if (endpoint.getEndpointType().equals(EndpointType.INBOUND)) {
               name = endpoint.getServiceName().toString() +
                    endpoint.getEndpointName();
               //port = parsePort(endpoint.getSMTPAddress().getLocation());
               port = endpoint.getSMTPAddress().getSMTPPort();
                final AddressBook addressBook = createAddressBook(endpoint);
                final EmailListener listener =
                    new InboundMessageProcessor(addressBook, endpoint);
                mServer.startEmailService(listener, name, port);
            }
        } catch (final Throwable ex) {                   
            InboundReceiver.mLogger.log(Level.SEVERE, 
                        "IR_Start_Email_Service_Failed",
                        new Object[] {name, port});
            throw new EndpointChangeException(
                        InboundReceiver.mMessages.getString("IR_Start_Email_Service_Failed",
                                            new Object[] {name, port}));
        }
    }

    public void endpointDeactivated(final Endpoint endpoint) {
        if (endpoint.getEndpointType().equals(EndpointType.INBOUND)) {
            final String name = endpoint.getServiceName().toString() +
                endpoint.getEndpointName();
            final int port = endpoint.getSMTPAddress().getSMTPPort();
            mServer.stopEmailService(name, port);
        }
    }

    public void endpointShutdown(final Endpoint endpoint) {}

    private AddressBook createAddressBook(final Endpoint endpoint) {
        final AddressBook book = new AddressBook();
        final MailTo mailTo = endpoint.getSMTPAddress().getLocation();
        final Iterator mailboxes = mailTo.getMailbox().iterator();
        while (mailboxes.hasNext()) {
            final Mailbox box = (Mailbox)mailboxes.next();
            book.addEmailAddress(box.getNormalizedAddressSpec());
        }
        return book;
    }

        
    public class InboundMessageProcessor implements EmailListener {
        
        private Endpoint mEndpoint;
        private AddressBook mAddressbook;
 
        public InboundMessageProcessor(final AddressBook addressbook,
                                       final Endpoint endpoint) {
            mAddressbook = addressbook;
            mEndpoint = endpoint;
        }

        public AddressBook getAddressBook() {
            return mAddressbook;
        }
        
        public void setAddressBook(final AddressBook addressbook) {
            mAddressbook = addressbook;
        }

        public void onMessage(final byte[] message, final EmailCallback callback) 
            throws Exception {

            // Create our response message
            final MessageExchangeFactory mef = mChannel.createExchangeFactory();
            final InOnly inOnlyExchange = mef.createInOnlyExchange();
            inOnlyExchange.setEndpoint(lookupServiceEndpoint());
            final QName operationName = lookupOperation();
            inOnlyExchange.setOperation(operationName);
            final NormalizedMessage inMessage =
                new SMTPNormalizer().normalize(message,
                                               inOnlyExchange,
                                               operationName,
                                               mEndpoint);
            
            inOnlyExchange.setInMessage(inMessage);

            // Store the message information for response later to our
            // External Service
            final String inOnlyExchangeId = inOnlyExchange.getExchangeId();
            mMessageStore.put(inOnlyExchangeId, callback);

            mChannel.send(inOnlyExchange);
            mEndpoint.getEndpointStatus().incrementSentRequests();
        }

        private ServiceEndpoint lookupServiceEndpoint() {
            //ServiceEndpoint activatedEndpoint = null;

            final QName fullServiceName = mEndpoint.getServiceName();
            final String endpointName = mEndpoint.getEndpointName();
            return mContext.getEndpoint(fullServiceName, endpointName);
            /*
            int count = 0;
            while (activatedEndpoint == null  && count < 10) {
                
                mLogger.info("Trying to lookup service endpoint");
                ServiceEndpoint[] allServiceEndpoints =
                    mContext.getEndpointsForService(fullServiceName);
                if (allServiceEndpoints != null) {
                    for (int ii = 0; ii < allServiceEndpoints.length; ii++) {
                        if (allServiceEndpoints[ii].getEndpointName().equals(endpointName)) {
                            // Found our endpoint
                            activatedEndpoint = allServiceEndpoints[ii];
                        }
                    }
                }
                
                if (activatedEndpoint == null) {
                    try {
                        mLogger.info("activated endpoint is null...Sleeping");
                        Thread.sleep(500);
                    } catch (Exception ex) {
                    }
                }
                count++;
            }
            */
            //return activatedEndpoint;
        }

        private QName lookupOperation() {
            // Just select the first operation.  This is the same hack that File BC
            // is doing.
            final QName[] operationNames =
                (QName[])mEndpoint.getSMTPOperations().keySet().toArray(new QName[0]);
            return operationNames[0];
            
        }
    }
}
