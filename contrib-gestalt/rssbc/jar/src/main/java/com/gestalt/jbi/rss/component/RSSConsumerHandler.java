/**
 *   rss-binding-component - RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.rss.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.lifecycle.AbstractComponentLifeCycle;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;

import com.sun.syndication.feed.synd.SyndEntry;

import org.w3c.dom.Document;

import java.util.List;
import java.util.ArrayList;
import java.util.logging.Level;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.wsdl.*;

import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;


/**
 * Handles the Normalization and the De-Normalization of the message exchange,
 * and performs the operation defined in the WSDL to either publish an RSS feed
 * or getExternalFeed to an RSS feed.
 */
public class RSSConsumerHandler extends AbstractMessageExchangeHandler {
    /**
     * Constructor
     *
     * @param endpoint
     */
    public RSSConsumerHandler(Endpoint endpoint) {
        super(endpoint);
    }

    protected void processError(Exception exception) {
    }

    protected void processDone() {
    }

    protected void processMessage() {
    }

    protected void processFault(Fault fault) {
    }

    /**
     * Processes the feedlist, depending on configuration
     * the feedlist may be split up before sending
     *
     * @param feedList
     */
    public void process(List<SyndEntry> feedList, RSSOperationInput rssOperationInput) {
        Integer entriesPerMessage = rssOperationInput.getEntriesPerMessage();
        log.log(Level.FINEST, "process with entriesPerMessage " + entriesPerMessage);
        if (entriesPerMessage == null || entriesPerMessage.intValue() <= 0) {
            // Send all entries in the feedlist as one JBI message
            send(feedList); 
        } else {
            int maxPerMessage = entriesPerMessage.intValue();
            // Send the desired number of entries as individual JBI messages
            List individualList = new ArrayList();
            for (SyndEntry entry : feedList) {
                individualList.add(entry);
                if (individualList.size() == maxPerMessage) {
                   send(individualList);
                   individualList.clear();
                }
            }
            if (individualList.size() > 0) {
                send(individualList);
            }
        }
    }

    /**
     * Send the particular feedlist
     *
     * @param feedList
     */        
    public void send(List<SyndEntry> feedList) {
        final QName fullServiceName = endpoint.getServiceName();
        final String endpointName = endpoint.getEndpointName();
        final ServiceEndpoint serviceEndpoint = ((AbstractComponentLifeCycle) endpoint
                                                 .getServiceUnit().getComponent()
                                                 .getLifeCycle()).getComponentContext()
                                                 .getEndpoint(fullServiceName,
                endpointName);

        final QName operation = ((QName[]) endpoint.getOperations().keySet()
                                                   .toArray(new QName[] {  }))[0];

        try {
            exchange = channel.createExchangeFactoryForService(serviceEndpoint.getServiceName())
                              .createInOnlyExchange();
        } catch (MessagingException e) {
            e.printStackTrace();
        }

        exchange.setEndpoint(serviceEndpoint);
        exchange.setOperation(operation);

        NormalizedMessage in = null;

        try {
            in = exchange.createMessage();
        } catch (MessagingException me) {
            log.log(Level.SEVERE,
                "Received MessagingException trying to create NormalizedMessage");
        }

        if (in != null) {
            log.log(Level.INFO, "Creating Message");

            QName type = null;

            try {
                Service service = endpoint.getWsdlDefinition()
                                          .getService(endpoint.getServiceName());
                Port port = service.getPort(QName.valueOf(
                            endpoint.getEndpointName()).getLocalPart());
                PortType portType = port.getBinding().getPortType();
                Operation op = (Operation) portType.getOperations().get(0);
                type = op.getInput().getMessage().getQName();
            } catch (Exception e) {
                e.printStackTrace();
            }

            Definition wsdl = endpoint.getWsdlDefinition();
            Message wsdlMessageDefinition = wsdl.getMessage(type);
            log.fine("WSDL Message: " +
                wsdlMessageDefinition.getOrderedParts(null));

            RSSNormalizer normalizer = new RSSNormalizer();
            Document jbiMessage = normalizer.createConsumerContent(type,
                    feedList);

            try {
                in.setContent(new DOMSource(jbiMessage));
            } catch (Exception e) {
                sendError(e);
            }

            try {
                InOnly inOnly = (InOnly) exchange;
                log.fine("InOnly Exchange" + inOnly);
                exchange.setMessage(in, "in");
                sendSync(exchange);
            } catch (Exception e) {
                log.log(Level.SEVERE,
                    "Received Exception trying to create NormalizedMessage", e);
            }
        }
    }
}
