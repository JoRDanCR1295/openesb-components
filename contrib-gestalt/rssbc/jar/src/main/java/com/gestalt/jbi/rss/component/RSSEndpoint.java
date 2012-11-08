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
import com.gestalt.jbi.component.manager.PooledEndpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.rss.component.embedded.EmbeddedServerController;
import com.gestalt.jbi.rss.component.rss.RSSConsumerPoller;
import com.gestalt.jbi.rss.component.rss.RSSManager;
import com.gestalt.jbi.rss.extensions.RSSAddress;
import com.gestalt.jbi.rss.extensions.RSSOperation;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;
import com.gestalt.jbi.rss.extensions.RSSOperationOutput;

import com.sun.grizzly.http.SelectorThread;

import org.w3c.dom.Document;

import java.net.URL;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


/**
 * Provides the endpoint information needed for the exchange of messages with
 * this service provider.
 */
public class RSSEndpoint extends PooledEndpoint {
    private static RSSManager rssManager = new RSSManager();
    private static SelectorThread selectorThread = new SelectorThread();
    private RSSAddress address;
    private RSSOperationInput rssOperationInput;
    private RSSConsumerHandler rssConsumerHandler;
    private RSSProviderHandler rssProviderHandler;
    private RSSConsumerPoller rssConsumerPoller;
    private Map<QName, RSSOperation> rssOperations;
    private Map<RSSOperation, RSSOperationInput> rssOperationInputs = new HashMap<RSSOperation, RSSOperationInput>();
    private Map<RSSOperation, RSSOperationOutput> rssOperationOutputs = new HashMap<RSSOperation, RSSOperationOutput>();

    public RSSEndpoint(QName qName1, QName qName2, String string,
        MessageExchange.Role role, Document doc, Definition def,
        ServiceUnit serviceUnit) {
        super(qName1, qName2, string, role, doc, def, serviceUnit);
    }

    /**
     * gets the rssOperations
     *
     * @return Map
     */
    public Map<QName, RSSOperation> getOperations() {
        return rssOperations;
    }

    /**
     * Creates a ConsumerExchangeHandler
     *
     * @return AbstractMessageExchangeHandler
     */
    public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
        rssConsumerHandler = new RSSConsumerHandler(this);

        return rssConsumerHandler;
    }

    /**
     * Creates a ProviderExchangeHandler and sets the RSSManager for the handler.
     *
     * @return AbstractMessageExchangeHandler
     */
    public AbstractMessageExchangeHandler createProviderExchangeHandler() {
        rssProviderHandler = new RSSProviderHandler(this);
        rssProviderHandler.setRSSManager(rssManager);

        return rssProviderHandler;
    }

    /**
     * Activates the endpoint.
     *
     * @throws Exception
     */
    @Override
    public void activate() throws Exception {
        super.activate();

        final QName operation = ((QName[]) getOperations().keySet()
                                               .toArray(new QName[] {  }))[0];
        RSSOperation rssOperation = getRSSOperations().get(operation);
        rssOperationInput = getRSSOperationInput(rssOperation);

        int pollingInterval = rssOperationInput.getPollingInterval();
        getProcessor();

        if (MessageExchange.Role.CONSUMER.equals(role)) {
            if ((address.getLocation() != null) &&
                    !address.getLocation().equals("")) {
                rssConsumerPoller = new RSSConsumerPoller(rssConsumerHandler,
                        rssOperationInput, address);
                rssManager.scheduleConsumerPoller(rssConsumerPoller,
                    pollingInterval);
            } else if ((address.getCorrelationId() != null) &&
                    !address.getCorrelationId().equals("")) {
                if (!rssManager.addSubscriber(address.getCorrelationId(),
                            MessageExchange.Role.CONSUMER)) {
                    throw new Exception(
                        "Subscriber ID already exists as a Consumer!");
                }

                rssManager.receiveNotifications(rssOperationInput, address,
                    rssConsumerHandler);
            }
        } else {
            try {
                RSSOperation.OperationName rssOperationName = rssOperation.getOperationName();

                if (rssOperationName.equals(RSSOperation.OperationName.publish)) {
                    log.fine("Starting embedded Server Controller......");
                    EmbeddedServerController.setSelectorThread(selectorThread);
                    EmbeddedServerController.setRssManager(rssManager);
                    EmbeddedServerController.startServer(new URL(
                            address.getLocation()).getPort());
                } else if (rssOperationName.equals(
                            RSSOperation.OperationName.subscribe)) {
                    if (!rssManager.addSubscriber(address.getCorrelationId(),
                                MessageExchange.Role.PROVIDER)) {
                        throw new Exception(
                            "Subscriber ID already exists as a Provider!");
                    }
                } else {
                    throw new Exception("Unsupported Operation");
                }
            } catch (Exception e) {
                log.severe("Error activating Provider: " + e);
                throw e;
            }
        }
    }

    /**
     * Deactivates the endpoint.
     *
     * @throws Exception
     */
    @Override
    public void deactivate() throws Exception {
        super.deactivate();

        if (address.getCorrelationId() != null) {
            rssManager.removeSubscriber(address.getCorrelationId());
        }

        if (MessageExchange.Role.CONSUMER.equals(role)) {
            rssManager.shutdown();
        } else {
            EmbeddedServerController.stopServer();
        }
    }

    /**
     * Sets the RSSOperationInput
     *
     * @param rssOperation
     * @param input
     */
    public void setRSSOperationInput(RSSOperation rssOperation,
        RSSOperationInput input) {
        rssOperationInputs.put(rssOperation, input);
    }

    /**
     * Sets the RSSOperationOutput
     *
     * @param rssOperation
     * @param output
     */
    public void setRSSOperationOutput(RSSOperation rssOperation,
        RSSOperationOutput output) {
        rssOperationOutputs.put(rssOperation, output);
    }

    /**
     * Sets the RSSAddress
     *
     * @param address
     */
    public void setRSSAddress(RSSAddress address) {
        this.address = address;
    }

    /**
     * Gets the RSSAddress
     *
     * @return RSSAddress
     */
    public RSSAddress getRSSAddress() {
        return address;
    }

    /**
     * Sets the RSSOperations
     *
     * @param operations
     */
    public void setRSSOperations(Map<QName, RSSOperation> operations) {
        this.rssOperations = operations;
    }

    /**
     * gets the RSSOperations
     *
     * @return Map<QName, RSSOperation>
     */
    public Map<QName, RSSOperation> getRSSOperations() {
        return rssOperations;
    }

    /**
     * Gets the RSSOperationInput
     *
     * @param rssOperation
     * @return RSSOperationInput
     */
    public RSSOperationInput getRSSOperationInput(RSSOperation rssOperation) {
        return rssOperationInputs.get(rssOperation);
    }

    /**
     * gets the RSSOperationOutput
     *
     * @param rssOperation
     * @return RSSOperationOutput
     */
    public RSSOperationOutput getRSSOperationOutput(RSSOperation rssOperation) {
        return rssOperationOutputs.get(rssOperation);
    }

    public void setFilterDate(Date newFilterDate) {
        //this.filterDate = newFilterDate;
    }

    /**
     * gets RSSOperationInputs
     *
     * @return Map<RSSOperation, RSSOperationInput>
     */
    public Map<RSSOperation, RSSOperationInput> getRSSOperationInputs() {
        return rssOperationInputs;
    }

    /**
     * sets RSSOperationInputs
     *
     * @param rssOperationInputs
     */
    public void setRSSOperationInputs(
        Map<RSSOperation, RSSOperationInput> rssOperationInputs) {
        this.rssOperationInputs = rssOperationInputs;
    }
}
