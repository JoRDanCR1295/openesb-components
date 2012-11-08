/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author radval
 */
public class TesterSEComponent implements Component {

    public ComponentLifeCycle getLifeCycle() {
        return new TesterSELifeCycle();
    }

    public ServiceUnitManager getServiceUnitManager() {
        return new TesterSEServiceUnitManager();
    }

    public Document getServiceDescription(ServiceEndpoint arg0) {
        return null;
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint arg0, MessageExchange arg1) {
        return true;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint arg0, MessageExchange arg1) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment arg0) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
