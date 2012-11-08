/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
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
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
/*
 * CamelSEProviderEndpoint.java
 */

package org.openesb.components.camelse;

import org.openesb.components.camelse.camel.JBIBridgeEndpoint;
import org.openesb.components.camelse.common.MessageExchangeHandler;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.MessageExchangeListener;
import org.openesb.components.camelse.common.deployment.ProviderEndpoint;
import org.openesb.components.camelse.common.deployment.SUDescriptor;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Definition;

/**
 * This class extends the ProviderEndpoint to implement component specific endpoint configuration.
 * It implements the MessageExchangeListener to receive message exchange notifications from the 
 * MessageExchangeSupport fired by MessageExchangeReceiver and process it using a message exchange
 * handler.
 *
 * This endpoint is configured for a service provided by this component which can receive a xml 
 * document and apply xslt transformation and send the results back in a InOut message exchange
 * to complete a service invocation on this endpoint.
 *
 * @author chikkala
 */
public class CamelSEProviderEndpoint extends ProviderEndpoint implements MessageExchangeListener {
    
    /** Creates a new instance of XSLTProviderEndpoint */
    public CamelSEProviderEndpoint(SUDescriptor.Provides provides, Definition wsdlDef, CamelSEServiceUnit su) {
        super(provides, wsdlDef, su);
    }
    
    public JBIBridgeEndpoint getCamelEndpoint(MessageExchange me) {
        CamelSEServiceUnit su = (CamelSEServiceUnit)this.getServiceUnit();
        return su.getCamelEndpoint(me);
    }    
    
    @Override
    public void doInit() throws JBIException {
        RuntimeHelper.getLogger().info("ProviderSEEndpoint:doInit called");
        CamelSEServiceUnit su = (CamelSEServiceUnit) this.getServiceUnit();
        //TODO: do any service impl specific initiaization
    }
    /**
     * creates the message exchange handler that will be used to process the message exchange
     * received for this endpoint.
     */
    @Override
    public MessageExchangeHandler createMessageExchangeHandler() {
        return new CamelSEProviderMessageExchangeHandler(this);
    }
    /**
     * returns the implementation of the message exchange listener which the 
     * MessageExchangeSupport invokes when a message exchange is received from delivery
     * channel by MessageExchangeReceiver.
     */
    @Override
    protected MessageExchangeListener createMessageExchangeListener() {
        return this;
    }
    /**
     * implementation of the message exchange received method of the listener.
     */
    public void messageExchangeReceived(ExchangeStatus status, MessageExchange me) {
        try {
            RuntimeHelper.getLogger().fine("MXListener.MessageExchangeReceived: with Status: " + status);
             processMessageExchangeWithHandler(status, me);
            // processMessageExchange(me);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
