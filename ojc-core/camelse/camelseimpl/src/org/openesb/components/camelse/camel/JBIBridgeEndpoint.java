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
package org.openesb.components.camelse.camel;

import java.util.logging.Logger;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import org.apache.camel.Consumer;
import org.apache.camel.ExchangePattern;
import org.apache.camel.Processor;
import org.apache.camel.Producer;
import org.apache.camel.impl.DefaultEndpoint;
import org.openesb.components.camelse.CamelSEConsumerEndpoint;

/**
 * A camel endpoint implementation to invoke services provided in jbi components
 * or provide a jbi provider endpoint in camel to allow jbi components to invoke
 * the camel app as a provider.
 * @author chikkala
 */
public class JBIBridgeEndpoint extends DefaultEndpoint {

    private static final Logger LOG = Logger.getLogger(JBIBridgeEndpoint.class.getName());
    private JBIBridgeConsumer consumer;
    private JBIBridgeProducer producer;
    private CamelSEConsumerEndpoint jbiConsumerEP;
    private String jbiEndpointURL; // "<service-namespace>/<servicename>/<endpointname>"
    private String operation;
            
    protected JBIBridgeEndpoint(String endpointUri, String remaining, JBIBridgeComponent component) {
        super(endpointUri, component);
        this.jbiEndpointURL = remaining;
    }

    public String getJbiEndpointURL() {
        return jbiEndpointURL;
    }

    public String getOperation() {
        return operation;
    }

    public void setOperation(String operation) {
//        LOG.info("Setting endpoint operation " + operation);
        this.operation = operation;
    }
        
    public CamelSEConsumerEndpoint getJbiConsumerEP() {
        return jbiConsumerEP;
    }

    public void setJbiConsumerEP(CamelSEConsumerEndpoint jbiConsumerEP) {
        this.jbiConsumerEP = jbiConsumerEP;
    }
    
    /**
     * @return a Producer
     * @throws Exception
     * @see org.apache.camel.Endpoint#createProducer()
     */
    public Producer createProducer() throws Exception {
        LOG.fine("Creating Camel 2 JBI JBIBridgeProducer");
        JBIBridgeProducer result = new JBIBridgeProducer(this);
        this.producer = result;
        // do necessary camel 2 jbi initialization.
        return result;
    }

    /**
     * @param proc
     * @return a Consumer
     * @throws Exception
     * @see org.apache.camel.Endpoint#createConsumer(org.apache.camel.Processor)
     */
    public Consumer createConsumer(Processor proc) throws Exception {
        LOG.fine("Creating JBI 2 Camel JBIBridgeConsumer");
        JBIBridgeConsumer result = new JBIBridgeConsumer(this, proc);
        consumer = result;
        // do necessary jbi 2 camel initialization.
        return result;
    }

    public JBIBridgeConsumer getConsumer() {
        return consumer;
    }

    public JBIBridgeProducer getProducer() {
        return producer;
    }

    public boolean isSingleton() {
        return true;
    }

    public JBIBridgeExchange createExchange(InOnly jbiExchange) {
        return new JBIBridgeExchange(getCamelContext(), jbiExchange);
    }

    public JBIBridgeExchange createExchange(InOut jbiExchange) {
        return new JBIBridgeExchange(getCamelContext(), jbiExchange);
    }

    @Override
    public JBIBridgeExchange createExchange() {
        return new JBIBridgeExchange(getCamelContext(), getExchangePattern());
    }

    @Override
    public JBIBridgeExchange createExchange(ExchangePattern pattern) {
        return new JBIBridgeExchange(getCamelContext(), pattern);
    }
}
