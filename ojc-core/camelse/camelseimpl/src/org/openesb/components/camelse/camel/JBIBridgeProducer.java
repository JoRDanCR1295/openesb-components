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

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.apache.camel.Exchange;
import org.apache.camel.ExchangePattern;
import org.apache.camel.Message;
import org.apache.camel.impl.DefaultProducer;
import org.apache.camel.util.ExchangeHelper;
import org.openesb.components.camelse.CamelSEConsumerEndpoint;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.deployment.ConsumerEndpoint;

/**
 * 
 *
 * @author chikkala
 */
public class JBIBridgeProducer extends DefaultProducer {

    private static final Logger LOG = Logger.getLogger(JBIBridgeProducer.class.getName());
    private final JBIBridgeEndpoint endpoint;

    public JBIBridgeProducer(JBIBridgeEndpoint endpoint) {
        super(endpoint);
        this.endpoint = endpoint;
    }

    @Override
    public JBIBridgeEndpoint getEndpoint() {
        return (JBIBridgeEndpoint) super.getEndpoint();
    }

    /**
     * @param exchange
     * @see org.apache.camel.Processor#process(Exchange)
     */
    public void process(Exchange exchange) throws Exception {
//        RuntimeHelper.getLogger().fine("JBIBridgeProducer.process(Exchange)");
        JBIBridgeExchange jbiBridgeExchange = (JBIBridgeExchange)endpoint.createExchange(exchange);
        process(jbiBridgeExchange);
        ExchangeHelper.copyResults(exchange, jbiBridgeExchange);
    }
    /**
     * finds the suitable operation for the jbi message exchange.
     * @param jbiConsumerEP
     * @param exchange
     * @return
     * @throws java.lang.Exception
     */
    private QName getOperation(CamelSEConsumerEndpoint jbiConsumerEP, JBIBridgeExchange exchange) throws Exception {
        // 1. check the camel ep uri has operation specified. if yes. use it.
        // 2. if camel exchange specifies operation and the interface has 
        //    multiple operations. valdate and use the operation specified in exchange.
        // 3. else use the defualt operation in the interface.        

        String op = this.endpoint.getOperation();
        QName opQName = (op ==null) ? QName.valueOf("") : QName.valueOf(op.trim());
        String opNS = opQName.getNamespaceURI();
        String opName = opQName.getLocalPart();
        
        QName ifQName = jbiConsumerEP.getService().getInterface();
        String ifNS = ifQName.getNamespaceURI();
        if ( opName.length() > 0 ) {
            if ( opNS.length() == 0 ) {
                opNS = ifNS;
            } else if (!ifNS.equals(opNS)) {
                throw new Exception("invalid namesapce: namespace for camel endpoint uri operation and jbi endpoint operation are not matching " + opNS + "!=" + ifNS);
            }
            opQName = new QName(opNS, opName);            
        } else {
            //no op specified in camel jbi uri
            opQName = null;
            String exOp = (String) exchange.getProperty(JBIBridgeExchange.JBI_OPERATION_PROP);
            if ( exOp != null && exOp.trim().length() > 0 ) {
                List opList = jbiConsumerEP.getWSDL().getPortType(ifQName).getOperations();
                if ( opList.size() > 1 ) {
                    opQName = QName.valueOf(exOp.trim());
                    String exOpNS = opQName.getNamespaceURI();
                    if ( !ifNS.equals(exOpNS)) {
                        throw new Exception("invalid namesapce: namespace for camel exchange operation and jbi endpoint operation are not matching " + exOpNS + "!=" + ifNS);
                    }
                }
            }
        }
        return opQName;
    }
    
    public void process(JBIBridgeExchange exchange) throws Exception {
        RuntimeHelper.getLogger().fine("JBIBridgeProducer.process(JBIBridgeExchange)");
        // process exchange.       
        CamelSEConsumerEndpoint jbiConsumerEP = this.endpoint.getJbiConsumerEP();
        if (jbiConsumerEP == null) {
            throw new IllegalStateException("No JBI Consumer Endpoint bound to the Camel JBI Endpoint " + this.endpoint.getEndpointUri());
        }
        QName opQName = getOperation(jbiConsumerEP, exchange);    
//        if ( opQName == null ) {
//            throw new IllegalStateException("Cannot find the operation on jbi endpoint " + jbiConsumerEP.getID() + " for camel ep " + this.endpoint.getEndpointUri());
//        }
        JBIBridgeMessage msg = (JBIBridgeMessage) exchange.getIn();
        RuntimeHelper.getLogger().fine("##### JBIBridgeProducer.process processing IN message for operation " + opQName);
        
        Object obj = msg.getBody();
        DOMSource inContent = RuntimeHelper.sourceToDOMSource((Source) obj);
        if ( RuntimeHelper.getLogger().isLoggable(Level.FINE) ) {
            RuntimeHelper.getLogger().fine(RuntimeHelper.readFromDOMSource(inContent).toString());
        }
        Source outContent = null;
        try {
            outContent = jbiConsumerEP.initiateMessageExchange(opQName, inContent);
            if ( outContent != null ) {
                RuntimeHelper.getLogger().fine("##### JBIBridgeProducer.process recevied OUT message ####");
                outContent = RuntimeHelper.sourceToDOMSource(outContent);
                RuntimeHelper.getLogger().fine(RuntimeHelper.readFromSource(outContent).toString());
                // check if JBIBridgeExchange is of *Out pattern
                 if ( exchange.getPattern().equals(ExchangePattern.InOut)) {
                    RuntimeHelper.getLogger().fine("##### JBIBridgeProducer.process settng OUT message ####");
                    exchange.getOut().setBody(RuntimeHelper.sourceToDOMSource(outContent));
                 }
            }
        } catch (JBIException jbiEx) {
            // fault
            jbiEx.printStackTrace();
            exchange.setException(jbiEx);
        }
    }
}
