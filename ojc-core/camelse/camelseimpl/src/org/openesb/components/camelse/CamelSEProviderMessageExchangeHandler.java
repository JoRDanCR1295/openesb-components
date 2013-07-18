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
 * @(#)CamelSEProviderMessageExchangeHandler.java 
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.openesb.components.camelse;

import java.util.ArrayList;
import java.util.List;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.transform.dom.DOMSource;
import org.openesb.components.camelse.common.AbstractMessageExchangeHandler;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.wsdl.WSDL11JBIWrapper;
import org.openesb.components.camelse.camel.JBIBridgeEndpoint;
import java.io.StringReader;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This class extends the AbstractMessageExchangeHandler to implement the component specific
 * message exchange processing on the provider side in a service engine.
 *
 * @author chikkala
 */
public class CamelSEProviderMessageExchangeHandler extends AbstractMessageExchangeHandler {
    private static final String XMLNS_NS = "http://www.w3.org/2000/xmlns/";
    private CamelSEProviderEndpoint mEndpoint;

    /** Creates a new instance of ProviderSEMXHandler */
    public CamelSEProviderMessageExchangeHandler(CamelSEProviderEndpoint endpoint) {
        this.mEndpoint = endpoint;
    }

    protected Logger getLogger() {
        return this.mEndpoint.getLogger();
    }

    protected DeliveryChannel getDeliveryChannel() {
        return this.mEndpoint.getDeliveryChannel();
    }

    protected void validateMessageExchange() throws MessagingException {
        MessageExchange msgExchange = this.getMessageExchange();

        if (this.getMessageExchange() == null) {
            throw new MessagingException("MessageExchange Object is null in MessageExchageHandler");
        }

        if (MessageExchange.Role.CONSUMER.equals(msgExchange.getRole())) {
            throw new MessagingException("Provider Message Exchange Handler can not have MessageExchange with CONSUMER Role");
        }

    }

    protected void processError(Exception ex) {
        MessageExchange msgExchange = this.getMessageExchange();
        Exception errEx = msgExchange.getError(); // get the error and print

        RuntimeHelper.getLogger().info(
                " Message Exchange Provider received Error: " + errEx.getMessage());
        msgExchange.getError().printStackTrace();
    }

    protected void processDone() {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.getLogger().info(" Message Exchange Provider received DONE :" +
                " END of service invocation");
    }

    protected void processFault(Fault fault) {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.logError(" Message Exchange Provider Handler can not receive Fault on Provider side");
    }

    protected void processMessage() {
        try {
            MessageExchange me = this.getMessageExchange();
            if (me instanceof InOnly) {
                processMessageOnProvider((InOnly) me);
            } else if (me instanceof InOut) {
                processMessageOnProvider((InOut) me);
            } else {
                throw new JBIException("Unsupported message exchage " + me);
            }
        // processInMessageOnProvider((InOut)this.getMessageExchange());
        // processInOnlyMessageOnProvider((InOnly)this.getMessageExchange());
        } catch (JBIException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * synchronous processing of the inonly message exchange.
     * @param inOnlyExchange
     * @throws javax.jbi.JBIException
     */
    protected void processMessageOnProvider(InOnly inOnlyExchange) throws JBIException {
        RuntimeHelper.getLogger().fine("Processing inonly message exchange" + inOnlyExchange);
        
        JBIBridgeEndpoint ep = this.mEndpoint.getCamelEndpoint(inOnlyExchange);
        if (ep == null) {
            throw new JBIException("Can not find Camel Endpoint for " + inOnlyExchange);
        }
        QName operationQName = inOnlyExchange.getOperation();
        Operation wsdlOperation = this.mEndpoint.getWSDLOperation(operationQName);
        JBIWrapperUtil.removeJBIWrapperFromInMessage(wsdlOperation, inOnlyExchange.getInMessage());
        ep.getConsumer().processJBIMessageExchange(inOnlyExchange);

        this.sendDone();
    }

    /**
     * synchronour processing of the inout message exchange.
     * @param inOutExchange
     * @throws javax.jbi.JBIException
     */
    protected void processMessageOnProvider(InOut inOutExchange) throws JBIException {
        RuntimeHelper.getLogger().fine("Processing In Message on Provider side " + inOutExchange);
        // invoke the service operation
        try {
            QName opName = inOutExchange.getOperation();
            QName svcName = inOutExchange.getEndpoint().getServiceName();
            // process 
            JBIBridgeEndpoint ep = this.mEndpoint.getCamelEndpoint(inOutExchange);
            if (ep == null) {
                throw new JBIException("Can not find Camel Endpoint for " + inOutExchange);
            }
            QName operationQName = inOutExchange.getOperation();
            Operation wsdlOperation = this.mEndpoint.getWSDLOperation(operationQName);
            JBIWrapperUtil.removeJBIWrapperFromInMessage(wsdlOperation, inOutExchange.getInMessage());
            WSDL11JBIWrapper outWrapper = WSDL11JBIWrapper.createOutputWrapper(wsdlOperation);
            
            ep.getConsumer().processJBIMessageExchange(inOutExchange, outWrapper);

        } catch (Exception ex) {
            // exception invoking the operation. so, set exception text as fault content.
            ex.printStackTrace();
            String faultText = RuntimeHelper.getExceptionAsXmlText(ex);
            Source faultContent = RuntimeHelper.createDOMSource(new StringReader(faultText));
            Fault fault = inOutExchange.createFault();
            inOutExchange.setFault(fault);
            fault.setContent(faultContent);  // may need to normalize the content.
        }
        // send out or fault message.
        // this.getDeliveryChannel().sendSync(inOutExchange, SEND_SYNC_TIMEOUT);
        this.send();
    }
    
}
