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

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import org.apache.camel.Processor;
import org.apache.camel.impl.DefaultConsumer;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.wsdl.WSDL11JBIWrapper;

/**
 * 
 * @author chikkala
 */
public class JBIBridgeConsumer extends DefaultConsumer<JBIBridgeExchange> {

    JBIBridgeEndpoint jbiBridgeEndpoint;

    public JBIBridgeConsumer(JBIBridgeEndpoint endpoint, Processor processor) {
        super(endpoint, processor);
        this.jbiBridgeEndpoint = endpoint;

    }

    /**
     *  call this when the jbi message exchange is processed on jbi endpiont provider side.
     * @param jbiMsg
     */
    public void processJBIMessageExchange(InOnly inOnlyExchange) {
        try {
            JBIBridgeExchange exchange = jbiBridgeEndpoint.createExchange(inOnlyExchange);
            getProcessor().process(exchange);
        } catch (Throwable e) {
            RuntimeHelper.logError(e);
            handleException(e);
        }
    }

    /**
     *  call this when the jbi message exchange is processed on jbi endpiont provider side.
     * @param jbiMsg
     */
    public void processJBIMessageExchange(InOut inOutExchange, WSDL11JBIWrapper outWrapper) throws Exception {
        try {
            JBIBridgeExchange exchange = jbiBridgeEndpoint.createExchange(inOutExchange);
            getProcessor().process(exchange);
            if (exchange.isFailed()) {
                Throwable ex = exchange.getException();
                if (ex != null ) {
                    throw new Exception(ex);
                } else {
                    // fault
                    JBIBridgeMessage msg = (JBIBridgeMessage) exchange.getFault(false);
                    NormalizedMessage faultMsg = msg.getJBIMessage();
                    inOutExchange.setFault((Fault)faultMsg);
                }
            } else {
                JBIBridgeMessage msg = (JBIBridgeMessage) exchange.getOut(false);
                NormalizedMessage outMsg = msg.getJBIMessage();
                addContentToJBIWrapper(outWrapper, msg.getJBIMessage().getContent());
                outMsg.setContent(outWrapper.toDOMSource());
                inOutExchange.setOutMessage(outMsg);
            }
        } catch (Exception e) {
            handleException(e);
            throw e;
        }
    }

    private void addContentToJBIWrapper(WSDL11JBIWrapper contentWrapper, Source content) {
        // check if the output from the transformer is in a wrapped form
        WSDL11JBIWrapper wrapper = WSDL11JBIWrapper.sourceToWrapper(
                RuntimeHelper.sourceToDOMSource(content));
        if (wrapper != null) {
            contentWrapper.appendParts(wrapper.getParts());
        } else {
            contentWrapper.appendPart(RuntimeHelper.sourceToDOMSource(content));
        }
    }
}
