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
 * @(#)ScreenScrapingseExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scriptse.jbiadapter;

import java.sql.Timestamp;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;


import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.util.Util;

import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.PatternRoleStatus;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.jbi.engine.scriptsecore.process.ScriptInfoVO;

public class ScriptseExchangeHandler extends AbstractExchangeHandler {

    /** Constructs an {@link ExchangeHandler} for AleSE. */
    public ScriptseExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange mex) throws JBIException {
        String i18n = I18n.loc("SCPTSE-5005: handle Exchnage {0}", mex.getExchangeId());
        log().log(Level.INFO, i18n);

        try {
            PatternRoleStatus prs = PatternRoleStatus.valueOf(mex);
            PatternRole key = PatternRole.toKey(mex);
            switch (prs) {
                case IN_OUT_PROVIDER_ACTIVE: {
                     RedeliveryStatus status = Redelivery.getRedeliveryStatus(mex);
                     
                    provisionService(mex);
                    break;
                }
                case IN_OUT_CONSUMER_ACTIVE: {
                    if (mex.getFault() != null) {
                        handleFault(mex);
                    } else if (ExchangeStatus.ERROR.equals(mex.getStatus())) {
                        RedeliveryStatus status = Redelivery.getRedeliveryStatus(mex);
                        if (status != null) {
                            if (!status.hasFailed()) {
                                //resend(mex);
                                return;
                            }
                        }
                        // otherwise, relay error back to original consumer
                        relayError(mex);
                    } else {
                        //continuesl processing 
                        continueProcessing(mex);
                    }
                    break;
                }
                case IN_ONLY_PROVIDER_ACTIVE: {
                    provisionService(mex);

                    break;
                }
                default: {
                    //status back in mex just log in
                    String err = I18n.loc("SCPTSE-6002: {0} does not support {1} exchange pattern",
                            this.getClass().getSimpleName(),
                            String.valueOf(ExchangePattern.valueOf(mex)));
                    log().severe(err);
                }
            }

        } catch (Exception e) {
            i18n = I18n.loc("SCPTSE-6003: Failed to handle message exchange ({0}): {1} : {2}",
                    mex.getExchangeId(), e.getMessage(), e.getCause());
            log().log(Level.WARNING, i18n, e);
            // ExchangeUtil.setErrorData(mex, error, FaultCode.Server, detail, actor);
            // throw new JBIException(i18n, e);
            mex.setError(new JBIException(i18n, e));
            getContext().getMessagingChannel().send(mex);
        }
    }

    protected void provisionService(MessageExchange mex)
            throws JBIException {
        /**
         * get the srevice endpoint from message exchange, create 
         * endpointinfo based om mex's endpointname and servicename
         * from endpointmanager to look up an endpoint
         * 
         */
        ScriptseEndpoint endpt = (ScriptseEndpoint) findProvisioningEndpoint(mex);
        NormalizedMessage xIn = ((InOut) mex).getInMessage();
        if (xIn == null) {
            mex.setError(new JBIException("Cannot process NULL in message!"));

            return;
        }
        // 2. Transform X ==================================================
        Source processedContent = null;
        Source content = xIn.getContent();

        processedContent = ScriptseExecutorFactory.transform(content, endpt);
        NormalizedMessage xOut = mex.createMessage();
        xOut.setContent(processedContent);
        ((InOut) mex).setOutMessage(xOut);

        getContext().getMessagingChannel().send(mex);
    }

    /**
     * Logs a status message describing the status of the specified message exchange.
     * @param msg The message exchange to log.
     * @return <code>true</code> if the status of the exchange is ACTIVE or DONE.
     */
    protected boolean logStatus(MessageExchange msg) {
        if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
            logError(msg);
            return false;
        } else if (log().isLoggable(Level.FINER)) {	// DONE
            log().finer(I18n.loc(
                    "SCPTSE-6008: Exchange({0}) is complete with DONE status.",
                    msg.getExchangeId()));
        }
        return true;
    }

    protected void logError(MessageExchange msg) {
        ServiceEndpoint endpt = msg.getEndpoint();
        log().warning(I18n.loc(
                "SCPTSE-6007: Exchange({0}) terminated with ERROR status by {1}.\n" +
                "Service: {2}, Endpoint: {3}, Operation: {4}\n" +
                "Cause: {5} - {6}\nActor: {7}, Detail: {8}",
                msg.getExchangeId(), getContext().getComponentContext().getComponentName(),
                endpt.getServiceName(), endpt.getEndpointName(),
                msg.getOperation().getLocalPart(),
                msg.getProperty(ExchangeUtil.FAULTCODE_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTSTRING_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTDETAIL_PROPERTY_NAME)));
    }

    /**
     * Handles a returned {@link Fault} from consumed {@link InOut} service.
     * @param msg The reply message containing a <code>Fault</code>.
     * @throws Exception If an error occurs identifying fault or replying.
     */
    protected void handleFault(MessageExchange msg) throws Exception {
        // lookup process awaiting InOut response, decorrelate
        logError(msg);
    }

    /**
     * Forwards a received {@link ExchangeStatus#ERROR} to the consumer of this service.
     * 
     * @param msg The received message with <code>ERROR</code> status.
     * @param ctx The listener's context.
     * @throws JBIException If an error occurs forwarding the error.
     */
    protected void relayError(MessageExchange msg)
            throws JBIException {
        logError(msg);
    // InOut:Consumer - lookup process awaiting response, decorrelate

    // propagate error details and send

    // from systemics wiki: "Even when component relays the failure it should set its name."

    }

    /**
     * Continues executing a transformation process after receiving a reply.
     * @param msg The reply message.
     * @param ctx The enclosing context.
     * @throws JBIException if an error occurs completing process.
     */
    protected void continueProcessing(MessageExchange msg)
            throws JBIException {
        // lookup process awaiting response, decorrelate

        String err = I18n.loc(
                "SCPTSE-6041 Not implemented yet {1}", msg.getExchangeId());
        log().severe(err);

    }
}
