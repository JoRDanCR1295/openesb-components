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
 * @(#)EmailBCExchangeHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email;

import com.sun.jbi.binding.email.protocol.receive.EmailMessageContext;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.binding.email.protocol.send.smtp.SMTPEndpoint;
import com.sun.jbi.binding.email.protocol.send.smtp.SMTPMessageSender;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.jbi.component.toolkit.util.PatternRoleStatus;

/**
 * Handles exchanges for Email Binding Component.
 * @author CDK
 */
public class EmailBCExchangeHandler extends AbstractExchangeHandler {

    /** Constructs an {@link ExchangeInitiator} for Email Binding Component. */
    public EmailBCExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange msg) throws JBIException {
        try {
            /* 
             * An enum to help direct the message exchange handling,
             * PatternRoleStatus clearly identifies the state of the exchange.
             * Please refer to Section 5.4.2 of the JBI spec for more details.
             */
            PatternRoleStatus prs = PatternRoleStatus.valueOf(msg);
            I18n.finer(log(), "EMAILBC-2048: Received MEx, Exchange id={0}, role={1}, status={2}",
                    msg.getExchangeId(), msg.getRole().toString(),
                    msg.getStatus().toString());
            //log().info("EMAILBC-9999: PatternRoleStatus :" +  prs);
//            log().fine(MessageFormat.format("EMAILBC-9999: Handling received Message "//NOI18N
//                    + "Exchange id={0}, role={1}, status={2}",          //NOI18N
//                    msg.getExchangeId(), msg.getRole().toString(),
//                    msg.getStatus().toString()));
            switch (prs) {
                case IN_OUT_PROVIDER_ACTIVE: {
                    // TODO (required) Handle incoming InOut request
                    break;
                }
                case IN_OUT_PROVIDER_DONE:
                case IN_OUT_PROVIDER_ERROR: {
                    /*
                     * Exchange is complete; perhaps log the ERROR status...
                     */
                    // TODO (optional) Handle ERROR response
                    break;
                }
                /*
                 * This case only applies to components which send invocations
                 * through the NMR, where component acts as a service consumer
                 */
                case IN_OUT_CONSUMER_ACTIVE: {
                    if (msg.getFault() != null) {
                        // TODO Handle fault; remove this block if component doesn't handle faults
                    } else {  // ACTIVE, should never be DONE
                        // send ACK to consumer to complete InOut exchange
                        msg.setStatus(ExchangeStatus.DONE);
                        send(msg);

                        // TODO (required) Continue processing after invoke completes
                    }
                    break;
                }
                case IN_OUT_CONSUMER_ERROR: {
                    /*
                     * This implements basic Redelivery for synchronous InOut
                     * exchanges, meaning ACK is not sent until exchange completes
                     */
                    // TODO (optional) Enhance or remove as needed
                    RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);
                    if (status != null) {
                        if (!status.hasFailed()) {
                            // TODO (required) Create new exchange and resend
                            return;
                        }
                    }
                    /*
                     * Otherwise, relay error back to original consumer.
                     * Do NOT send THIS exchange as it's terminated by ERROR status
                     * Relay ERROR status to consumer of service this component provisions
                     */
                    // TODO (required) Relay ERROR back to consumer of provisioned service
                    break;
                }
                case IN_ONLY_PROVIDER_ACTIVE: { // never DONE or ERROR
                    // TODO (required) Handle incoming InOnly request
                    // TODO (required) Send status response: DONE or ERROR
                    try {
                        SMTPEndpoint endpoint = (SMTPEndpoint) findProvisioningEndpoint(msg);
                        NormalizedMessage normMsg = ((InOnly) msg).getInMessage();
                        SMTPMessageSender sender = new SMTPMessageSender(msg.getOperation(), endpoint, normMsg);
                        sender.sendMail();
                        msg.setStatus(ExchangeStatus.DONE);
                        this.send(msg);
                    } catch (Exception e) {
                        log().log(Level.SEVERE, e.getMessage(), e);
                        msg.setStatus(ExchangeStatus.ERROR);
                        this.sendError(msg, e);
                    }
                    break;
                }
                case IN_ONLY_CONSUMER_DONE: {
                    // always a status response, DONE or ERROR
                    // exchange is complete, do not respond
                    // TODO (required) Handle InOnly response
                    String messageId = (String) msg.getProperty(ServiceQuality.MESSAGE_ID);
                    EmailMessageContext emailMessageContext =
                            (EmailMessageContext) getContext().getCorrelationMap().remove(messageId);
                    if (emailMessageContext != null) {
                        emailMessageContext.addDoneMessage(messageId);
                    }
                    RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);

                    if (status != null) {
                        if (status.hasFailed()) {
                            I18n.severe(log(), "EMAILBC-7028: Redelivery failed, DONE acknowledged: Message ID: {0}", status.getError(), messageId);
                            return;
                        }
                    }

                    break;
                }
                case IN_ONLY_CONSUMER_ERROR: {
                    /*
                     * This implements basic Redelivery for synchronous InOnly
                     * exchanges, meaning ACK is not sent until exchange completes
                     */
                    String messageId = (String) msg.getProperty(ServiceQuality.MESSAGE_ID);
                    EmailMessageContext emailMessageContext =
                            (EmailMessageContext) getContext().getCorrelationMap().remove(messageId);
                    if (emailMessageContext != null) {
                        emailMessageContext.addErrorMessage(messageId);
                    }
                    RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);

                    if (status != null) {
                        if (status.hasFailed()) {
                            I18n.severe(log(), "EMAILBC-7029: Redelivery failed, ERROR acknowledged: Message ID: {0}", status.getError(), messageId);
                            return;
                        }
                    }

                    // TODO (required) Handle InOnly response with ERROR status

                    break;
                }
                default: {  // unsupported pattern
                    String err = I18n.loc(
                            "EMAILBC-7002: {0} does not support {1} exchange pattern",
                            this.getClass().getSimpleName(),
                            String.valueOf(ExchangePattern.valueOf(msg)));
                    log().severe(err);
                    if (ExchangeStatus.ACTIVE.equals(msg.getStatus())) {
                        ExchangeUtil.setErrorData(msg, err, FaultCode.Client, null, getComponentName());
                        sendError(msg, new Exception(err));
                    } else if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
                        // TODO (optional) Log termination of exchange
                    }
                }
            }
        } catch (Exception e) {
            // processing exceptions are handled, but anything here means we can't send
            // log and throw
            throw error(I18n.loc("EMAILBC-7003: Failed to complete exchange({0}) for endpoint({1}-{2}): {3}",
                    msg.getExchangeId(), msg.getEndpoint().getServiceName(),
                    msg.getEndpoint().getEndpointName(), e.getMessage()),
                    e);
        }
    }
}
