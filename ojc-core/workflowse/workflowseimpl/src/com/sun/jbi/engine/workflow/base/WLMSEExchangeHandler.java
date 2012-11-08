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
 * @(#)$Id: WLMSEExchangeHandler.java,v 1.1 2010/02/15 19:25:09 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.jbi.engine.workflow.util.I18n;

public class WLMSEExchangeHandler extends AbstractExchangeHandler {

    private WLMSEInOutProvider mInOutProvider;

    private WLMSEInOnlyConsumer mInOnlyConsumer;

    protected WLMSEExchangeHandler(ComponentContext ctx) {
        super(ctx);
        // TODO Auto-generated constructor stub
    }

    public void handleExchange(MessageExchange mex) throws JBIException {
        // TODO Auto-generated method stub
        try {
            ExchangePattern key = ExchangePattern.valueOf(mex);
            switch (key) {
            case IN_OUT: {
                if (ExchangeStatus.ACTIVE.equals(mex.getStatus())) {
                    mInOutProvider.processIn((InOut) mex);
                } else {
                    logStatus(log(), getContext().getComponentContext(), mex);
                    // remove process, ERROR or DONE
                    getContext().getCorrelationMap()
                            .remove(mex.getExchangeId());
                }
                break;
            }
            case IN_ONLY: {
                mInOnlyConsumer.processStatus((InOnly) mex);
                break;
            }
            default: { // unsupported pattern
                String err = I18n.loc(
                        "WLM-7100: {0} does not support {1} exchange pattern",
                        this.getClass().getSimpleName(), String
                                .valueOf(ExchangePattern.valueOf(mex)));
                log().severe(err);
                if (ExchangeStatus.ACTIVE.equals(mex.getStatus())) {
                    ExchangeUtil.setErrorData(mex, err, FaultCode.Client, null,
                            getComponentName());
                    sendError(mex, new Exception(err));
                } else if (ExchangeStatus.ERROR.equals(mex.getStatus())) {
                    logError(log(), getContext().getComponentContext(), mex);
                }
            }
            }
        } catch (Exception e) {
            // processing exceptions are handled, but anything here means we
            // can't send
            // log and throw
            throw error(
                    I18n
                            .loc(
                                    "TRANSL-7002: Failed to complete exchange({0}) for endpoint({1}-{2}): {3}",
                                    mex.getExchangeId(), mex.getEndpoint()
                                            .getServiceName(), mex
                                            .getEndpoint().getEndpointName(), e
                                            .getMessage()), e);
        }
    }

    public void setInOutProvider(WLMSEInOutProvider provider) {
        mInOutProvider = provider;
    }

    public void setInOnlyConsumer(WLMSEInOnlyConsumer consumer) {
        mInOnlyConsumer = consumer;
    }

    public static void logError(Logger log, ComponentContext ctx,
            MessageExchange msg) {
        ServiceEndpoint endpt = msg.getEndpoint();
        log.warning(I18n.loc(
                "WLM-6200: Exchange({0}) terminated with ERROR status by {1}.\n"
                        + "Service: {2}, Endpoint: {3}, Operation: {4}\n"
                        + "Cause: {5} - {6}\nActor: {7}, Detail: {8}", msg
                        .getExchangeId(), ctx.getComponentName(), endpt
                        .getServiceName(), endpt.getEndpointName(), msg
                        .getOperation().getLocalPart(), msg
                        .getProperty(ExchangeUtil.FAULTCODE_PROPERTY_NAME), msg
                        .getProperty(ExchangeUtil.FAULTSTRING_PROPERTY_NAME),
                msg.getProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME), msg
                        .getProperty(ExchangeUtil.FAULTDETAIL_PROPERTY_NAME)));
    }

    /**
     * Logs a status message describing the status of the specified message
     * exchange.
     * 
     * @param msg
     *            The message exchange to log.
     * @return <code>true</code> if the status of the exchange is ACTIVE or
     *         DONE.
     */
    public static boolean logStatus(Logger log, ComponentContext ctx,
            MessageExchange msg) {
        if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
            logError(log, ctx, msg);
            return false;
        } else if (log.isLoggable(Level.FINER)) { // DONE
            log.finer(I18n.loc(
                    "TRANSL-2006: Exchange({0}) is complete with DONE status.",
                    msg.getExchangeId()));
        }
        return true;
    }

}
