/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyProvider;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.NormalizedMessage;

import javax.xml.transform.Source;

/**
 * Provisions endpoints for <code>InOnly</code> exchanges.
 *
 * @author Prashanth B.R
 */
public class ScreenScrapingSEInOnlyProvider extends AbstractInOnlyProvider {

    private static Logger mLogger = Logger.getLogger(ScreenScrapingSEInOnlyProvider.class.getName());

    /**
     *
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOnly,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processIn(CRLInOnly inOnly, ExchangeContext ctx)
            throws JBIException {
        ScreenScrapingseEndpoint input = null;

        try {
            NormalizedMessage xIn = inOnly.getInMessage();

            if ((xIn == null) || (xIn.getContent() == null)) {
                // TODO add CRLFault data?
                inOnly.sendError(new JBIException(
                        "ScriptseInOnlyProvider cannot transform missing message!"));

                return;
            }

            input = (ScreenScrapingseEndpoint) ctx.getEndpointManager().lookupEndpoint(inOnly.getEndpoint().getServiceName(),
                    inOnly.getEndpoint().getEndpointName(), true);

            ScreenScrapingseEndpoint output = input.getInvoke();

            // 1. Receive inOnly X from A
            // INF 111811            input.getStatus().incrementReceivedRequests();

            // 2. Transform==================================
            Source processedContent = null;

            try {
                Source content = xIn.getContent();

                if (log().isLoggable(Level.FINE)) {
                    log().fine("filterOneWay-input: " +
                            ScreenScrapingSEUtil.print(content));
                }

                processedContent = ScreenScrapingSEUtil.transform(content,
                        input, ctx);

                if (log().isLoggable(Level.FINE)) {
                    log().fine("filterOneWay-output: " +
                            ScreenScrapingSEUtil.print(processedContent));
                }
            } catch (Exception ex) {
                mLogger.log(Level.SEVERE, ex.getMessage(), ex);
                inOnly.sendError(ex);

                // INF 111811                input.getStatus().incrementSentErrors();
                return;
            }

        //===================================================

        //            // 3. Send inOnly Y to C ===========================
        //            InvokableExchange newInOnly =
        //                    ctx.getExchangeFactory().createInvokableExchange(
        //                            Pattern.IN_ONLY, output.getInfo());
        //            ScreenScrapingSEUtil.propogateTransaction(inOnly, newInOnly);
        //            ctx.getCorrelationMap().correlate(newInOnly.getExchangeId(), inOnly);
        //            newInOnly.invoke(processedContent, output.getOperation());
        //            output.getStatus().incrementSentRequests();
        //========================================================
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE,
                    "An unexpected error occurred provisioning a one-way service: " +
                    ex.getMessage(), ex);
            inOnly.sendError(ex);

            if (input != null) {
                // INF 111811                input.getStatus().incrementSentErrors();
            }
        }
    }
}
