/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.NormalizedMessage;

import javax.xml.transform.Source;

/**
 * TODO Document this type.
 *
 * @author Prashanth B.R
 */
public class ScreenScrapingSEInOutConsumer extends AbstractInOutConsumer {

    /**
     *
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processFault(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processFault(CRLInOut inOut, ExchangeContext ctx)
            throws JBIException {
        // FILTER_REQUEST_REPLY:
        // A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C 
        //                         B <----------------- (4. Y.fault  ) <-- C
        //                         B -----------------> (5. Y.done  ) <-- C
        // A <--- (7. X.out) <---- B (6. Transform Y)
        // A ---> (8. X.done) ---> B
        CRLInOut originalInOut = null;
        ScreenScrapingseEndpoint input = null;
        ScreenScrapingseEndpoint output = null;

        try {
            originalInOut = (CRLInOut) ctx.getCorrelationMap().decorrelate(inOut.getExchangeId());

            input = (ScreenScrapingseEndpoint) ctx.getEndpointManager().lookupEndpoint(originalInOut.getEndpoint().getServiceName(),
                    originalInOut.getEndpoint().getEndpointName(), true);
            output = input.getInvoke(); // C

            // send DONE to consumed endpoint
            // INF 111811            output.getStatus().incrementReceivedReplies();
            inOut.setStatus(ExchangeStatus.DONE);
            inOut.send();
            // INF 111811            output.getStatus().incrementSentDones();

            // Script does not really support faults, so it'll be passed back
            originalInOut.setFault(inOut.getFault());
            originalInOut.setStatus(ExchangeStatus.DONE);
            originalInOut.send();

        // INF 111811            input.getStatus().incrementSentDones();
        } catch (Exception e) {
            log().log(Level.SEVERE,
                    "Failed to process fault: " + e.getMessage(), e);
            inOut.sendError(e);

            if (output != null) {
                // INF 111811                output.getStatus().incrementSentErrors();
            }

            if (originalInOut != null) {
                originalInOut.sendError(e);

            // INF 111811                if (input != null) {
            // INF 111811                    input.getStatus().incrementSentErrors();
            // INF 111811                }
            }
        }
    }

    /**
     *
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processOut(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processOut(CRLInOut inOut, ExchangeContext ctx)
            throws JBIException {
        // FILTER_REQUEST_REPLY:
        // A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C 
        //                         B <----------------- (4. Y.out  ) <-- C
        //                         B -----------------> (5. Y.done  ) <-- C
        // A <--- (7. X.out) <---- B (6. Transform Y)
        // A ---> (8. X.done) ---> B
        CRLInOut originalInOut = null;
        ScreenScrapingseEndpoint input = null;

        try {
            originalInOut = (CRLInOut) ctx.getCorrelationMap().decorrelate(inOut.getExchangeId());

            input = (ScreenScrapingseEndpoint) ctx.getEndpointManager().lookupEndpoint(originalInOut.getEndpoint().getServiceName(),
                    originalInOut.getEndpoint().getEndpointName(), true);

            ScreenScrapingseEndpoint output = input.getInvoke(); // C

            // propogate transaction regardless of status
            ScreenScrapingSEUtil.propogateTransaction(inOut, originalInOut);

            // 4. Receive Y.out from C
            NormalizedMessage yOut = inOut.getOutMessage();

            if (yOut == null) {
                inOut.sendError(new JBIException(
                        "Cannot process NULL out message!"));
                originalInOut.sendError(new JBIException(
                        "Received NULL response!"));

                return;
            }

            // INF 111811            output.getStatus().incrementReceivedReplies();
            // 5. Send Y.done or Y.error to C ==============================
            inOut.setStatus(ExchangeStatus.DONE);
            inOut.send();

            // INF 111811            output.getStatus().incrementSentDones();

            // 6. Transform Y ==============================================
            Source content = yOut.getContent();

            // 6. Transform Y ==============================================
            Source processedContent = null;

            try {
                if (log().isLoggable(Level.FINE)) {
                    log().fine("filterRequestReply-response: " +
                            ScreenScrapingSEUtil.print(content));
                }

                processedContent = ScreenScrapingSEUtil.transform(content,
                        input, ctx);

                if (log().isLoggable(Level.FINE)) {
                    log().fine("filterRequestReply-transformed response: " +
                            ScreenScrapingSEUtil.print(processedContent));
                }
            } catch (Exception transformEx) {
                log().severe("Transformation failed: " +
                        transformEx.getMessage());
                originalInOut.sendError(transformEx);

            // INF 111811                input.getStatus().incrementSentErrors();
            }

            // 7. Send X.out to A ==========================================
            originalInOut.reply(processedContent);

        // INF 111811            input.getStatus().incrementSentReplies();
        } catch (JBIException e) {
            log().log(Level.SEVERE, "Consuming InOut failed: " + e.getMessage(),
                    e);
            originalInOut.sendError(e);

        // INF 111811            if (input != null) {
        // INF 111811                input.getStatus().incrementSentErrors();
        // INF 111811            }
        }
    }
}
