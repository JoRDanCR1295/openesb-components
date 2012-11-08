/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.zaz.jbi.engine.screenscrapingse.process;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.InOutProvider;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint;
import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint.EntryType;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.NormalizedMessage;

import javax.xml.transform.Source;

/**
 * ScriptSE implementation of {@link InOutProvider}.
 *
 * @author Prashanth B.R
 */
public class ScreenScrapingSEInOutProvider extends AbstractInOutProvider {

    /**
     *
     */
    public ScreenScrapingSEInOutProvider() {
        // TODO Auto-generated constructor stub
    }

    /**
     *
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processIn(CRLInOut inOut, ExchangeContext ctx)
            throws JBIException {
        ScreenScrapingseEndpoint input = (ScreenScrapingseEndpoint) ctx.getEndpointManager().lookupEndpoint(inOut.getEndpoint().getServiceName(),
                inOut.getEndpoint().getEndpointName(), true);

        if (input.getEntryType() == EntryType.REQUEST_REPLY) {
            handleRequest(inOut, ctx, input);
        } else if (input.getEntryType() == EntryType.FILTER_REQUEST_REPLY) {
            handleFilterInvoke(inOut, ctx, input);
        } else {
            log().warning("Service type: " +
                    String.valueOf(input.getEntryType()) + //srvcType +
                    " not supported, skipping message " + inOut.getExchangeId());
            //Prash added
            handleRequest(inOut, ctx, input);

        //Prash added ends here.
        }
    }

    // REQUEST_REPLY_SERVICE
    // A ---> (1. inOut X)  --> B (2. Transform)
    // A <--- (3. X.out)  <---- B
    // A ----> (4. X.done) ---> B
    private void handleRequest(CRLInOut inOut, ExchangeContext ctx,
            ScreenScrapingseEndpoint input) throws JBIException {
        // 4. Receive X.done from A
        if (inOut.getStatus().equals(ExchangeStatus.DONE)) {
            // INF 11811            input.getStatus().incrementReceivedDones();
            return;
        }

        if (inOut.getStatus().equals(ExchangeStatus.ERROR)) {
            // INF 11811            input.getStatus().incrementReceivedErrors();
            return;
        }

        // 1. Receive inOut X from A
        // INF 11811        input.getStatus().incrementReceivedRequests();
        NormalizedMessage xIn = inOut.getInMessage();

        if (xIn == null) {
            inOut.sendError(new JBIException("Cannot process NULL in message!"));

            return;
        }

        // 2. Transform X ==================================================
        Source processedContent = null;

        try {
            Source content = xIn.getContent();

            if (log().isLoggable(Level.FINE)) {
                log().fine("requestReply-input: " +
                        ScreenScrapingSEUtil.print(content));
            }

            processedContent = ScreenScrapingSEUtil.transform(content, input,
                    ctx);

            if (log().isLoggable(Level.FINE)) {
                log().fine("requestReply-output: " +
                        ScreenScrapingSEUtil.print(processedContent));
            }
        } catch (Exception transformEx) {
            inOut.sendError(transformEx);

            // INF 11811            input.getStatus().incrementSentErrors();
            return;
        }

        // 3. Send X.out to A ==============================================
        inOut.reply(processedContent);

    // INF 11811        input.getStatus().incrementSentReplies();
    }

    // FILTER_REQUEST_REPLY:
    // A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
    //                         B <----------------- (4. Y.out  ) <-- C
    //                         B -----------------> (5. Y.done  ) <-- C
    // A <--- (7. X.out) <---- B (6. Transform Y)
    // A ---> (8. X.done) ---> B
    private void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx,
            ScreenScrapingseEndpoint input) throws JBIException {
        // 8. Receive X.done from A
        if (!inOut.getStatus().equals(ExchangeStatus.ACTIVE)) {
            // INF 11811            input.getStatus().incrementReceivedDones();
            return;
        }

        // 1. Receive inOut X from A
        NormalizedMessage xIn = inOut.getInMessage();

        if (xIn == null) {
            inOut.sendError(new JBIException("Cannot process NULL in message!"));

            return;
        }

        // INF 11811        input.getStatus().incrementReceivedRequests();

        // 2. Transform X ==============================================
        Source processedContent = null;

        try {
            Source content = xIn.getContent();

            if (log().isLoggable(Level.FINE)) {
                log().fine("filterRequestReply-input: " +
                        ScreenScrapingSEUtil.print(content));
            }

            processedContent = ScreenScrapingSEUtil.transform(content, input,
                    ctx);

            if (log().isLoggable(Level.FINE)) {
                log().fine("filterRequestReply-output: " +
                        ScreenScrapingSEUtil.print(processedContent));
            }
        } catch (Exception transformEx) {
            inOut.sendError(transformEx);

            // INF 11811            input.getStatus().incrementSentErrors();
            return;
        }

    // 3. Send InOut Y to C ========================================
    //        ScreenScrapingseEndpoint output = input.getInvoke();
    //        InvokableExchange newInOut =
    //                ctx.getExchangeFactory().createInvokableExchange(
    //                        Pattern.IN_OUT, output.getInfo());
    //        ScreenScrapingSEUtil.propogateTransaction(inOut, newInOut);
    //        ctx.getCorrelationMap().correlate(newInOut.getExchangeId(), inOut);
    //        newInOut.invoke(processedContent, output.getOperation());
    //        output.getStatus().incrementSentRequests();
    }

    /**
     *
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOut,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processStatus(CRLInOut msg, ExchangeContext ctx)
            throws JBIException {
        if (log().isLoggable(Level.INFO)) {
            ExchangeStatus status = msg.getStatus();

            if (ExchangeStatus.DONE.equals(status)) {
                log().info("Received DONE status for exchange: " +
                        msg.getExchangeId());
            } else if (ExchangeStatus.ERROR.equals(status)) {
                log().warning("Received ERROR status for exchange: " +
                        msg.getExchangeId());
            }
        }
    }
} //class ScreenScrapingSEInOutProvider  ends.
