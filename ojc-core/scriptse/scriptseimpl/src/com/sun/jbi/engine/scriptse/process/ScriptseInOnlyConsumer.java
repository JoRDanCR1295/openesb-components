/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.sun.jbi.engine.scriptse.process;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer;
import com.sun.jbi.engine.scriptse.ScriptseEndpoint;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;


/**
 * Consumes status responses from one-way invocations.
 *
 * @author Prashanth B.R
 */
public class ScriptseInOnlyConsumer extends AbstractInOnlyConsumer {
    /**
     * 
     * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOnly,
     *      com.sun.jbi.crl.mep.ExchangeContext)
     */
    public void processStatus(CRLInOnly inOnly, ExchangeContext ctx)
        throws JBIException {
        // 4. Receive Y.done or Y.error from C
        CRLInOnly originalInOnly = (CRLInOnly) ctx.getCorrelationMap()
                                                  .decorrelate(inOnly.getExchangeId());
        ScriptseEndpoint input = (ScriptseEndpoint) ctx.getEndpointManager()
                                                       .lookupEndpoint(
                originalInOnly.getEndpoint().getServiceName(),
                originalInOnly.getEndpoint().getEndpointName(), true
            ); // provisioning
        ScriptseEndpoint output = input.getInvoke();

        // propogate transaction regardless of status
        ScriptseUtil.propogateTransaction(inOnly, originalInOnly);

        // 5. Send X.done or X.error to A ========================
        if (inOnly.getStatus().equals(ExchangeStatus.DONE)) {
// INF 111811            output.getStatus().incrementReceivedDones();
            originalInOnly.setStatus(ExchangeStatus.DONE);
            originalInOnly.send();
// INF 111811            input.getStatus().incrementSentDones();
        } else { // ExchangeStatus.ERROR
// INF 111811            output.getStatus().incrementReceivedErrors();
            originalInOnly.setStatus(ExchangeStatus.ERROR);
            originalInOnly.send();
// INF 111811            input.getStatus().incrementSentErrors();
        }
    }
}
