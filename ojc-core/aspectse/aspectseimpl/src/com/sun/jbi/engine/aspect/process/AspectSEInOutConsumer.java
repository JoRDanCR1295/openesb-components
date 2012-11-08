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
 * @(#)AspectSEInOutConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.process;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;

/**
 * @author Sujit Biswas
 * 
 */
public class AspectSEInOutConsumer extends AbstractInOutConsumer {

	/**
	 * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processFault(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 * 
	 * The fault handling remains same irrespective of the advice or pattern
	 */
	public void processFault(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		// FILTER_REQUEST_REPLY:
		// A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
		// B <----------------- (4. Y.fault ) <-- C
		// B -----------------> (5. Y.done ) <-- C
		// A <--- (7. X.out) <---- B (6. Transform Y)
		// A ---> (8. X.done) ---> B

		if ((inOut == null) || (ctx == null)) {
			return;
		}
		CRLInOut originalInOut = null;
		AspectSEEndpoint input = null;

		originalInOut = (CRLInOut) ctx.getCorrelationMap().decorrelate(
				inOut.getExchangeId());

		input = (AspectSEEndpoint) // B
		ctx.getEndpointManager().lookupEndpoint(
				originalInOut.getEndpoint().getServiceName(),
				originalInOut.getEndpoint().getEndpointName(), true);

		handleFault(inOut, originalInOut, input);
	}

	/**
	 * delegate fault handling to the aspect se endpoints, some endpoints like
	 * auto-reconnect needs to handle fault in a different way
	 */
	public void handleFault(CRLInOut inOut, CRLInOut originalInOut,
			AspectSEEndpoint input) throws JBIException {
		
		AspectSEEndpoint lastEpt = getLastEndpoint(input);
		lastEpt.handleFault(inOut, originalInOut);
	}

	/**
	 * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processOut(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	public void processOut(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		// FILTER_REQUEST_REPLY:
		// A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
		// B <----------------- (4. Y.out ) <-- C
		// B -----------------> (5. Y.done ) <-- C
		// A <--- (7. X.out) <---- B (6. Transform Y)
		// A ---> (8. X.done) ---> B

		CRLInOut originalInOut = null;
		AspectSEEndpoint input = null;

		originalInOut = (CRLInOut) // A <--> B
		ctx.getCorrelationMap().decorrelate(inOut.getExchangeId());

		input = (AspectSEEndpoint) // B
		ctx.getEndpointManager().lookupEndpoint(
				originalInOut.getEndpoint().getServiceName(),
				originalInOut.getEndpoint().getEndpointName(), true);

		handleFilterResponse(inOut, originalInOut, input, ctx);
	}

	public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
			AspectSEEndpoint input, ExchangeContext ctx) throws JBIException {

		AspectSEEndpoint lastEpt = getLastEndpoint(input);

		// note this intercepts the message for both IN and OUT
		lastEpt.handleFilterResponse(inOut, originalInOut, ctx);

		// if the interception is required only for IN case use
		// input.handleFilterResponse(inOut, originalInOut,ctx);
	}

	private AspectSEEndpoint getLastEndpoint(AspectSEEndpoint input) {
		AspectSEEndpoint lastEpt = input;
		while (lastEpt.getNext() != null) {
			lastEpt = lastEpt.getNext();
		}
		return lastEpt;
	}
}
