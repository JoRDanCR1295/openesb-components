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
 * @(#)AspectSEInOnlyProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.process;

import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyProvider;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;

/**
 * @author Sujit Biswas
 *
 */
public class AspectSEInOnlyProvider extends AbstractInOnlyProvider {
	public static Logger mLogger = Logger
			.getLogger(AspectSEInOnlyProvider.class.getName());

	/**
	 * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOnly,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	public void processIn(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		if ((inOnly == null) || (ctx == null)) {
			return;
		}

		AspectSEEndpoint input = null;

		NormalizedMessage xIn = inOnly.getInMessage();
		if (xIn == null || xIn.getContent() == null) {
			// TODO add CRLFault data?
			inOnly
					.sendError(new JBIException(
							"AspectseInOnlyProvider cannot transform missing message!"));
			return;
		}

		input = (AspectSEEndpoint) ctx.getEndpointManager().lookupEndpoint(
				inOnly.getEndpoint().getServiceName(),
				inOnly.getEndpoint().getEndpointName(), true);

		handleFilterInvoke(inOnly, ctx, input);
	}

	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx,
	AspectSEEndpoint input) throws JBIException {
		input.handleFilterInvoke(inOnly, ctx);
	}
}
