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
 * @(#)AspectSEInOnlyConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.process;

import javax.jbi.JBIException;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;

/**
 * @author Sujit Biswas
 *
 */
public class AspectSEInOnlyConsumer extends AbstractInOnlyConsumer {
	

	
	public void processStatus(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		if ((inOnly == null) || (ctx == null)) {
			return;
		}
		// 4. Receive Y.done or Y.error from C
		CRLInOnly originalInOnly = (CRLInOnly) ctx.getCorrelationMap()
				.decorrelate(inOnly.getExchangeId());
		AspectSEEndpoint input = (AspectSEEndpoint) ctx.getEndpointManager()
				.lookupEndpoint(originalInOnly.getEndpoint().getServiceName(),
						originalInOnly.getEndpoint().getEndpointName(), true); // provisioning
		
		
		
		handleStatus(inOnly, originalInOnly, input);
	}

	public void handleStatus(CRLInOnly inOnly, CRLInOnly originalInOnly, AspectSEEndpoint input) throws MessagingException, JBIException {
		
		AspectSEEndpoint lastEndpoint = getLastEndpoint(input);
		lastEndpoint.handleStatus(inOnly, originalInOnly);
	}
	
	private AspectSEEndpoint getLastEndpoint(AspectSEEndpoint input) {
		AspectSEEndpoint lastEpt = input;
		while (lastEpt.getNext() != null) {
			lastEpt = lastEpt.getNext();
		}
		return lastEpt;
	}
}
