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
 * @(#)AspectSEInOutProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.process;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.xml.namespace.QName;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint.EntryType;

/**
 * @author Sujit Biswas
 *
 */
public class AspectSEInOutProvider extends AbstractInOutProvider {

	/**
	 * 
	 */
	public AspectSEInOutProvider() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processIn(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	public void processIn(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		AspectSEEndpoint input = null;
		if ((inOut == null) || (ctx == null)) {
			return;
		}
		QName serviceQName = inOut.getEndpoint().getServiceName();
		String endpointName = inOut.getEndpoint().getEndpointName();
		input = (AspectSEEndpoint) ctx.getEndpointManager().lookupEndpoint(
				serviceQName, endpointName, true);

		if (input == null) {
			log().warning(
					"Service Type not supported - Skipping message "
							+ inOut.getExchangeId() + " for Service QName: "
							+ serviceQName.toString() + " Endpoint Name: "
							+ endpointName);
			return;
		}

		if ((input != null)
				&& (input.getEntryType() == EntryType.REQUEST_REPLY)) {
			handleRequest(inOut, ctx, input);
		} else if ((input != null)
				&& (input.getEntryType() == EntryType.FILTER_REQUEST_REPLY)) {
			handleFilterInvoke(inOut, ctx, input);
		} else {
			log().warning(
					"Service type: " + String.valueOf(input.getEntryType())
							+ // srvcType +
							" not supported, skipping message "
							+ inOut.getExchangeId());
		}
	}

	// REQUEST_REPLY_SERVICE
	// A ---> (1. inOut X) --> B (2. Transform)
	// A <--- (3. X.out) <---- B
	// A ----> (4. X.done) ---> B
	public void handleRequest(CRLInOut inOut, ExchangeContext ctx,
			AspectSEEndpoint input) throws JBIException {
		input.handleRequest(inOut, ctx);
	}

	// FILTER_REQUEST_REPLY:
	// A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
	// B <----------------- (4. Y.out ) <-- C
	// B -----------------> (5. Y.done ) <-- C
	// A <--- (7. X.out) <---- B (6. Transform Y)
	// A ---> (8. X.done) ---> B
	public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx,
			AspectSEEndpoint input) throws JBIException {
		input.handleFilterInvoke(inOut, ctx);
	}

	
	/**
	 * @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider#processStatus(com.sun.jbi.crl.mep.exchange.CRLInOut,
	 *      com.sun.jbi.crl.mep.ExchangeContext)
	 */
	public void processStatus(CRLInOut msg, ExchangeContext ctx)
			throws JBIException {
		if (log().isLoggable(Level.INFO)) {
			ExchangeStatus status = msg.getStatus();
			if (ExchangeStatus.DONE.equals(status)) {
				log().info(
						"Received DONE status for exchange: "
								+ msg.getExchangeId());
			} else if (ExchangeStatus.ERROR.equals(status)) {
				log().warning(
						"Received ERROR status for exchange: "
								+ msg.getExchangeId());
			}
		}
	}
}
