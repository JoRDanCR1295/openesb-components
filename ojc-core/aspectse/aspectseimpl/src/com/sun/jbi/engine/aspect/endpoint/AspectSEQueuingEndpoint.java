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
 * @(#)AspectSEQueuingEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import javax.jbi.JBIException;
import javax.management.Notification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEQueuingEndpointHandler;

/**
 *
 * <b>Queuing Aspect</b> In a one-way web service request scenario, if the
 * service provider is unavailable, one can use a Queuing Aspect to queue the
 * requests until the service provider becomes available again. The Queuing
 * Aspect would keep trying to make the call to the destination every
 * configurable seconds for another configurable amount of time until the
 * service provider becomes available again or the maximum number of retries is
 * reached based on the configuration.
 * <p>
 * This pattern ensures the Queuing policy acts between the client and the
 * service that is being invoked. Therefore, if this aspect is configured, all
 * messages flowing between the client and the service provider flows through
 * the Queuing engine core.
 * <p>
 * Composite Apps requiring use of this pattern will have to employ the
 * <b>Filter One-Way</b> exchange pattern.
 * <p>
 * The Queuing Aspect works on idempotent operations that do not need to go back
 * to the client for conflict resolution.
 * <p>
 * <b>Queuing Management Configuration</b>
 *
 * <ol>
 * <li>Get/Set queue strategy
 * <li>Get/Set the Max Queue size
 *
 * </ol>
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEQueuingEndpoint extends AspectSEEndpoint {

    public AspectSEQueuingEndpoint(EndpointInfo info) {
        super(info);
        setHandler(new AspectSEQueuingEndpointHandler(this));
        // TODO Auto-generated constructor stub
    }



    @Override
    public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
    throws JBIException {
        // TODO Auto-generated method stub
        super.handleFilterInvoke(inOnly, ctx);
    }


    @Override
    public void init() {
        getHandler().parse();
    }



	@Override
	public void registerMbean() {
		// TODO Auto-generated method stub

	}



	public void handleNotification(Notification notification, Object handback) {
		// TODO Auto-generated method stub

	}

}
