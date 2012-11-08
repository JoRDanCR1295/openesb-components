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
 * @(#)AspectSEMessageTrackingEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEMessageTrackingEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.mbean.AspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.MessageTrackingAspectSEEndpointConfiguration;

/**
 * <b>Message Tracking Aspect</b> The Message Tracking aspect can be used to
 * track messages flow through the system. Message Tracking will happen based on
 * a configurable Rule Set which can be configured from the Web Console.
 * <p>
 * This pattern ensures the Message Tracking policy acts between the client and
 * the multiple services that are involved to service the operation. Therefore,
 * if this aspect is configured, all messages flowing between the client and the
 * service providers flow through the Message Tracking engine core.
 *
 * <p>
 * Composite Apps requiring use of this pattern will have to employ either the
 * <b>Filter Request/Reply</b> or the <b>Filter One-Way</b> exchange pattern.
 * Message Tracking Management Configuration 1. Get/Set/Add <RuleSet/>s 2.
 * Get/Set/Add/Edit <Rule/>s to <RuleSet/> 3. Specification based 4.
 * Pre-condition based 5. Get messages based on ID - Each normalized message has
 * a unique message identifier that lets you select all the recorded events for
 * a specific message
 *
 *
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEMessageTrackingEndpoint extends AspectSEEndpoint {

	private static Logger logger = Logger
			.getLogger(AspectSEMessageTrackingEndpoint.class.getName());

	public AspectSEMessageTrackingEndpoint(EndpointInfo info) {
		super(info);
		setHandler(new AspectSEMessageTrackingEndpointHandler(this));
		// TODO Auto-generated constructor stub
	}

	@Override
	public void init() {
		getHandler().parse();
	}

	@Override
	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		// TODO code for message tracking
		logger.info("IN message " + inOnly.toString());
		super.handleFilterInvoke(inOnly, ctx);
	}

	@Override
	public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		// TODO code for message tracking
		logger.info("IN message " + inOut.toString());
		super.handleFilterInvoke(inOut, ctx);
	}

	@Override
	public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
			ExchangeContext ctx) throws JBIException {

		// TODO code for message tracking
		logger.info("OUT message " + inOut.toString());
		super.handleFilterResponse(inOut, originalInOut, ctx);
	}

	@Override
	public void registerMbean() {
		try {
			AspectSEEndpointConfiguration mbean = new MessageTrackingAspectSEEndpointConfiguration();
			mbean.addNotificationListener(this, null, null);
			super.registerMbean(mbean, "MessageTracking");
		} catch (NotCompliantMBeanException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public void handleNotification(Notification notification, Object handback) {
		// TODO Auto-generated method stub

	}

}
