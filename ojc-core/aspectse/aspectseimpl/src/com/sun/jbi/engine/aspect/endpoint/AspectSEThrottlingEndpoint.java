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
 * @(#)AspectSEThrottlingEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import com.sun.jbi.engine.aspect.endpoint.mbean.AspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.ThrottleAspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.ThrottleAspectSEEndpointConfigurationMbean;
import com.sun.jbi.engine.aspect.endpoint.support.Throttle;
import javax.jbi.JBIException;
import javax.jbi.messaging.Fault;
import javax.management.AttributeChangeNotification;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEThrottlingEndpointHandler;
import javax.xml.transform.Source;

/**
 * <b>Throttling</b> (a.k.a. Choking) Aspect This aspect reduces the load on
 * services based on policies which determine: · When to delay access · When to
 * refuse access This pattern ensures the Throttling policy acts between the
 * client and the service that is being invoked. Therefore, if this aspect is
 * configured, all messages flowing between the client and the service provider
 * flows through the Throttling engine core.
 * <p>
 * Composite Apps requiring use of this aspect will have to employ the <b>Filter</b>
 * <b>Request/Reply</b> or the <b>Filter One-Way</b> exchange pattern.
 * <p>
 * The Request Throttling policy should be applied based on: 1. Service
 * Endpoints 2. Key Expressions in the message payload. 3. Remote User if the
 * user principle is available in the Normalized message header
 *
 * <p>
 * <b>Throttle Management Configuration</b> 1. Get/Set the Number of
 * connections allowed 2. Get/Set the Delay by x milliseconds 3. Get/Set the
 * Allow/Disable access based on policies
 *
 *
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEThrottlingEndpoint extends AspectSEEndpoint {

	private Throttle throttle = null;

	private String pollingTimer = "600"; // seconds

	private String throttleRate = "5"; // seconds

	private String maxThrottleQueueSize = "50000";

	public AspectSEThrottlingEndpoint(EndpointInfo info) {
		super(info);
		setHandler(new AspectSEThrottlingEndpointHandler(this));

		// Starting Throttle Thread
		throttle = new Throttle(this);

		throttle.setThrottlePolingTimer(this.getThrottlePollTimer());
		throttle.setThrottleRate(this.getThrottleRate());
		throttle.setMaxThrottleQueueSize(this.getMaxThrottleQueueSize());
		throttle.start();

		// TODO set handler for aspect throttling
	}

	@Override
	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		Boolean status = this.throttle.pushIntoQueue(inOnly, ctx);
		// System.out.println("[InOnly Message] Offer Status to Throttle Queue
		// :"
		// + status);
		if (!status) {
			Fault fault = inOnly.createFault();
			inOnly.setFault(fault);
			inOnly.send();
		}
	}

	@Override
	public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		// System.out.println(" [InOut Message] Calling Invoke ..");
		Boolean status = this.throttle.pushIntoQueue(inOut, ctx);
		// System.out.println("[InOut Message] Offer Status to Throttle Queue :"
		// + status);
		if (!status) {
			Fault fault = inOut.createFault();
			inOut.setFault(fault);
			inOut.send();
		}
	}

	public void callbackHandleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		super.handleFilterInvoke(inOnly, ctx);
	}

	public void callbackHandleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {
		super.handleFilterInvoke(inOut, ctx);
	}

	@Override
	public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
			ExchangeContext ctx) throws JBIException {
		// TODO Auto-generated method stub
		super.handleFilterResponse(inOut, originalInOut, ctx);
	}

	public double getThrottlePollTimer() {
		return Double.parseDouble(this.pollingTimer);
	}

	public void setPollingTimer(String pollingTimer) {
		this.pollingTimer = pollingTimer;
		throttle.setThrottlePolingTimer(this.getThrottlePollTimer());
	}

	public double getThrottleRate() {
		return Double.parseDouble(this.throttleRate);
	}

	public void setThrottleRate(String throttleRate) {
		this.throttleRate = throttleRate;
		throttle.setThrottleRate(this.getThrottleRate());
	}

	public int getMaxThrottleQueueSize() {
		return Integer.parseInt(this.maxThrottleQueueSize);
	}

	public void setMaxThrottleQueueSize(String maxQueueSize) {
		this.maxThrottleQueueSize = maxQueueSize;
		throttle.setMaxThrottleQueueSize(this.getMaxThrottleQueueSize());
	}

	@Override
	public void init() {
		getHandler().parse();
	}

	public void shutdown() throws InstanceNotFoundException,
			MBeanRegistrationException {
		// System.out.println("Shutting down Throttle Aspect");
		this.throttle.stopThrottleThread();
		super.shutdown();
	}

	@Override
	public void registerMbean() {
		try {
			AspectSEEndpointConfiguration mbean = new ThrottleAspectSEEndpointConfiguration(
					this.pollingTimer, this.throttleRate,
					this.maxThrottleQueueSize);
			mbean.addNotificationListener(this, null, null);
			super.registerMbean(mbean, "Throttling");
		} catch (NotCompliantMBeanException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void handleNotification(Notification notification, Object handback) {
		String attributeName = null;
		AttributeChangeNotification attributeNotification = null;
		String attributeValue = null;
		if (notification instanceof AttributeChangeNotification) {
			attributeNotification = (AttributeChangeNotification) notification;

			// Check if configuration change is for Cache SE component
			if (attributeNotification.getSource() instanceof ThrottleAspectSEEndpointConfigurationMbean) {
				attributeName = attributeNotification.getAttributeName();

				if (true == attributeName
						.equals(AspectConstants.THROTTLE_POLLING_TIMER)) {
					attributeValue = (String) attributeNotification
							.getNewValue();
					try {
						this.setPollingTimer(attributeValue);
					} catch (Exception e) {
						// leave the old rate. Dont change.
					}
				}

				if (true == attributeName.equals(AspectConstants.THROTTLE_RATE)) {
					attributeValue = (String) attributeNotification
							.getNewValue();
					try {
						this.setThrottleRate(attributeValue);
					} catch (NumberFormatException ex) {
						// ignore
					}
				}

				if (true == attributeName
						.equals(AspectConstants.MAX_THROTTLE_QUEUE_SIZE)) {
					attributeValue = (String) attributeNotification
							.getNewValue();
					try {
						this.setMaxThrottleQueueSize(attributeValue);
					} catch (NumberFormatException ex) {
						// ignore
					}
				}

				// persist the changes to the corresponding config file
				save();
			}
		}
	}

}
