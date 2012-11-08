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
 * @(#)Throttle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.util.LinkedList;
import java.util.Queue;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.AspectSEThrottlingEndpoint;

/**
 * This is a runnable class that privides a mechanism to implement configurable
 * delay between the incoming and outgoing messages from the Aspect SE.
 * 
 * @author bharanim
 */
public class Throttle extends Thread {

	private static Logger mLogger = Logger.getLogger(Throttle.class.getName()); // Logger

	private Queue<ThrottleInput> throttleQueue = null;

	private AspectSEThrottlingEndpoint aspectSEThrottlingEndPoint = null;

	private double THROTTLE_POLLING_TIMER, THROTTLE_RATE; // sec

	private int MAX_THROTTLE_QUEUE_SIZE;

	private Boolean throttleIsRunning = true;

	private Boolean throttleInProgress = false;

	private int inputsProcessed = 0;

	/** Creates a new instance of Throttle */
	public Throttle() {
	}

	public Throttle(AspectSEThrottlingEndpoint aspectSEThrottlingEndPoint) {
		throttleQueue = new LinkedList<ThrottleInput>();
		this.aspectSEThrottlingEndPoint = aspectSEThrottlingEndPoint;
	}

	/**
	 * 
	 * Generic method to inplement thread delays.
	 */
	private void delay(double seconds) {
		try {
			// System.out.println("Going to Sleep for [ " + seconds + " ]
			// sec.");
			Thread.sleep((long) (seconds * 1000));
		} catch (InterruptedException ex) {
			this.throttleInProgress = true;
			mLogger
					.warning("Throttle Thread has been interrupted my incoming messages. Throttle in progress status changed to : "
							+ this.throttleInProgress);
		}
	}

	/**
	 * 
	 * Throttle the incoming data here.
	 */
	private void processData() throws JBIException {
		if (this.throttleQueue.peek() != null) {
			ThrottleInput throttleInputObj = (ThrottleInput) this.throttleQueue
					.poll();
			// System.out.println("Processing Data :: (Processed :" +
			// ++this.inputsProcessed + " ) (Current Size in Throttle : " +
			// this.throttleQueue.size() + " )");
			if (throttleInputObj.getCRLInOut() != null) {
				this.aspectSEThrottlingEndPoint.callbackHandleFilterInvoke(
						throttleInputObj.getCRLInOut(), throttleInputObj
								.getExchangeContext());
			} else if (throttleInputObj.getCRLInOnly() != null) {
				this.aspectSEThrottlingEndPoint.callbackHandleFilterInvoke(
						throttleInputObj.getCRLInOnly(), throttleInputObj
								.getExchangeContext());
			}

			delay(this.THROTTLE_RATE);
		}
	}

	/**
	 * 
	 * Push the Incoming Messages into the Queue used for Throttling (In Only)
	 */
	public boolean pushIntoQueue(CRLInOnly inOnly, ExchangeContext ctx) {
		Boolean offer_status = false;
		if (this.throttleQueue.size() < this.MAX_THROTTLE_QUEUE_SIZE) {
			ThrottleInput throttleInputObj = new ThrottleInput(inOnly, ctx);
			offer_status = this.throttleQueue.offer(throttleInputObj);
			if (offer_status) {
				if (!this.isThrottleInProgress()) {
					this.interrupt();
					return offer_status;
				}
			} else {
				return offer_status;
			}
		}
		return offer_status;
	}

	/**
	 * 
	 * Push the Incoming Messages into the Queue used for Throttling (In Out)
	 */
	public boolean pushIntoQueue(CRLInOut inOut, ExchangeContext ctx) {
		Boolean offer_status = false;
		if (this.throttleQueue.size() < this.MAX_THROTTLE_QUEUE_SIZE) {
			ThrottleInput throttleInputObj = new ThrottleInput(inOut, ctx);
			offer_status = this.throttleQueue.offer(throttleInputObj);
			if (offer_status) {
				if (!this.isThrottleInProgress()) {
					this.interrupt();
					return offer_status;
				}
			} else {
				return offer_status;
			}
		}
		return offer_status;
	}

	/**
	 * 
	 * Stop Running the Throttle
	 */
	public void stopThrottleThread() {
		this.throttleIsRunning = false;
	}

	public void setThrottlePolingTimer(double throttlePolingTimer) {
		this.THROTTLE_POLLING_TIMER = throttlePolingTimer;
		mLogger.info("Setting Throttle Poll Timer to ["
				+ this.THROTTLE_POLLING_TIMER + "] sec");
	}

	public void setThrottleRate(double throttleRate) {
		this.THROTTLE_RATE = throttleRate;
		mLogger.info("Setting Throttle Rate to [" + this.THROTTLE_RATE
				+ "] sec");
	}

	public void setMaxThrottleQueueSize(int maxQueueSize) {
		this.MAX_THROTTLE_QUEUE_SIZE = maxQueueSize;
		mLogger.info("Setting Max Throttle Queue Size to ["
				+ this.MAX_THROTTLE_QUEUE_SIZE + "]");
	}

	public Boolean isThrottleInProgress() {
		return this.throttleInProgress;
	}

	public void run() {

		while (throttleIsRunning) {

			if (!this.throttleInProgress) {
				delay(this.THROTTLE_POLLING_TIMER);
			} else {
				if (this.throttleQueue.size() == 0) {
					this.throttleInProgress = false;
					this.inputsProcessed = 0; // resetting input counter.
				} else {
					try {
						processData();
					} catch (JBIException ex) {
						mLogger
								.warning("Unable to process data from the Throttle");
						ex.printStackTrace();
					}
				}
			}
		}

		mLogger.info("Throttle Thread has been stopped.");
	}
}
