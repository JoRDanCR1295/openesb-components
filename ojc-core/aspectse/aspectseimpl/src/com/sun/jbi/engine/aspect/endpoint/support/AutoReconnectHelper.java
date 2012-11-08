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
 * @(#)AutoReconnectHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.engine.aspect.endpoint.AspectSEAutoReconnectEndpoint;

/**
 * This Class handles all the auto-reconnect operations. The Auto-Reconnect
 * Endpoint delegates the retry action to this class.
 * 
 * @author karthikeyan s
 */
public class AutoReconnectHelper {

	/* AspectSEAutoReconnectEndpoint endpoint */
	private AspectSEAutoReconnectEndpoint endpoint;

	/* reconnect queue */
	private Map<String, AutoReconnectThread> reconnectMap = new HashMap<String, AutoReconnectThread>();

	// TODO use this logger
	private Logger logger = Logger.getLogger(AutoReconnectHelper.class
			.getName());

	/**
	 * Creates a new instance of AutoReconnectHelper
	 */
	public AutoReconnectHelper(AspectSEAutoReconnectEndpoint endpt) {
		// Using default value.
		endpoint = endpt;
	}

	/**
	 * This method tries to invoke an endpoint. If it doesn't succeed, the
	 * endpoint is placed in reconnect queue.
	 * 
	 */
	public void invokeThroughReconnector(CRLInOut inOut, ExchangeContext ctx) {
		AutoReconnectThread reconnector = new AutoReconnectThread(inOut, ctx,
				endpoint, Long.parseLong(endpoint.getRate()), Integer
						.parseInt(endpoint.getTimeout()));
		reconnectMap.put(inOut.getExchangeId(), reconnector);
	}

	/**
	 * This method tries to invoke an endpoint. If it doesn't succeed, the
	 * endpoint is placed in reconnect queue.
	 * 
	 */
	public void invokeThroughReconnector(CRLInOnly inOnly, ExchangeContext ctx) {
		AutoReconnectThread reconnector = new AutoReconnectThread(inOnly, ctx,
				endpoint, Long.parseLong(endpoint.getRate()), Integer
						.parseInt(endpoint.getTimeout()));
		reconnectMap.put(inOnly.getExchangeId(), reconnector);
	}

	/**
	 * This method cleans up the thread for the corresponding message.
	 * 
	 */
	public void handleResponse(CRLInOut inOut) {
		AutoReconnectThread reconnector = reconnectMap.get(inOut
				.getExchangeId());
		if (reconnector != null) {
			reconnector.interrupt();
			Object lock = reconnector.getLock();
			synchronized (lock) {
				lock.notify();
			}
		}
	}

	/**
	 * This method notifies the thread that fault has occured.
	 * 
	 * @param inOut
	 *            message
	 * @return true if timeout has occured false if timeout has not been
	 *         reached.
	 * 
	 */

	// TODO this method should return the fault message that comes from the
	// httpsoapbc and notify the auto-reconnect thread to exit when max no of
	// tries has been reached
	public boolean handleFault(CRLInOut inOut) {
		AutoReconnectThread reconnector = reconnectMap.get(inOut
				.getExchangeId());
		if (reconnector != null) {
			if (reconnector.isMaxReached()) {
				reconnector.interrupt();
				Object lock = reconnector.getLock();
				synchronized (lock) {
					lock.notify();
				}
				return true;
			}
			reconnector.setFaultFlag(true);
			Object lock = reconnector.getLock();
			synchronized (lock) {
				lock.notify();
			}
		}
		return false;
	}

	/**
	 * This method interrupts all threads and cleans them up.
	 * 
	 */
	public void cleanup() {
		Iterator it = reconnectMap.keySet().iterator();
		while (it.hasNext()) {
			AutoReconnectThread reconnector = reconnectMap.get((String) it
					.next());
			reconnector.interrupt();
			Object lock = reconnector.getLock();
			synchronized (lock) {
				lock.notify();
			}
		}
	}

	// TODO this method should return the error status that comes from the
	// httpsoapbc and notify the auto-reconnect thread to exit when max no of
	// tries has been reached
	public boolean handleStatus(CRLInOnly inOnly) {
		AutoReconnectThread reconnector = reconnectMap.get(inOnly
				.getExchangeId());
		if (reconnector != null) {
			if (reconnector.isMaxReached()) {
				reconnector.interrupt();
				Object lock = reconnector.getLock();
				synchronized (lock) {
					lock.notify();
				}
				return true;
			}
			if (inOnly.getStatus().equals(ExchangeStatus.DONE)) {
				reconnector.interrupt();
				Object lock = reconnector.getLock();
				synchronized (lock) {
					lock.notify();
				}
				return true;
			} else if (inOnly.getStatus().equals(ExchangeStatus.ERROR)) {
				reconnector.setFaultFlag(true);
				Object lock = reconnector.getLock();
				synchronized (lock) {
					lock.notify();
				}
			}
		}
		return false;
	}

	/**
	 * This Class tries to invoke the specified service for every x seconds for
	 * y number of times where x and y are configurable.
	 * 
	 * @author karthikeyan s
	 */
	private class AutoReconnectThread extends Thread {

		private long retry;

		private Object lock = new Object();

		private int timeout;

		private CRLInOut inOut;

		private CRLInOnly inOnly;

		private ExchangeContext ctx;

		private AspectSEAutoReconnectEndpoint endpoint;

		boolean shouldContinue = true;

		private int counter = 1;

		public AutoReconnectThread(CRLInOut inOutME, ExchangeContext exCtx,
				AspectSEAutoReconnectEndpoint endpt, long retries,
				int retryTimeout) {
			super("AutoReconnectThread:" + UUID.randomUUID().toString());
			inOut = inOutME;
			ctx = exCtx;
			endpoint = endpt;
			retry = retries;
			timeout = retryTimeout;
			start();
		}

		public AutoReconnectThread(CRLInOnly inOnlyME, ExchangeContext exCtx,
				AspectSEAutoReconnectEndpoint endpt, long retries,
				int retryTimeout) {
			super("AutoReconnectThread:" + UUID.randomUUID().toString());
			inOnly = inOnlyME;
			ctx = exCtx;
			endpoint = endpt;
			retry = retries;
			timeout = retryTimeout;
			start();
		}

		public void run() {
			while (true) {
				if (isInterrupted()) {
					return;
				}
				if (shouldContinue) {
					if (counter++ <= timeout) {
						try {
							Logger.getLogger(this.getName()).log(
									Level.INFO,
									"AutoReconnect Thread : count:"
											+ String.valueOf(counter - 1));
							if (inOut != null) {
								endpoint.callbackHandleFilterInvoke(inOut, ctx);
							} else {
								endpoint
										.callbackHandleFilterInvoke(inOnly, ctx);
							}
							shouldContinue = false;
						} catch (Exception ex) {
							shouldContinue = true;
							Logger.getLogger(this.getName()).log(Level.INFO,
									"JBI Exception received.", ex);
						}
						try {
							sleep(retry);
						} catch (InterruptedException ex) {
							return;
						}
					} else {
						logger
								.info("Auto Reconnect Thread unable to connect after: "
										+ timeout
										+ " try, reply with fault message");
						if (inOut != null) {
							try {
								Fault fault = inOut.createFault();
								inOut.setFault(fault);
								inOut.send();
							} catch (Exception ex) {
								// ignore
							}
						} else {
							try {
								inOnly.setStatus(ExchangeStatus.ERROR);
								inOnly.send();
							} catch (Exception ex) {
								// ignore
							}
						}
					}
				} else {
					synchronized (lock) {
						try {
							lock.wait();
						} catch (InterruptedException ex) {
							// ignore
						}
					}
				}
			}
		}

		public CRLInOut getOriginalME() {
			return inOut;
		}

		public void setRetry(long retries) {
			retry = retries;
		}

		public void setTimeout(int retryTimeout) {
			timeout = retryTimeout;
		}

		public void setFaultFlag(boolean flag) {
			shouldContinue = flag;
		}

		public Object getLock() {
			return lock;
		}

		public boolean isMaxReached() {
			return counter > timeout;
		}
	}
}
