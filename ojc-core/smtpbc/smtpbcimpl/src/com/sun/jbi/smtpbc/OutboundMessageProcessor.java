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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import java.net.URI;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.smtpbc.Endpoint.EndpointState;
import com.sun.jbi.smtpbc.extservice.EmailClientAgent;
import com.sun.jbi.smtpbc.extservice.EmailData;

/**
 * Process replies/requests received from the SE.
 */
public class OutboundMessageProcessor implements Runnable {

	private static final Messages mMessages = Messages
			.getMessages(OutboundMessageProcessor.class);

	private static final Logger mLog = Messages
			.getLogger(OutboundMessageProcessor.class);

	private Collection mServiceUnits;

	private MessageStore mMessageStore;

	private DeliveryChannel mChannel;

	private EmailClientAgent mEmailClient;

	// private Object mMonitor;
	private AtomicBoolean mMonitor;

	public OutboundMessageProcessor(final DeliveryChannel chnl,
			final Collection serviceUnits, final MessageStore messageStore) {
		mChannel = chnl;
		mServiceUnits = serviceUnits;
		mMessageStore = messageStore;
		// mMonitor = new Object();
		mMonitor = new AtomicBoolean(true);
		mEmailClient = new EmailClientAgent();
	}

	/**
	 * Process the message exchange
	 */
	public void run() {
		MessageExchange mExchange = null;
		try {
			do {
				mExchange = mChannel.accept(5000);
				if (mExchange != null) {
					// First see if this is a response to a message from an
					// inbound message we sent
					final String exchangeId = mExchange.getExchangeId();
					if (OutboundMessageProcessor.mLog.isLoggable(Level.FINE)) {
						OutboundMessageProcessor.mLog.log(Level.FINE,
								"OMP_Accept_msg", exchangeId);
					}

					if (mMessageStore.has(exchangeId)) {
						mMessageStore.get(exchangeId).emailProcessed();
						return;
					}

					final ServiceEndpoint se = mExchange.getEndpoint();
					Endpoint destination = null;
					final Iterator it = mServiceUnits.iterator();
					while (it.hasNext()) {
						final Iterator it2 = ((ServiceUnit) it.next())
								.getEndpoints().iterator();
						while (it2.hasNext()) {
							final Endpoint endpoint = (Endpoint) it2.next();
							final QName serviceName = mExchange.getEndpoint()
									.getServiceName();
							final String endpointName = mExchange.getEndpoint()
									.getEndpointName();

							if (endpoint.getServiceName().equals(serviceName)
									&& endpoint.getEndpointName().equals(
											endpointName)) {
								destination = endpoint;
							}
						}
					}

					if (destination == null) {
						OutboundMessageProcessor.mLog.log(Level.SEVERE,
								"OMP_Null_Dest", new Object[] { se,
										mExchange.getExchangeId() });
						return;
					}

					final EndpointState state = destination.getState();
					if (!(state.equals(EndpointState.RUNNING))) {
						// If the endpoint is not in the RUNNING state
						// (i.e. is stopped or shutdown), ignore the message
						final String strState = (state == EndpointState.STOPPED ? OutboundMessageProcessor.mMessages
								.getString("OMP_Stopped")
								: OutboundMessageProcessor.mMessages
										.getString("OMP_Shutdown"));

						if (OutboundMessageProcessor.mLog
								.isLoggable(Level.INFO)) {
							OutboundMessageProcessor.mLog
									.log(Level.INFO, "OMP_EP_state",
											new Object[] {
													destination
															.getEndpointName(),
													strState,
													mExchange.getExchangeId() });
						}

					} else {
						final URI pattern = mExchange.getPattern();
						if (OutboundMessageProcessor.mLog
								.isLoggable(Level.FINER)) {
							OutboundMessageProcessor.mLog.log(Level.FINER,
									"OMP_Pattern",
									new Object[] { mExchange.getExchangeId(),
											pattern });
						}

						final String pat = pattern.toString().trim();
						switch (ExchangePattern.valueOf(mExchange)) {
						case IN_ONLY:
							OutboundMessageProcessor.mLog.log(Level.INFO,
									"OMP_Recv_InOnly", mExchange
											.getExchangeId());
							processInOnly((InOnly) mExchange, destination);
							break;
						case IN_OUT:
							OutboundMessageProcessor.mLog
									.log(Level.WARNING, "OMP_Recv_InOut",
											mExchange.getExchangeId());
							break;
						case ROBUST_IN_ONLY:
							OutboundMessageProcessor.mLog.log(Level.WARNING,
									"OMP_Robust_inonly", mExchange
											.getExchangeId());
							break;
						case IN_OPTIONAL_OUT:
							OutboundMessageProcessor.mLog.log(Level.SEVERE,
									"OMP_Invalid_pattern", mExchange
											.getExchangeId());
							break;
						default:
							OutboundMessageProcessor.mLog.log(Level.SEVERE,
									"OMP_Invalid_pattern", mExchange
											.getExchangeId());
							break;
						}
					}

				} else { // mExchange is null
					// mLog.log(Level.INFO, "OMP_Null_Ex");
					continue;
				}
				// } while(mMonitor != null);
			} while (mMonitor.get());

			if (OutboundMessageProcessor.mLog.isLoggable(Level.FINE)) {
				OutboundMessageProcessor.mLog.log(Level.FINE,
						"OMP_Complete_processing", mExchange.getExchangeId());
			}

		} catch (final Throwable e) {
			OutboundMessageProcessor.mLog.log(Level.INFO,
					OutboundMessageProcessor.mMessages
							.getString("OMP_Unexpected_exception", e
									.getMessage()));
		}
	}

	public void processInOnly(final InOnly inonly, final Endpoint destination) {
		try {
			// Update our counters to keep track of messages
			destination.getEndpointStatus().incrementReceivedRequests();

			// Grab our normalized message
			final NormalizedMessage inMsg = inonly.getInMessage();

			// Convert our normalized message to a format that our EmailClient
			// understands
			final EmailData data = new SMTPDenormalizer().denormalize(inMsg,
					inonly.getOperation(), destination);

			boolean success = true;
			try {
				mEmailClient.sendMessage(data.getEmailConfiguration(), data
						.getEmailMessage());
				inonly.setStatus(ExchangeStatus.DONE);
			} catch (final Exception ex) {
				success = false;
				OutboundMessageProcessor.mLog.log(Level.WARNING,
						"OMP_Exception_Sending");
				OutboundMessageProcessor.mLog.log(Level.WARNING, ex
						.getMessage());
				inonly.setStatus(ExchangeStatus.ERROR);
				mEmailClient.sendFailureNotification(data
						.getEmailConfiguration(), data.getEmailMessage());
			}
			mChannel.send(inonly);

			// Update our counters to keep track of messages
			if (success) {
				destination.getEndpointStatus().incrementSentDones();
			} else {
				destination.getEndpointStatus().incrementSentErrors();
			}
		} catch (final Exception ex) {
			OutboundMessageProcessor.mLog.log(Level.WARNING,
					"OMP_Exception_ProcessReply", ex);
		}
	}

	/**
	 * Stop the processor thread.
	 */
	public void stopReceiving() {
		if (OutboundMessageProcessor.mLog.isLoggable(Level.INFO)) {
			OutboundMessageProcessor.mLog.log(Level.INFO, "OMP_Stopped_thread");
		}
		// mMonitor = null;
		mMonitor.set(false);
	}
}
