package com.sun.jbi.engine.aspect.endpoint;

import java.util.HashMap;
import java.util.List;
import java.util.Iterator;
import java.util.logging.Level;

import javax.management.Notification;

import javax.xml.transform.Source;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.exchange.InvokableExchange;
import com.sun.jbi.engine.aspect.endpoint.support.CacheSEUtil;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSETeeEndpointHandler;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;

/**
 *
 *
 * <b>Tee Aspect</b> In some scenarios, you want to redirect the same message
 * to two different components. The Tee Aspect Service Engine core introduces a
 * T-junction that facilitates message redirection to two different components
 * at the same time.
 * <p>
 * It acts very much like the tee command in Unix where, for example, you may
 * want to save the output of a command in a file, while at the same time see
 * the results on the terminal as it is being produced. Then if you see that the
 * output is in error, you can stop the command.
 *
 * <p>
 * Operation of the Unix tee command
 *
 * The tee command in Unix acts like a T-junction. For e.g., the UNIX command
 * <p>
 * ls -al | tee folderdetails
 * <p>
 * shows all the details of the current folder on the terminal screen and also
 * redirects the details to a file named folderdetails.
 * <p>
 * This pattern ensures that the Tee policy acts between the client and the
 * service that is involved to service the operation. Therefore, if this aspect
 * is configured, all messages flowing between the client and the service
 * providers flow through the Tee engine core. The routing will happen based on
 * a configurable Rule Set which can be configured from the Web Console.
 * <p>
 * Composite Apps requiring use of this pattern will have to employ the
 * <b>Filter</b> <b>One-Way</b> exchange pattern.
 *
 *
 * @author Sujit Biswas, Manish Bharani
 *
 */
public class AspectSETeeEndpoint extends AspectSEEndpoint {

	private HashMap<String, AspectSEEndpoint> inOnly_InvokeMap = new HashMap<String, AspectSEEndpoint>();

	private HashMap<String, Integer> originalInOnlyCounter = new HashMap<String, Integer>();

	public AspectSETeeEndpoint(EndpointInfo info) {
		super(info);
		setHandler(new AspectSETeeEndpointHandler(this));
		// TODO set handler for aspect tee
	}

	@Override
	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		Source source = inOnly.getInMessage().getContent();
		List<AspectSEEndpoint> invokes = getRuleset().evaluate(
				CacheSEUtil.convertToDOMSource(source, ctx));

		originalInOnlyCounter.put(inOnly.getExchangeId(), Integer
				.valueOf(invokes.size()));

		// 1. Receive inOnly X from A
		get_Status().incrementReceivedRequests();

		Iterator it = invokes.iterator();
		while (it.hasNext()) {
			AspectSEEndpoint endpt = (AspectSEEndpoint) it.next();
			setInvoke(endpt);
			_handleFilterInvoke(inOnly, ctx);
		}
	}

	public void _handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		try {
			AspectSEEndpoint output = getInvoke();

			// 2. Transform==================================
			Source processedContent = null;
			try {
				NormalizedMessage xIn = inOnly.getInMessage();
				Source content = xIn.getContent();

				if (log().isLoggable(Level.FINE)) {
					log().fine(
							"filterOneWay-input: "
									+ AspectSEUtil.print(content));
				}

				// TODO use the pattern here
				processedContent = content;

				if (log().isLoggable(Level.FINE)) {
					log().fine(
							"filterOneWay-output: "
									+ AspectSEUtil.print(processedContent));
				}
			} catch (Exception ex) {
				log().log(Level.SEVERE, ex.getMessage(), ex);
				inOnly.sendError(ex);
				get_Status().incrementSentErrors();
				return;
			}

			// handover the inOnly to the next aspect;
			if (next != null) {
				next.handleFilterInvoke(inOnly, ctx);
				return;
			}

			// ===================================================

			// 3. Send inOnly Y to C ===========================
			InvokableExchange newInOnly = ctx.getExchangeFactory()
					.createInvokableExchange(ExchangePattern.IN_ONLY,
							output.getInfo());
			AspectSEUtil.propogateTransaction(inOnly, newInOnly);
			ctx.getCorrelationMap()
					.correlate(newInOnly.getExchangeId(), inOnly);

			inOnly_InvokeMap.put(newInOnly.getExchangeId(), output);

			newInOnly.invoke(processedContent, output.getOperation());
			// output.get_status().incrementSentRequests();
			// ========================================================
		} catch (Exception ex) {
			log().log(
					Level.SEVERE,
					"An unexpected error occurred provisioning a one-way service: "
							+ ex.getMessage(), ex);
			inOnly.sendError(ex);
			if (this != null)
				get_Status().incrementSentErrors();
		}
	}

	@Override
	public synchronized void handleStatus(CRLInOnly inOnly,
			CRLInOnly originalInOnly) throws MessagingException, JBIException {

		// propogate transaction regardless of status

		if (this.next == null) {
			AspectSEEndpoint output = null;
			try {
				output = inOnly_InvokeMap.remove(inOnly.getExchangeId()); // C
				if (inOnly.getStatus().equals(ExchangeStatus.DONE)) {
					output.get_Status().incrementReceivedDones();
				} else {
					// output.get_status().incrementReceivedErrors();
				}
			} catch (RuntimeException e) {
				this.log().log(Level.SEVERE,
						"Failed to process fault: " + e.getMessage(), e);

			}
		}

		// TODO when the conter becomes equal to the number of invokes the
		// return the status for original InOnly
		Integer counter = originalInOnlyCounter.get(originalInOnly
				.getExchangeId());

		if (counter != 1) {
			counter = counter - 1;
			originalInOnlyCounter.put(originalInOnly.getExchangeId(), counter);

			return;
		} else {// this is the last response
			originalInOnlyCounter.remove(originalInOnly.getExchangeId());
		}

		// 5. Send X.done or X.error to A ========================
		if (this.previous == null) {
			AspectSEUtil.propogateTransaction(inOnly, originalInOnly);

			try {
				if (inOnly.getStatus().equals(ExchangeStatus.DONE)) {
					originalInOnly.setStatus(ExchangeStatus.DONE);
					originalInOnly.send();
					get_Status().incrementSentDones();
				} else {
					originalInOnly.setStatus(ExchangeStatus.ERROR);
					originalInOnly.send();
					get_Status().incrementSentErrors();
				}
			} catch (RuntimeException e1) {
				this.log().log(Level.SEVERE,
						"Failed to process fault: " + e1.getMessage(), e1);
			}

		} else {
			this.previous.handleStatus(inOnly, originalInOnly);
		}

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
