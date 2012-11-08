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
 * @(#)AspectSEEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.management.StandardMBean;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.endpoint.impl.AbstractEndpoint;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.exchange.InvokableExchange;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.support.Ruleset;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;

/**
 * @author Sujit Biswas
 *
 */
public abstract class AspectSEEndpoint extends AbstractEndpoint implements
		NotificationListener {
	public enum EntryType {
		REQUEST_REPLY, FILTER_ONE_WAY, FILTER_REQUEST_REPLY
	};

	public enum PatternType {
		LOGGING, CONTENT_BASED_FILTER, TEE, CACHE, THROTTLING, QUEUING, MESSAGING
	};

	private static TransformerFactory mTransformerFactory = TransformerFactory
			.newInstance();

	private transient Logger mLog = null;

	private Templates mTemplates = null;

	private QName mMessageType = null;

	private QName mOperation = null;

	private AspectSEEndpoint mInvoke = null;

	private EntryType mEntryType = null;

	private PatternType patternType = null;

	private Element advice = null;

	protected AspectSEEndpoint next;

	protected AspectSEEndpoint previous;

	protected Ruleset ruleset;

	protected String mServiceUnitName = null;

	protected List<AspectSEEndpoint> invokes = new ArrayList<AspectSEEndpoint>();

	/**
	 * this element is either of type filterRequestReply or filterOneWay or
	 * requestReplyService
	 */
	private Element root = null;

	private AspectSEEndpointHandler handler;

	private String rootPath;

	private ComponentContext componentContext;

	// this id should be set while parsing the aspect root element
	private String id = "0";

	public AspectSEEndpoint(EndpointInfo info) {
		super(info);
	}

	protected Logger log() {
		if (mLog == null)
			mLog = Logger.getLogger(this.getClass().getName());
		return mLog;
	}

	/**
	 * the sub class should parse the rootElement and the adviceElement to
	 * initate itself
	 *
	 */
	public abstract void init();

	public EntryType getEntryType() {
		return mEntryType;
	}

	public void setEntryType(EntryType type) {
		mEntryType = type;
	}

	public AspectSEEndpoint getInvoke() {
		return mInvoke;
	}

	public void setInvoke(AspectSEEndpoint invoke) {
		mInvoke = invoke;
		if (!invokes.contains(invoke))
			invokes.add(invoke);
	}

	public QName getMessageType() {
		return mMessageType;
	}

	public void setMessageType(QName messageType) {
		mMessageType = messageType;
	}

	public QName getOperation() {
		return mOperation;
	}

	public void setOperation(QName opName) {
		mOperation = opName;
	}

	public Transformer getTransformer()
			throws TransformerConfigurationException {
		if (mTemplates == null) {
			synchronized (mTransformerFactory) {
				return mTransformerFactory.newTransformer();
			}
		}

		return mTemplates.newTransformer();
	}

	public void setTemplates(Templates templates) {
		mTemplates = templates;
	}

	public PatternType getPatternType() {
		return patternType;
	}

	public void setPatternType(PatternType patternType) {
		this.patternType = patternType;
	}

	/**
	 * This is called by the intermediate end point i.e InOutProvider, which can
	 * invoke one or more services once it receives the inOut message
	 *
	 *
	 * @param inOut
	 * @param ctx
	 * @throws JBIException
	 */
	public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {

		// FILTER_REQUEST_REPLY:
		// A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
		// B <----------------- (4. Y.out ) <-- C
		// B -----------------> (5. Y.done ) <-- C
		// A <--- (7. X.out) <---- B (6. Transform Y)
		// A ---> (8. X.done) ---> B

		// 8. Receive X.done from A
		if (!inOut.getStatus().equals(ExchangeStatus.ACTIVE)) {
			get_Status().incrementReceivedDones();
			return;
		}

		// 1. Receive inOut X from A
		NormalizedMessage xIn = inOut.getInMessage();
		if (xIn == null) {
			inOut
					.sendError(new JBIException(
							"Cannot process NULL in message!"));
			return;
		}

		get_Status().incrementReceivedRequests();
		// 2. Transform X ==============================================
		Source processedContent = null;
		try {
			Source content = xIn.getContent();
			if (log().isLoggable(Level.FINE)) {
				log().fine(
						"filterRequestReply-input: "
								+ AspectSEUtil.print(content));
			}

			// TODO process the content of the incoming message
			processedContent = content;

			if (log().isLoggable(Level.FINE)) {
				log().fine(
						"filterRequestReply-output: "
								+ AspectSEUtil.print(processedContent));
			}
		} catch (Exception transformEx) {
			inOut.sendError(transformEx);
			get_Status().incrementSentErrors();
			return;
		}

		// handover the inOut to the next aspect;
		if (next != null) {
			next.handleFilterInvoke(inOut, ctx);
			return;
		}
		// 3. Send InOut Y to C ========================================
		AspectSEEndpoint output = getInvoke();
		if (output != null) {
			DOMSource newDomSource = (DOMSource) processedContent;// TODO
			// create a
			// new
			// DOMSource
			// from
			// processedContent

			DOMResult result = null;
			if (false) { // if do not want to forward the message like in
				// caching just send the reply back
				sendReply(inOut, ctx, this, result);
			} else {
				InvokableExchange newInOut = ctx.getExchangeFactory()
						.createInvokableExchange(ExchangePattern.IN_OUT,
								output.getInfo());
				AspectSEUtil.propogateTransaction(inOut, newInOut);
				String exchangeId = newInOut.getExchangeId();
				ctx.getCorrelationMap().correlate(exchangeId, inOut);

				// TODO can make use of the exchange ID and source

				newInOut.invoke(newDomSource, output.getOperation());
				output.get_Status().incrementSentRequests();
			}
		} else {
			log().warning(
					"Service Type cannot be invoked - Skipping message invoke "
							+ inOut.getExchangeId() + " for Service QName: "
							+ output.getInfo().getServiceName()
							+ " Endpoint Name: "
							+ output.getInfo().getEndpointName());
			return;
		}
	}

	/**
	 *
	 * this is called by the InOut Service provider, The inout service provider
	 * in this case no longer invoke any service but just sends a response back
	 * i.e the service is of type REQUEST_REPLY_SERVICE
	 *
	 * <p>
	 * A<----->B
	 *
	 * @param inOut
	 * @param ctx
	 * @throws JBIException
	 */
	public void handleRequest(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {

		// REQUEST_REPLY_SERVICE
		// A ---> (1. inOut X) --> B (2. Transform)
		// A <--- (3. X.out) <---- B
		// A ----> (4. X.done) ---> B

		if ((inOut == null) || (ctx == null) || (this == null)) {
			return;
		}
		// 4. Receive X.done from A
		if (inOut.getStatus().equals(ExchangeStatus.DONE)) {
			get_Status().incrementReceivedDones();
			return;
		}
		if (inOut.getStatus().equals(ExchangeStatus.ERROR)) {
			get_Status().incrementReceivedErrors();
			return;
		}

		// 1. Receive inOut X from A
		get_Status().incrementReceivedRequests();
		NormalizedMessage xIn = inOut.getInMessage();
		if (xIn == null) {
			inOut
					.sendError(new JBIException(
							"Cannot process NULL in message!"));
			return;
		}

		// 2. Transform X ==================================================
		Source processedContent = null;
		try {
			Source content = xIn.getContent();
			if (log().isLoggable(Level.FINE)) {
				log()
						.fine(
								"requestReply-input: "
										+ AspectSEUtil.print(content));
			}

			processedContent = content; // TODO note the processed content shd
			// match the expected output message
			// type

			if (log().isLoggable(Level.FINE)) {
				log().fine(
						"requestReply-output: "
								+ AspectSEUtil.print(processedContent));
			}
		} catch (Exception transformEx) {
			inOut.sendError(transformEx);
			get_Status().incrementSentErrors();
			return;
		}

		// 3. Send X.out to A ==============================================
		inOut.reply(processedContent);
		get_Status().incrementSentReplies();
	}

	/**
	 *
	 * this is invoked by the intermediate node(B) i.e InOut Consumer , when it
	 * receives a response message from node(C) and has to send the reply back
	 * to the initiating node (A)
	 *
	 * <p>
	 * A<----->B<------>C
	 *
	 * @param inOut
	 * @param originalInOut
	 * @param ctx
	 * @throws JBIException
	 */

	public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
			ExchangeContext ctx) throws JBIException {
		try {

			// FILTER_REQUEST_REPLY:
			// A ---> (1. inOut X) --> B (2. Transform X) --> (3. inOut Y) --> C
			// B <----------------- (4. Y.out ) <-- C
			// B -----------------> (5. Y.done ) <-- C
			// A <--- (7. X.out) <---- B (6. Transform Y)
			// A ---> (8. X.done) ---> B

			// 4. Receive Y.out from C
			NormalizedMessage yOut = inOut.getOutMessage();
			if (yOut == null) {

				if (next == null) {
					inOut.sendError(new JBIException(
							"Cannot process NULL out message!"));
				}
				if (previous == null) {
					originalInOut.sendError(new JBIException(
							"Received NULL response!"));
					return;
				}
				if (next != null && previous != null) {
					previous.handleFilterResponse(inOut, originalInOut, ctx);
					return;
				}
			}

			if (next == null) {
				// propogate transaction regardless of status
				AspectSEUtil.propogateTransaction(inOut, originalInOut);

				AspectSEEndpoint output = getInvoke(); // C
				output.get_Status().incrementReceivedReplies();
				// 5. Send Y.done or Y.error to C ==============================
				inOut.setStatus(ExchangeStatus.DONE);
				inOut.send();
				output.get_Status().incrementSentDones();
			}

			// 6. Transform Y ==============================================
			Source content = yOut.getContent(), processedContent = null;
			try {
				if (log().isLoggable(Level.FINE)) {
					log().fine(
							"filterRequestReply-response: "
									+ AspectSEUtil.print(content));
				}

				/*
				 * TODO process the consumed message
				 */
				processedContent = content;

				if (log().isLoggable(Level.FINE)) {
					log().fine(
							"filterRequestReply-transformed response: "
									+ AspectSEUtil.print(processedContent));
				}
			} catch (Exception transformEx) {
				log().severe(
						"Transformation failed: " + transformEx.getMessage());

				// TODO handle application exception in an intermediate node;
				if (previous == null) {
					originalInOut.sendError(transformEx);
					get_Status().incrementSentErrors();
				}
			}

			if (previous != null) {
				previous.handleFilterResponse(inOut, originalInOut, ctx);
				return;
			}
			// 7. Send X.out to A ==========================================
			originalInOut.reply(processedContent);
			get_Status().incrementSentReplies();
		} catch (JBIException e) {
			log().log(Level.SEVERE,
					"Consuming InOut failed: " + e.getMessage(), e);
			originalInOut.sendError(e);
			if (this != null)
				get_Status().incrementSentErrors();
		}
	}

	/**
	 * handles inOnly message, this is called by the InOnlyProvider(B), which
	 * may need to invoke more than one service
	 *
	 *
	 * <p>
	 * A----->B------>C
	 *
	 *
	 * @param inOnly
	 * @param ctx
	 * @throws JBIException
	 */

	public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
			throws JBIException {
		try {
			AspectSEEndpoint output = getInvoke();

			// 1. Receive inOnly X from A
			get_Status().incrementReceivedRequests();
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
			newInOnly.invoke(processedContent, output.getOperation());
			output.get_Status().incrementSentRequests();
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

	protected void sendReply(CRLInOut originalInOut, ExchangeContext ctx,
			AspectSEEndpoint input, DOMResult processedContent)
			throws JBIException {

		Document document = null;
		DOMSource returnValue = null;
		try {
			returnValue = new DOMSource(ctx.newDocument());
			if ((processedContent != null)
					&& (processedContent.getNode() != null)) {
				if (processedContent.getNode() instanceof Document) {
					document = (Document) processedContent.getNode();
					if (document != null) {
						returnValue = new DOMSource(document);
					}
				} else {
					returnValue.setNode((Node) processedContent.getNode());
				}
			}
			originalInOut.reply(returnValue);
			input.get_Status().incrementSentReplies();
		} catch (JBIException e) {
			log().log(Level.SEVERE,
					"Consuming InOut failed: " + e.getMessage(), e);
			originalInOut.sendError(e);
			if (input != null)
				input.get_Status().incrementSentErrors();
		}
	}

	public Element getAdvice() {
		return advice;
	}

	public void setAdvice(Element advice) {
		this.advice = advice;
	}

	public void setRoot(Element root) {
		this.root = root;

	}

	public void setNext(AspectSEEndpoint nextEndpt) {
		next = nextEndpt;
	}

	public void setPrevious(AspectSEEndpoint previousEndpt) {
		previous = previousEndpt;
	}

	public AspectSEEndpoint getNext() {
		return next;
	}

	public AspectSEEndpoint getPrevious() {
		return previous;
	}

	public Element getRoot() {
		return root;
	}

	public AspectSEEndpointHandler getHandler() {
		return handler;
	}

	public void setHandler(AspectSEEndpointHandler handler) {
		this.handler = handler;
	}

	public String getSURootPath() {
		return rootPath;
	}

	public void setSURootPath(String rootPath) {
		this.rootPath = rootPath;
	}

	public void setComponentContext(ComponentContext componentContext) {
		this.componentContext = componentContext;

	}

	public abstract void registerMbean();

	@SuppressWarnings("unchecked")
	protected void registerMbean(StandardMBean mbean, String adviceName) {

		MBeanServer mbeanServer = componentContext.getMBeanServer();

		String objectNameString = getObjectNameString(adviceName);

		try {
			// TODO put back annotated mbean later on
			// AnnotatedStandardMBean config = new AnnotatedStandardMBean(mbean,
			// mbean.getMBeanInterface());

			ObjectName objectName = new ObjectName(objectNameString);

			if ((objectName != null)
					&& (false == mbeanServer.isRegistered(objectName))
					&& (mbean != null)) {
				mbeanServer.registerMBean(mbean, objectName);
				setObjectName(objectName);

			}

		} catch (Exception e) {
			mLog.info(e.getMessage());
		}
	}

	private ObjectName objectName;

	public void shutdown() throws InstanceNotFoundException,
			MBeanRegistrationException {
		MBeanServer mbeanServer = componentContext.getMBeanServer();
		if (getObjectName() != null
				&& mbeanServer.isRegistered(getObjectName())) {
			mbeanServer.unregisterMBean(getObjectName());
		}
	}

	private String getObjectNameString(String adviceName) {
		String componentType = RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE;
		String componentName = this.getComponentContext().getComponentName();
		String serviceUnitId = this.getServiceUnitName();

		String aspectID = "Aspect" + this.getId();
		String objectNameString = "com.sun.ebi:ServiceType=Configuration,InstallationType="
				+ componentType
				+ ",IdentificationName="
				+ componentName
				+ ",ServiceUnitID="
				+ serviceUnitId
				+ ",AspectID="
				+ aspectID
				+ ",AdviceName=" + adviceName;
		return objectNameString;
	}

	/**
	 * @return the componentContext
	 */
	public ComponentContext getComponentContext() {
		return componentContext;
	}

	/**
	 * @return the id
	 */
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * @return the objectName
	 */
	public ObjectName getObjectName() {
		return objectName;
	}

	/**
	 * @param objectName
	 *            the objectName to set
	 */
	public void setObjectName(ObjectName objectName) {
		this.objectName = objectName;
	}

	public String getServiceUnitName() {
		return mServiceUnitName;
	}

	public void setServiceUnitName(String serviceUnitName) {
		mServiceUnitName = serviceUnitName;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see javax.management.NotificationListener#handleNotification(javax.management.Notification,
	 *      java.lang.Object)
	 */
	public void handleNotification(Notification notification, Object handback) {
		// TODO Auto-generated method stub

	}

	public void setRuleset(Ruleset ruleSet) {
		ruleset = ruleSet;
	}

	public Ruleset getRuleset() {
		return ruleset;
	}

	public void save() {
		getHandler().save();
	}

	/**
	 *
	 * this is invoked by the intermediate node(B) i.e InOut Consumer , when it
	 * receives a fault message from node(C) and has to send the fault back to
	 * the initiating node (A)
	 *
	 * <p>
	 * A<----->B<------>C
	 *
	 * @param inOut
	 * @param originalInOut
	 * @throws JBIException
	 */
	public void handleFault(CRLInOut inOut, CRLInOut originalInOut)
			throws JBIException {

		// send DONE to consumed endpoint
		if (this.next == null) {
			AspectSEEndpoint output = null;
			try {
				output = this.getInvoke(); // C
				output.get_Status().incrementReceivedReplies();
				inOut.setStatus(ExchangeStatus.DONE);
				inOut.send();
				output.get_Status().incrementSentDones();
			} catch (RuntimeException e) {
				this.log().log(Level.SEVERE,
						"Failed to process fault: " + e.getMessage(), e);
				inOut.sendError(e);
				if (output != null)
					output.get_Status().incrementSentErrors();
			}
		}

		if (this.previous == null) {
			try {
				originalInOut.setFault(inOut.getFault());
				originalInOut.send();
				get_Status().incrementSentReplies();
			} catch (RuntimeException e1) {
				if (originalInOut != null) {
					// TODO should create a fault message
					originalInOut.sendError(e1);
					if (this != null)
						get_Status().incrementSentErrors();
				}
			}

		} else {
			this.previous.handleFault(inOut, originalInOut);
		}
	}

	EndpointStatus status;

	protected EndpointStatus get_Status() {
		// TODO Auto-generated method stub
		return status;
	}

	/**
	 *
	 * this is invoked by the intermediate node(B) i.e InOnly Consumer , when it
	 * receives a status message from node(C) and has to send the status back to
	 * the initiating node (A)
	 *
	 * <p>
	 * A------>B------->C
	 *
	 * @param inOnly
	 * @param originalInOnly
	 * @throws JBIException
	 */
	public void handleStatus(CRLInOnly inOnly, CRLInOnly originalInOnly)
			throws MessagingException, JBIException {

		// propogate transaction regardless of status

		if (this.next == null) {
			AspectSEEndpoint output = null;
			try {
				output = this.getInvoke(); // C
				if (inOnly.getStatus().equals(ExchangeStatus.DONE)) {
					output.get_Status().incrementReceivedDones();
				} else {
					output.get_Status().incrementReceivedErrors();
				}
			} catch (RuntimeException e) {
				this.log().log(Level.SEVERE,
						"Failed to process fault: " + e.getMessage(), e);

			}
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

	public void set_Status(EndpointStatus status) {
		this.status = status;

	}

	public List<AspectSEEndpoint> getInvokes() {
		return invokes;
	}

	public void addInvoke(AspectSEEndpoint invoke) {
		invokes.add(invoke);
	}
}
