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
 * @(#)AspectSECacheEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.NormalizedMessage;
import javax.management.AttributeChangeNotification;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.exchange.InvokableExchange;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectSECacheEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.mbean.AspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.CacheAspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.CacheAspectSEEndpointConfigurationMbean;
import com.sun.jbi.engine.aspect.endpoint.support.Cache;
import com.sun.jbi.engine.aspect.endpoint.support.CacheSEUtil;
import com.sun.jbi.engine.aspect.endpoint.support.CacheStrategyType;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;

/**
 *
 *
 * <b>Cache Aspect</b>
 * <p>
 * There are a couple of patterns that the Cache aspect should support for
 * starters: 1. Write-Through Cache (a.k.a. Transparent Cache) pattern 2.
 * Cache-Aside pattern Write-Through Cache (a.k.a. Transparent Cache) pattern
 * This pattern ensures the Cache is placed between the client and the service
 * that is being invoked. This makes sure all messages flowing between the
 * client and the service provider flows through the cache.
 * <p>
 * This is the default Cache pattern that we will use. Composite Apps requiring
 * use of this aspect will have to employ the <b>Filter Request/Reply</b>
 * exchange pattern.
 * <p>
 * In a request/reply message exchange, the request message from the client is
 * sent to the service with the Cache in the middle. The reply message to the
 * client is cached. Subsequently, when the same request message is sent out by
 * the client, the reply message is provided from the Cache instead of having to
 * invoke the actual service. Cache-Aside pattern This pattern allows the
 * Composite App developer to manage caching of certain key elements and their
 * values contained within the messages that flow through the write-through
 * cache and use it in building features into his composite app. Composite Apps
 * requiring use of this aspect will have to employ the regular Request/Reply
 * exchange pattern on the cache and make use of its get/put operations.
 *
 * <p>
 * One scenario the user can use the Cache Aside pattern in his composite app is
 * as follows:
 *
 * <p>
 * <li> 1. Check the cache before invoking a service by providing a list of
 * XPath keyExpression(s).
 *
 * <li> 2. Get the value(s) directly from the cache and use them in the
 * composite app.
 * <li>3. Optionally, if the values are not cached, invoke the actual service,
 * and optionally, put the key/values into the cache.
 * <li>4. Optionally, evict or optionally, update the cache with new values.
 *
 * <p>
 * Cache Aside Management configuration
 *
 * <p>
 * The above mentioned scenarios would require that the user provide a list of
 * XPath keyExpression(s) (present in the message) to cache. This can be
 * provided through the Web Console which would have the ability to manage the
 * key expressions. Typical operations include:
 *
 * <li> 1. Get all Key Expressions to Cache
 * <li>2. Edit Key Expression
 * <li>3. Remove Key Expression
 * <li>4. Edit value.
 *
 *
 * @author Sujit Biswas
 *
 */
public class AspectSECacheEndpoint extends AspectSEEndpoint {

	private Cache<String, DOMResult> cacheMap = new Cache<String, DOMResult>();

	static final String CACHING_STRATEGY_KEY = "CachingStrategy";

	static final String MAXIMUM_ENTRIES_KEY = "MaximumEntries";

	private String maximumEntries = "100";

	private String cachingStrategy = CacheStrategyType.FirstInFirstOut
			.getCachingStrategy();

	public AspectSECacheEndpoint(EndpointInfo info) {
		super(info);
		setHandler(new AspectSECacheEndpointHandler(this));
		// TODO set handler for caching end point
	}

	@Override
	public void init() {
		getHandler().parse();
	}

	public void registerMbean() {
		try {
			AspectSEEndpointConfiguration mbean = new CacheAspectSEEndpointConfiguration(maximumEntries,cachingStrategy);
			mbean.addNotificationListener(this, null, null);
			super.registerMbean(mbean, "Cache");
		} catch (NotCompliantMBeanException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
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

			processedContent = CacheSEUtil
					.cacheIt(content, this, ctx, cacheMap);

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
			DOMSource newDomSource = CacheSEUtil.convert(processedContent, ctx);
			// /////////////////////
			DOMResult result = CacheSEUtil.cacheGetAsDOMResult(cacheMap,
					newDomSource);

			// /////////////////////////////////
			// Cache the exchangeID and source
			// /////////////////////////////////
			CacheSEUtil.cachePut(cacheMap, inOut.getExchangeId(), newDomSource);
			// /////////////////////////////////

			if (result != null) {
				this.sendReply(inOut, ctx, this, result);
				// /////////////////////
			} else {
				next.handleFilterInvoke(inOut, ctx);
			}
			return;
		}
		// 3. Send InOut Y to C ========================================
		AspectSEEndpoint output = getInvoke();
		if (output != null) {
			DOMSource newDomSource = CacheSEUtil.convert(processedContent, ctx);
			// /////////////////////
			DOMResult result = CacheSEUtil.cacheGetAsDOMResult(cacheMap,
					newDomSource);
			if (result != null) {
				this.sendReply(inOut, ctx, this, result);
				// /////////////////////
			} else {
				InvokableExchange newInOut = ctx.getExchangeFactory()
						.createInvokableExchange(ExchangePattern.IN_OUT,
								output.getInfo());
				AspectSEUtil.propogateTransaction(inOut, newInOut);
				String exchangeId = newInOut.getExchangeId();
				ctx.getCorrelationMap().correlate(exchangeId, inOut);

				// /////////////////////////////////
				// Cache the exchangeID and source
				// /////////////////////////////////
				CacheSEUtil.cachePut(cacheMap, inOut.getExchangeId(),
						newDomSource);
				// /////////////////////////////////

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

	@Override
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

				processedContent = CacheSEUtil.cacheIt(content, this, ctx,
						cacheMap);

				// /////////////////////////////////
				// Retrieve source with exchangeID
				// Cache source and result
				// /////////////////////////////////
				String exchangeId = originalInOut.getExchangeId();
				DOMSource key = CacheSEUtil.cacheRemoveAsDOMSource(cacheMap,
						exchangeId);
				DOMResult processedDOMResult = CacheSEUtil.convertToDOMResult(
						processedContent, ctx);
				CacheSEUtil.cachePut(cacheMap, key, processedDOMResult);
				// /////////////////////////////////

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

	// REQUEST_REPLY_SERVICE
	// A ---> (1. inOut X) --> B (2. Transform)
	// A <--- (3. X.out) <---- B
	// A ----> (4. X.done) ---> B
	@Override
	public void handleRequest(CRLInOut inOut, ExchangeContext ctx)
			throws JBIException {

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

			processedContent = CacheSEUtil
					.cacheIt(content, this, ctx, cacheMap);

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

	public boolean isCacheJBI() {
		// TODO Auto-generated method stub
		return true;
	}

	public void handleNotification(Notification notification, Object obj) {
		log().log(Level.INFO, "AspectSECacheEndpoint.Handling_notification");
		String attributeName = null;
		AttributeChangeNotification attributeNotification = null;
		String attributeValue = null;
		if (notification instanceof AttributeChangeNotification) {
			attributeNotification = (AttributeChangeNotification) notification;

			// Check if configuration change is for Cache SE component
			if (attributeNotification.getSource() instanceof CacheAspectSEEndpointConfigurationMbean) {
				attributeName = attributeNotification.getAttributeName();
				attributeValue = (String) attributeNotification.getNewValue();
				if (true == attributeName.equals(CACHING_STRATEGY_KEY)) {
					setCachingStrategy(attributeValue);
				}
				if (true == attributeName.equals(MAXIMUM_ENTRIES_KEY)) {
					setMaximumEntries(attributeValue);

				}
				// persist the changes to the corresponding config file
				save();
			}

		}
	}

	/**
	 * @return the cachingStrategy
	 */
	public String getCachingStrategy() {
		return cachingStrategy;
	}

	/**
	 * @param cachingStrategy
	 *            the cachingStrategy to set
	 */
	public void setCachingStrategy(String cachingStrategy) {

		if (true == cachingStrategy.equals(CacheStrategyType.GenericCache
				.getCachingStrategy())) {
			cacheMap.setGenericCachingStrategy();
		} else {
			int maximumEntries = cacheMap.getMaximumEntries();
			cacheMap.setFirstInFirstOutCachingStrategy(maximumEntries);
		}

		this.cachingStrategy = cacheMap.getCachingStrategy()
				.getCachingStrategy();
	}

	/**
	 * @return the maximumEntries
	 */
	public String getMaximumEntries() {
		return maximumEntries;
	}

	/**
	 * @param maximumEntries
	 *            the maximumEntries to set
	 */
	public void setMaximumEntries(String maximumEntries) {
		this.maximumEntries = maximumEntries;
		Integer sizeObject = new Integer(maximumEntries);
		cacheMap.setMaximumEntries(sizeObject.intValue());
	}

}
