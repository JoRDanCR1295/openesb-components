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
 * @(#)CacheSEUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.io.StringWriter;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.engine.aspect.endpoint.AspectSECacheEndpoint;

import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

/**
 * Utility class for Cache service engine.
 * 
 * @author graj
 */
public class CacheSEUtil {

	private static boolean debug = false;

	private static Logger mLogger = Logger.getLogger(CacheSEUtil.class
			.getName());

	private static TransformerFactory mTransformerFactory = TransformerFactory
			.newInstance();

	private static ErrorListener mErrorListener = new ErrorListener() {
		public void warning(TransformerException exception)
				throws TransformerException {
			System.out.println("warning");
			exception.printStackTrace();
		}

		public void fatalError(TransformerException exception)
				throws TransformerException {
			System.out.println("fatal");
			exception.printStackTrace();
		}

		public void error(TransformerException exception)
				throws TransformerException {
			System.out.println("error");
			exception.printStackTrace();
		}
	};

	/* Not intended to be instantiated. */
	private CacheSEUtil() {
	}

	/**
	 * 
	 * @param source
	 * @param endpoint
	 * @param exchangeContext
	 * @param cacheMap
	 * @return
	 */
	public static Source cacheIt(Source source, AspectSECacheEndpoint endpoint,
			ExchangeContext exchangeContext, Map<String, DOMResult> cacheMap) {
		DOMSource returnValue = null, domSource = null;
		try {
			if (source instanceof DOMSource) {
				domSource = (DOMSource) source;
			} else {
				domSource = convert(source, exchangeContext);
			}

			if (endpoint.isCacheJBI()) {
				returnValue = cacheItAsIs(domSource, endpoint, exchangeContext,
						cacheMap);
			} else {
				// extract payload content of jbi:part element
				Element partContent = extractPartContent(domSource.getNode());
				if (partContent != null) {
					DOMResult result = new DOMResult(exchangeContext
							.newDocument());
					DOMSource payload = new DOMSource(exchangeContext
							.newDocument());
					payload.setNode(partContent);

					if (mLogger.isLoggable(Level.FINE)) {
						mLogger.fine("actual transform - " + print(payload));
					}
					// TODO: Remove later
					// result = cacheGetAsDOMResult(cacheMap, payload);

					if ((result != null) && (result.getNode() != null)) {
						// wrap transformed payload in WSDL 1.1 jbi:message
						Element plMsg = wrapCachedPayload(result, endpoint
								.getMessageType());
						returnValue = new DOMSource(plMsg);

						if (mLogger.isLoggable(Level.FINE)) {
							mLogger.fine("actual result - "
									+ print(returnValue));
						}
					} else {
						// Cannot find it in the cache
						DOMResult domResult = new DOMResult(exchangeContext
								.newDocument());
						domResult.setNode(partContent);

						// TODO: Remove later
						// cachePut(cacheMap, payload, domResult);

						returnValue = new DOMSource(exchangeContext
								.newDocument());
						if ((domResult != null)
								&& (domResult.getNode() != null)) {
							if (domResult.getNode() instanceof Document) {
								Document document = (Document) domResult
										.getNode();
								if (document != null) {
									returnValue = new DOMSource(document);
								}
							} else {
								returnValue.setNode((Node) domResult.getNode());
							}
						}
					}
				}
			}
		} catch (Exception e) {
			// TODO handle better
			mLogger.log(Level.SEVERE, "Cache transform failed", e);
		}

		return returnValue;
	}

	/**
	 * 
	 * @param source
	 * @param exchangeContext
	 * @return
	 */
	public static DOMSource convert(Source source,
			ExchangeContext exchangeContext) {
		try {
			Transformer transformer = null;
			synchronized (mTransformerFactory) {
				transformer = mTransformerFactory.newTransformer();
				transformer.setErrorListener(mErrorListener);
			}
			DOMResult result = new DOMResult(exchangeContext.newDocument());
			transformer.transform(source, result);
			return new DOMSource((Document) result.getNode());
		} catch (Exception e) {
			mLogger
					.warning("Failed to convert to DOMSource: "
							+ e.getMessage());
			return null;
		}
	}

	/**
	 * Formats the specified xml source and returns an xml string.
	 * 
	 * @param src
	 *            The xml source object.
	 * @return Formatted xml or an empty string if formatting fails.
	 */
	public static String print(Source src) {
		try {
			Transformer transformer = null;
			synchronized (mTransformerFactory) {
				transformer = mTransformerFactory.newTransformer();
				transformer.setErrorListener(mErrorListener);
			}

			StringWriter writer = new StringWriter();
			StreamResult dest = new StreamResult(writer);
			transformer.transform(src, dest);
			return writer.toString();
		} catch (Exception e) {
			mLogger.info("Failed to print xml: " + e.getMessage());
			return "";
		}
	}

	/**
	 * Propogates any transaction information from one message exchange to
	 * another.
	 * 
	 * @param from
	 *            the incoming message
	 * @param to
	 *            the outgoing message
	 */
	public static void propogateTransaction(MessageExchange from,
			MessageExchange to) {
		if (from != null && to != null && from.isTransacted()) {
			Object txProp = from
					.getProperty(InOnly.JTA_TRANSACTION_PROPERTY_NAME);
			mLogger.info("Propogating transaction: " + String.valueOf(txProp));
			to.setProperty(InOnly.JTA_TRANSACTION_PROPERTY_NAME, txProp);
		}
	}

	/**
	 * 
	 * @param domSource
	 * @param endpoint
	 * @param exchangeContext
	 * @param cacheMap
	 * @return
	 */
	private static DOMSource cacheItAsIs(DOMSource domSource,
			AspectSECacheEndpoint endpoint, ExchangeContext exchangeContext,
			Map<String, DOMResult> cacheMap) {
		DOMSource returnValue = null;
		try {

			DOMResult result = null;
			// TODO: Remove later
			// result = cacheGetAsDOMResult(cacheMap, domSource);

			if ((result != null) && (result.getNode() != null)) {
				returnValue = new DOMSource((Document) result.getNode());
			} else {
				DOMSource convertedSource = CacheSEUtil.convert(domSource,
						exchangeContext);
				result = new DOMResult((Document) convertedSource.getNode());
				// TODO: Remove later
				// cachePut(cacheMap, domSource, result);

				returnValue = new DOMSource((Document) result.getNode());
			}
		} catch (Exception e) {
			// TODO handle better
			mLogger.log(Level.SEVERE, "Caching failed", e);
		}
		return returnValue;
	}

	/**
	 * 
	 * @param node
	 * @return
	 */
	private static Element extractPartContent(Node node) {
		if (node instanceof Document) {
			return extractPartContent(((Document) node).getDocumentElement());
		}
		if (node instanceof Element) {
			Element msg = (Element) node;
			if (msg.getTagName().equals(WrapperUtil.WRAPPER_MESSAGE)) {
				// we have the jbi:message node, find first part (only 1
				// supported...)
				NodeList children = msg
						.getElementsByTagName(WrapperUtil.WRAPPER_PART);
				if (children.getLength() > 0) {
					// TODO this should return back the first child of the jbi
					// part element.
					Element part = (Element) children.item(0);
					// payload is expected to be first ELEMENT child
					NodeList partChildren = part.getElementsByTagName("*");
					if (partChildren.getLength() > 0) {
						return (Element) partChildren.item(0);
					}
				}
			}
		}
		return null;
	}

	/**
	 * 
	 * @param key
	 * @param value
	 * @param operation
	 */
	private static void dump(DOMSource key, DOMResult value, String operation) {

		if (!debug)
			return;

		String keyString = null;
		String valueString = null;
		DOMSource source = null;
		Document document = null;
		if (key != null) {
			keyString = print(key);
			System.out.println("/////////" + operation + "//////////////");
			System.out.println("Key: " + keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					source = new DOMSource(document);
					valueString = print(source);
				}
			} else {
				source = new DOMSource();
				source.setNode((Node) value.getNode());
				valueString = print(source);
			}
		}
		System.out.println("Value: " + valueString);
		System.out.println("///////////////////////////");

	}

	/**
	 * 
	 * @param keyString
	 * @param value
	 * @param operation
	 */
	private static void dump(String keyString, DOMResult value, String operation) {

		if (!debug)
			return;

		String valueString = null;
		DOMSource source = null;
		Document document = null;
		System.out.println("/////////" + operation + "//////////////");
		System.out.println("Key: " + keyString);
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					source = new DOMSource(document);
					valueString = print(source);
				}
			} else {
				source = new DOMSource();
				source.setNode((Node) value.getNode());
				valueString = print(source);
			}
		}
		System.out.println("Value: " + valueString);
		System.out.println("///////////////////////////");

	}

	/**
	 * 
	 * @param result
	 * @param resultMsgType
	 * @return
	 * @throws Exception
	 */
	private static Element wrapCachedPayload(DOMResult result,
			QName resultMsgType) throws Exception {
		Document doc = (Document) result.getNode();
		Element plPart = WrapperUtil.createJBIWrappedPart(doc, doc
				.getDocumentElement());
		Element plMsg = WrapperUtil.createJBIMessageWrapper(doc, resultMsgType,
				null);
		// append part and add type namespace decl
		plMsg.appendChild(plPart);

		return plMsg;
	}

	public DOMSource getAsDOMSource(String key) {
		DOMSource domSourceResult = null;

		return domSourceResult;
	}

	// //////////////////
	// Helper methods
	// //////////////////
	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param String
	 *            key
	 * 
	 * @result DOMResult value
	 */
	public static DOMResult cacheGetAsDOMResult(
			Map<String, DOMResult> cacheMap, String keyString) {
		DOMResult domResult = null;
		synchronized (cacheMap) {
			domResult = cacheMap.get(keyString);
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, domResult, "get");
		return domResult;
	}

	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param String
	 *            key
	 * 
	 * @result DOMSource value
	 */
	public static DOMSource cacheGetAsDOMSource(
			Map<String, DOMResult> cacheMap, String keyString) {
		DOMSource domSourceResult = null;
		DOMResult value = null;
		Document document = null;

		synchronized (cacheMap) {
			value = cacheMap.get(keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domSourceResult = new DOMSource(document);
				}
			} else {
				domSourceResult = new DOMSource();
				domSourceResult.setNode((Node) value.getNode());
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "get");

		return domSourceResult;
	}

	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param String
	 *            key
	 * 
	 * @result String value
	 */
	public static String cacheGetAsString(Map<String, DOMResult> cacheMap,
			String keyString) {
		DOMSource domSourceResult = null;
		DOMResult value = null;
		Document document = null;
		String valueString = null;
		synchronized (cacheMap) {
			value = cacheMap.get(keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domSourceResult = new DOMSource(document);
					valueString = print(domSourceResult);
				}
			} else {
				domSourceResult = new DOMSource();
				domSourceResult.setNode((Node) value.getNode());
				valueString = print(domSourceResult);
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "get");

		return valueString;
	}

	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param DOMSource
	 *            key
	 * 
	 * @result DOMResult value
	 */
	public static DOMResult cacheGetAsDOMResult(
			Map<String, DOMResult> cacheMap, DOMSource keyDOMSource) {
		DOMResult value = null;
		String keyString = print(keyDOMSource);
		synchronized (cacheMap) {
			value = cacheMap.get(keyString);
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "get");
		return value;
	}

	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param DOMSource
	 *            key
	 * 
	 * @result DOMSource value
	 */
	public static DOMSource cacheGetAsDOMSource(
			Map<String, DOMResult> cacheMap, DOMSource keyDOMSource) {
		DOMSource domSourceResult = null;
		DOMResult value = null;
		Document document = null;
		String keyString = print(keyDOMSource);
		synchronized (cacheMap) {
			value = cacheMap.get(keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domSourceResult = new DOMSource(document);
				}
			} else {
				domSourceResult = new DOMSource();
				domSourceResult.setNode((Node) value.getNode());
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "get");

		return domSourceResult;
	}

	/**
	 * @param Map
	 *            <String, DOMResult>
	 * @param DOMSource
	 *            key
	 * 
	 * @result String value
	 */
	public static String cacheGetAsString(Map<String, DOMResult> cacheMap,
			DOMSource keyDOMSource) {
		DOMSource domSourceResult = null;
		DOMResult value = null;
		Document document = null;
		String valueString = null;
		String keyString = print(keyDOMSource);
		synchronized (cacheMap) {
			value = cacheMap.get(keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domSourceResult = new DOMSource(document);
					valueString = print(domSourceResult);
				}
			} else {
				domSourceResult = new DOMSource();
				domSourceResult.setNode((Node) value.getNode());
				valueString = print(domSourceResult);
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "get");
		return valueString;
	}

	// ///////////
	// Put Helper methods
	// ///////////

	/**
	 * 
	 * @param cacheMap
	 * @param keyString
	 * @param value
	 * @return
	 */
	public static DOMResult cachePut(Map<String, DOMResult> cacheMap,
			String keyString, DOMResult value) {
		if ((keyString == null) || (keyString.length() <= 0) || (value == null)
				|| (value.getNode() == null)) {
			return null;
		}
		if ((keyString != null) && (keyString.length() > 0) && (value != null)
				&& (value.getNode() != null)) {
			synchronized (cacheMap) {
				cacheMap.put(keyString, value);
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "put");
		return value;
	}

	/**
	 * 
	 * @param cacheMap
	 * @param keyString
	 * @param value
	 * @return
	 */
	public static DOMResult cachePut(Map<String, DOMResult> cacheMap,
			String keyString, DOMSource value) {
		if ((keyString == null) || (keyString.length() <= 0) || (value == null)
				|| (value.getNode() == null)) {
			return null;
		}
		Document document = null;
		DOMResult domResult = null;
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domResult = new DOMResult(document);
				}
			} else {
				domResult = new DOMResult();
				domResult.setNode((Node) value.getNode());
			}
		}
		if ((keyString != null) && (keyString.length() > 0)
				&& (domResult != null) && (domResult.getNode() != null)) {
			synchronized (cacheMap) {
				cacheMap.put(keyString, domResult);

			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, domResult, "put");
		return domResult;
	}

	/**
	 * 
	 * @param cacheMap
	 * @param key
	 * @param value
	 * @return
	 */
	public static DOMResult cachePut(Map<String, DOMResult> cacheMap,
			DOMSource key, DOMResult value) {
		if ((key == null) || (key.getNode() == null) || (value == null)
				|| (value.getNode() == null)) {
			return null;
		}
		String keyString = print(key);
		synchronized (cacheMap) {
			cacheMap.put(keyString, value);
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, value, "put");
		return value;
	}

	/**
	 * 
	 * @param cacheMap
	 * @param key
	 * @param value
	 * @return
	 */
	public static DOMResult cachePut(Map<String, DOMResult> cacheMap,
			DOMSource key, DOMSource value) {
		if ((key == null) || (key.getNode() == null) || (value == null)
				|| (value.getNode() == null)) {
			return null;
		}
		String keyString = print(key);
		Document document = null;
		DOMResult domResult = null;
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					domResult = new DOMResult(document);
				}
			} else {
				domResult = new DOMResult();
				domResult.setNode((Node) value.getNode());
			}
			synchronized (cacheMap) {
				cacheMap.put(keyString, domResult);
			}
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, domResult, "put");
		return domResult;
	}

	/**
	 * 
	 * @param source
	 * @param exchangeContext
	 * @return DOMSource
	 */
	public static DOMSource convertToDOMSource(Source source,
			ExchangeContext exchangeContext) {
		DOMSource returnValue = null;
		if (source instanceof DOMSource) {
			returnValue = (DOMSource) source;
		} else {
			returnValue = convert(source, exchangeContext);
		}
		return returnValue;
	}

	/**
	 * 
	 * @param source
	 * @param exchangeContext
	 * @return DOMResult
	 */
	public static DOMResult convertToDOMResult(Source source,
			ExchangeContext exchangeContext) {
		try {
			Transformer transformer = null;
			synchronized (mTransformerFactory) {
				transformer = mTransformerFactory.newTransformer();
				transformer.setErrorListener(mErrorListener);
			}
			DOMResult result = new DOMResult(exchangeContext.newDocument());
			transformer.transform(source, result);
			return result;
		} catch (Exception e) {
			mLogger
					.warning("Failed to convert to DOMSource: "
							+ e.getMessage());
			return null;
		}

	}

	// //////////////////////
	// Remove Helper methods
	// //////////////////////

	/**
	 * 
	 * @param cacheMap
	 * @param keyString
	 * @return
	 */
	public static DOMResult cacheRemove(Map<String, DOMResult> cacheMap,
			String keyString) {
		if ((keyString == null) || (keyString.length() <= 0)) {
			return null;
		}
		DOMResult domResult = null;
		synchronized (cacheMap) {
			domResult = cacheMap.remove(keyString);
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, domResult, "remove");
		return domResult;
	}

	/**
	 * 
	 * @param cacheMap
	 * @param keyString
	 * @return
	 */
	public static DOMSource cacheRemoveAsDOMSource(
			Map<String, DOMResult> cacheMap, String keyString) {
		if ((keyString == null) || (keyString.length() <= 0)) {
			return null;
		}
		Document document = null;
		DOMResult value = null;
		DOMSource resultAsDOMSource = null;
		synchronized (cacheMap) {
			value = cacheMap.remove(keyString);
		}
		if (value != null) {
			if (value.getNode() instanceof Document) {
				document = (Document) value.getNode();
				if (document != null) {
					resultAsDOMSource = new DOMSource(document);
				}
			} else {
				resultAsDOMSource = new DOMSource();
				resultAsDOMSource.setNode((Node) value.getNode());
			}
		}

		// TODO: Remove - placed here for debugging
		dump(keyString, value, "remove");
		return resultAsDOMSource;
	}

	/**
	 * 
	 * @param cacheMap
	 * @param key
	 * @return
	 */
	public static DOMResult cacheRemove(Map<String, DOMResult> cacheMap,
			DOMSource key) {
		if ((key == null) || (key.getNode() == null)) {
			return null;
		}
		String keyString = print(key);
		DOMResult domResult = null;
		synchronized (cacheMap) {
			domResult = cacheMap.remove(keyString);
		}
		// TODO: Remove - placed here for debugging
		dump(keyString, domResult, "remove");
		return domResult;
	}

}
