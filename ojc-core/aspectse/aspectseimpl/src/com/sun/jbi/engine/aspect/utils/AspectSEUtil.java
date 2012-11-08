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
 * @(#)AspectSEUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.utils;

import java.io.File;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.factory.AspectMap;
import com.sun.jbi.engine.aspect.endpoint.factory.AspectSEEndpointFactory;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import com.sun.jbi.engine.aspect.endpoint.support.Rule;
import com.sun.jbi.engine.aspect.endpoint.support.Ruleset;

/**
 * @author Sujit Biswas
 * 
 */
public class AspectSEUtil {
	private static Logger logger = Logger.getLogger(AspectSEUtil.class
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
	private AspectSEUtil() {
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
			logger.info("Failed to print xml: " + e.getMessage());
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
			logger.info("Propogating transaction: " + String.valueOf(txProp));
			to.setProperty(InOnly.JTA_TRANSACTION_PROPERTY_NAME, txProp);
		}
	}

	public static String convert(QName qname) {
		StringBuffer buff = new StringBuffer();
		buff.append("{").append(qname.getNamespaceURI()).append("}").append(
				qname.getLocalPart());
		return buff.toString();
	}

	public static QName convert(String qname) {
		if (qname == null)
			return null;
		return QName.valueOf(qname);
	}

	public static String xmlString(Ruleset ruleset) {
		StringBuffer xmlString = new StringBuffer();

		// TODO add the ruleset name
		xmlString.append("<" + AspectConstants.RULESET_TAG + ">\n");
		if (ruleset.getRuleList().isEmpty()) {
			return "<" + AspectConstants.RULESET_TAG + " />\n";
		} else {
			Iterator it = ruleset.getRuleList().iterator();
			while (it.hasNext()) {
				Rule rule = (Rule) it.next();
				xmlString.append(rule.toXMLString());
			}
		}
		xmlString.append("</" + AspectConstants.RULESET_TAG + ">\n");
		return xmlString.toString();
	}

	public static AspectSEEndpoint getOutputForID(AspectSEEndpoint endpoint, String id) {
//		NodeList outputs = endpoint.getElementsByTagName(AspectMap.OUTPUT_ELEMENT);
//		for (int i = 0; i < outputs.getLength(); i++) {
//			Element output = (Element) outputs.item(i);
//			if (output.getAttribute(AspectMap.ID_ATTR).equalsIgnoreCase(id)) {
//				return output;
//			}
//		}
		
		for ( int i= 0; i< endpoint.getInvokes().size(); i++){
			AspectSEEndpoint output = endpoint.getInvokes().get(i);
			if(output.getId().equals(id)){
				return output;
			}
		}
		return null;
	}

	public static DeploymentException error(String message, Exception thrown) {
		if (thrown == null) {
			logger.severe(message);
			return new DeploymentException(message);
		} else {
			logger.log(Level.SEVERE, message, thrown);
			return new DeploymentException(message, thrown);
		}
	}

	private static TransformerFactory transformerFactory = TransformerFactory
			.newInstance();

	public static Templates getTemplates(String rootPath, String fileName) {
		if (fileName == null)
			return null;

		Templates ret = null;
		File file = new File(rootPath, fileName);
		StreamSource src = new StreamSource(file);
		try {
			synchronized (transformerFactory) {
				ret = transformerFactory.newTemplates(src);
			}

		} catch (TransformerConfigurationException e) {
			logger.log(Level.SEVERE, "Failed to create a new Transformer for "
					+ fileName, e);
		}
		return ret;
	}

}
