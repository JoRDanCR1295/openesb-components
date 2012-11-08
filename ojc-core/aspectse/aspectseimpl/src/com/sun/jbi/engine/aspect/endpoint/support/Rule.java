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
 * @(#)Rule.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPathExpression;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;

/**
 * This routing rule is used for Content Based Routing. If the xpath expression
 * matches the input from the MessageExchange, the invoke specified by this rule
 * would be used for destination
 * 
 * @author Sujit Biswas
 * 
 */


public class Rule {

	private Element rule;

	private String ruleName;

	private KeyExpression keyExpression;

	private List<AspectSEEndpoint> destinations;

	public Rule(Element ruleElement, List<AspectSEEndpoint> dest) {
		rule = ruleElement;
		destinations = dest;
		init();
	}

	private void init() {
		if (rule != null) {
			ruleName = rule.getAttribute(AspectConstants.PROPERTY_ATTR_NAME);
			NodeList xpathElements = rule
					.getElementsByTagName(AspectConstants.XPATH_TAG);

			// expecting only one xpath tag with key expression attribute.
			Element xpathElement = (Element) xpathElements.item(0);
			if (xpathElement != null) {
				keyExpression = new KeyExpression(xpathElement);
			}
		}
	}

	/**
	 * This is the getter method for getting list of destination for this rule.
	 * This list can be empty.
	 * 
	 * @return List<String> destinations
	 */
	public List<AspectSEEndpoint> evaluate(DOMSource source) {
		if (keyExpression.evaluate(source)) {
			return destinations;
		}
		return new ArrayList<AspectSEEndpoint>();
	}

	/**
	 * This is the getter method for getting xpathexpression for this rule
	 * object.
	 * 
	 * @return XPathExpression xpathExpression
	 */
	public XPathExpression getXPathExpression() {
		return keyExpression.getXPathExpression();
	}

	/**
	 * This is the getter method for getting the Rule name.
	 * 
	 * @return String name
	 */
	public String getName() {
		return ruleName;
	}

	/**
	 * This is the setter method for setting the Rule name.
	 * 
	 * @param String
	 *            name
	 */
	public void setName(String name) {
		ruleName = name;
	}

	/**
	 * This is the setter method for setting xpath expression for this rule
	 * object.
	 * 
	 * @param XPathExpression
	 *            exp
	 */
	public void setXPathExpression(XPathExpression exp) {
		keyExpression = new KeyExpression(exp);
	}

	public String toXMLString() {
		StringBuffer xmlString = new StringBuffer();
		xmlString.append("<" + AspectConstants.RULE_TAG + ">\n");
		if (keyExpression == null) {
			xmlString.append("<" + AspectConstants.XPATH_TAG + "/>");
		} else {
			xmlString.append(keyExpression.toXMLString());
		}
		if (destinations.isEmpty()) {
			xmlString.append("<" + AspectConstants.RULE_TAG_DESTINATION
					+ "/>\n");
		} else {
			Iterator it = destinations.iterator();
			while (it.hasNext()) {
				String destination = (String) it.next();
				xmlString.append("<" + AspectConstants.RULE_TAG_DESTINATION
						+ " " + AspectConstants.RULE_TAG_DESTINATION_ATTR
						+ "=\"" + destination + "\"/>\n");
			}
		}
		xmlString.append("</" + AspectConstants.RULE_TAG + ">\n");
		return xmlString.toString();
	}
}
