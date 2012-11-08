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
 * @(#)KeyExpression.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.support;

import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;


/**
 * This class represents a key expression.
 *
 * @author karthikeyan s
 */


//TODO support namespaceContext, variableResolver , functionResolver

public class KeyExpression {
    
    String keyExpression;
    
    private XPathExpression xpathExpression;
    
    private XPathFactory xpfactory;
    
    private XPath path;
    
    /** Creates a new instance of KeyExpression */
    public KeyExpression(Element exp) {
        init(exp);
    }
    
    public KeyExpression(XPathExpression exp) {
    	xpathExpression=exp;
	}

	private void init(Element keyElement) {
        keyExpression = keyElement.getAttribute(AspectConstants.XPATH_ATTR_KEY_EXPRESSION);
        if(keyExpression == null || "".equals(keyExpression)) {
            // default the keyExpression to something.
            keyExpression = "//*";
        }
        xpfactory = XPathFactory.newInstance();
        path = xpfactory.newXPath();
        try {
            xpathExpression = path.compile(keyExpression);
        } catch (XPathExpressionException e) {
            // TODO Auto-generated catch block
            //ignore
        }
    }
    
    public boolean evaluate(DOMSource source) {
        try {
            Node src = source.getNode();
            NodeList nl = (NodeList) getXPathExpression().evaluate(src, XPathConstants.NODESET);
            if (nl.getLength() > 0) {
                return true;
            }
        } catch (XPathExpressionException e) {
            // TODO Auto-generated catch block
            //ignore
        }
        return false;
    }
    
    public void setKeyExpression(String exp) {
        path = xpfactory.newXPath();
        try {
            xpathExpression = path.compile(exp);
        } catch(XPathExpressionException e) {
            //ignore
        }
    }
    
    public String getKeyExpression() {
        return keyExpression;
    }
    
    public void setXPathExpression(XPathExpression expression) {
        xpathExpression = expression;
    }
    
    public XPathExpression getXPathExpression() {
        return xpathExpression;
    }
    
    public String toXMLString() {
        StringBuffer xmlString = new StringBuffer();
        if(xpathExpression == null || keyExpression == null) {
            return "<" + AspectConstants.XPATH_TAG +"/>\n";
        }
        xmlString.append("<" + AspectConstants.XPATH_TAG + " " +
                AspectConstants.XPATH_ATTR_KEY_EXPRESSION +"=\""+keyExpression+"\" />\n");
        return xmlString.toString();
    }
}
