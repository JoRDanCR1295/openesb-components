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
 * @(#)AbstractJbiParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.descriptor.EndpointInfo.LinkType;
import com.sun.jbi.common.util.Util;

/**
 * Base class for JBI descriptor parsers. 
 * @author Kevan Simpson
 */
public abstract class AbstractJbiParser<T> implements JbiParser<T>{
	private static XPathFactory mFactory = XPathFactory.newInstance();
	
	private XPath mXPath;
	private JbiNamespaceContext mNSContext;
	private Logger mLogger;

	/**
	 * Constructs a <code>JbiParser</code>.
	 */
	protected AbstractJbiParser() {
		this(null, null);
	}
	
	/**
	 * Constructs a <code>JbiParser</code> using the specified parser's
	 * {@link XPath} resource and {@link JbiNamespaceContext}.
	 * @param parser A <code>JbiParser</code>, which may not be <code>null</code>.
	 */
	protected AbstractJbiParser(JbiParser parser) {
		this(parser.getXPath(), parser.getNSContext());
	}
	
	/**
	 * Constructs a <code>JbiParser</code> with an {@link XPath} resource and
	 * a {@link JbiNamespaceContext}.
	 * @param xpath An <code>XPath</code> resource.
	 * @param ctx A <code>JbiNamepaceContext</code>.
	 */
	protected AbstractJbiParser(XPath xpath, JbiNamespaceContext ctx) {
		mNSContext = (ctx == null) ? new JbiNamespaceContext() : ctx;
		if (xpath != null) { 
		    mXPath = xpath;
		    mXPath.setNamespaceContext(mNSContext);
		}
		mLogger = Logger.getLogger(this.getClass().getName());
	}

	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#getNSContext() */
	public JbiNamespaceContext getNSContext() {
		return mNSContext;
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#getXPath() */
	public XPath getXPath() {
	    if (mXPath == null) {  // lazy load the XPath resource
	        mXPath = mFactory.newXPath();
	        mXPath.setNamespaceContext(getNSContext());
	    }
		return mXPath;
	}

	/**
	 * Builds an XPath expression by concatenating the specified strings,
	 * replacing <code>null</code> with {@link JbiNamespaceContext#JBI_PREFIX}
	 * appended with a colon.
	 * 
	 * @param expr One or more XPath expression fragments.
	 * @return An XPath expression or the emtpy string if no parameters were passed.
	 */
	protected String build(String... expr) {
		if (expr == null) {
			return "";
		}
		else {
			StringBuffer buff = new StringBuffer();
			for (String str : expr) {
				if (str == null) {
					buff.append(JbiNamespaceContext.JBI_PREFIX).append(":");
				}
				else { 
					buff.append(str);
				}
			}
			
			return buff.toString();
		}
	}
	
	/**
	 * Utility method to log and throw a {@link DeploymentException}.
	 * @param cause The underlying cause of the returned exception.
	 * @param msg The error message to log.
	 * @return a <code>DeploymentException</code>.
	 */
	protected DeploymentException error(Exception cause, String msg) {
		if (cause == null) {
			log().warning(msg);
			return new DeploymentException(msg);
		}
		else {
			log().log(Level.WARNING, msg, cause);
			return new DeploymentException(msg, cause);
		}
	}
	
	/**
	 * Returns a {@link Logger} for this parser.
	 * @return a <code>Logger</code> for this parser.
	 */
	protected Logger log() {
		return mLogger;
	}
	
//	protected String lookupNamespace
	/**
	 * Resolves an endpoint description from the specified element.
	 * <p>
	 * The element <b>must</b> contain a {@link JbiDescriptor#ENDPOINT_ATTR} and
	 * a {@link JbiDescriptor#SERVICE_ATTR}.  The element can optionally
	 * contain a {@link JbiDescriptor#INTERFACE_ATTR} and/or a 
	 * {@link JbiDescriptor#LINK_TYPE_ATTR}.
	 * 
	 * @param elem An element containing endpoint configuration attributes.
	 * @param isProvides <code>true</code> if configuration describes a 
	 *                   provisioning endpoint, else <code>false</code>.
	 * @return An endpoint configuration.
	 */
	protected EndpointInfo resolveEndpoint(Element elem, boolean isProvides) {
		String endptNm = elem.getAttribute(JbiDescriptor.ENDPOINT_ATTR);
		QName interfaceNm = resolveQName(elem, JbiDescriptor.INTERFACE_ATTR);
		QName serviceNm = resolveQName(elem, JbiDescriptor.SERVICE_ATTR);
		
		LinkType linkType = null;
		String value = elem.getAttribute(ServicesDescriptor.LINK_TYPE_ATTR);
		if (!isProvides && !Util.isEmpty(value)) {
			linkType = LinkType.valueOf(value);
		}
		
		return new EndpointInfo(isProvides, endptNm, interfaceNm,
								serviceNm, linkType);
	}

	/**
	 * Resolves a {@link QName} from an attribute of the specified element.
	 * @param elem An element with the specified attribute.
	 * @param name The specified attribute name.
	 * @return a <code>QName</code> or <code>null</code> if the value cannot be resolved.
	 */
	protected QName resolveQName(Element elem, String name) {
		if (elem == null || name == null) {
			return null;
		}
		
		String qname = elem.getAttribute(name);
	    if (Util.isEmpty(qname)) {
	    	return null;
	    }
	    
		String nsURI = null, localPart = null, prefix = null;
		int index = qname.indexOf(":");
		if (index < 0) {    // this should never happen
			prefix = XMLConstants.DEFAULT_NS_PREFIX;
			localPart = qname;  
			nsURI = elem.getNamespaceURI();
		}
		else {
			prefix = qname.substring(0, index);
			localPart = qname.substring(index + 1);
			
			nsURI = elem.lookupNamespaceURI(prefix); 
		}

		return new QName(nsURI, localPart, prefix);
	}
}
