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
 * @(#)DescriptorHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import java.util.ArrayList;
import java.util.List;

import javax.jbi.management.DeploymentException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.util.I18n;

/**
 * Parses JBI connections from a service assembly descriptor.
 * @author Kevan Simpson
 */
public class ConnectionParser extends AbstractJbiParser<Connection[]> {
	private List<Connection> mConnections;
	private String mDefaultPrefix;

	/**
	 * Constructs a <code>ConnectionParser</code>.
	 */
	public ConnectionParser() {
		this(null, null);
	}

	/**
	 * Constructs a <code>ConnectionParser</code> using the specified parser's
	 * {@link XPath} resouce and {@link JbiNamespaceContext}.
	 * @param jp A <code>JbiParser</code> which must not be <code>null</code>.
	 */
	public ConnectionParser(JbiParser jp) {
		this(jp.getXPath(), jp.getNSContext());
	}
	
	/**
	 * Constructs a <code>ConnectionParser</code> using the specified
     * {@link XPath} resouce and {@link JbiNamespaceContext}.
     * @param xpath An <code>XPath</code> resource.
     * @param ctx A <code>JbiNamepaceContext</code>.
	 */
	protected ConnectionParser(XPath xpath, JbiNamespaceContext ctx) {
		super(xpath, ctx);
		mConnections = new ArrayList<Connection>();
		setDefaultPrefix(JbiNamespaceContext.JBI_PREFIX);
	}
	

	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public Connection[] parse(Element elem) throws DeploymentException {
		try {
			String exprBase = build("//", getDefaultPrefix(), ":", JbiDescriptor.CONNECTION_ELEM);
			NodeList connList = (NodeList) getXPath().evaluate(
					exprBase, elem, XPathConstants.NODESET);
			for (int i = 0, n = connList.getLength(); i < n; i++) {
				Element conn = (Element) connList.item(i);
				mConnections.add(parseConnection(conn));
			}
		}
		catch (Exception e) {
		    throw error(e,
	    			I18n.loc("UTIL-6003: Failed to parse service assembly connections: {0}",
	    				     e.getMessage()));
		}
		
		return getConnections();
	}

	/**
	 * Parses a JBI {@link JbiDescriptor#CONNECTION_ELEM Connection} element.
	 * @param elem The connection element.
	 * @return A <code>Connection</code>.
	 * @throws DeploymentException if an error occurs resolving connection.
	 */
	protected Connection parseConnection(Element elem) throws DeploymentException {
		String uri = getNSContext().getNamespaceURI(getDefaultPrefix());
		EndpointInfo cons = null, prov = null;
		
		NodeList list = elem.getElementsByTagNameNS(uri, JbiDescriptor.CONSUMER_ELEM);
		if (list != null && list.getLength() == 1) {
			cons = resolveEndpoint((Element) list.item(0), false);
		}
		
		list = elem.getElementsByTagNameNS(uri, JbiDescriptor.PROVIDER_ELEM);
		if (list != null && list.getLength() == 1) {
			prov = resolveEndpoint((Element) list.item(0), true);
		}

		return new Connection(cons, prov);
	}
	
	/** @return the defaultPrefix */
	protected String getDefaultPrefix() {
		return mDefaultPrefix;
	}

	/** @param defaultPrefix the defaultPrefix to set */
	protected void setDefaultPrefix(String defaultPrefix) {
		mDefaultPrefix = defaultPrefix;
	}

	private Connection[] getConnections() { 
		Connection[] result = new Connection[mConnections.size()];
		mConnections.toArray(result);
		return result;
	}
}
