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
 * @(#)JbiHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers.sax;

import java.io.CharArrayWriter;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.descriptor.EndpointInfo.LinkType;
import com.sun.jbi.common.descriptor.parsers.JbiNamespaceContext;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class JbiHandler<T> extends DefaultHandler {
	private static SAXParserFactory mSaxFactory = SAXParserFactory.newInstance();
	
	static {
		mSaxFactory.setNamespaceAware(true);
		mSaxFactory.setValidating(false);
		try {
			// ABSOLUTELY REQUIRED FOR NAMESPACE RESOLUTION!!!
			mSaxFactory.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
		}
		catch (Exception e) {
			// TODO throw Severe exception
			e.printStackTrace();
		}
	}
	
	private T mValue;
	private Logger mLogger;
	private CharArrayWriter mContent;
	// stack of NS contexts... a list so we can traverse w/o emptying
	private List<JbiNamespaceContext> mNSCtxStack;
	// map of custom handlers, keyed by QName
	private Map<QName, JbiHandler<? extends Object>> mDelegateHandlers;
	// parent, may be null
	private JbiHandler<? extends Object> mParent;
	
	protected JbiHandler() {
		initialize(null);
	}

	protected void initialize(JbiHandler<? extends Object> parent) {
		if (parent == null) {
			mNSCtxStack = new ArrayList<JbiNamespaceContext>();
			mDelegateHandlers = new HashMap<QName, JbiHandler<? extends Object>>();
		}
		else {
			mParent = parent;
			mNSCtxStack = mParent.mNSCtxStack;
		}
		
		mContent = new CharArrayWriter();
		mLogger = Logger.getLogger(this.getClass().getName());
		
	}
	
	public T parse(File file) throws DeploymentException {
		try {
			return (file == null) ? null
					: (parse(file, this)) ? getValue() : null;
		}
		catch (Exception e) {
			throw error(e, I18n.loc(	// XXX
					"Failed to parse file - {0}: {1}", 
					String.valueOf(file), e.getMessage()));
		}
	}

	public T parse(InputSource source) throws DeploymentException {
		try {
			return (source == null) ? null
					: (parse(source, this)) ? getValue() : null;
		}
		catch (Exception e) {
			throw error(e, I18n.loc(	// XXX
					"Failed to parse input source - {0}: {1}", 
					String.valueOf(source.getSystemId()), e.getMessage()));
		}
	}
	
	/** @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int) */
	@Override
	public void characters(char[] ch, int start, int length)
			throws SAXException {
		mContent.write(ch, start, length);
	}

	/** @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException) */
	@Override
	public void error(SAXParseException e) throws SAXException {
		// TODO implement
		e.printStackTrace();
		super.error(e);
	}

	/** @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException) */
	@Override
	public void fatalError(SAXParseException e) throws SAXException {
		// TODO implement
		e.printStackTrace();
		super.fatalError(e);
	}

	/** @see org.xml.sax.helpers.DefaultHandler#warning(org.xml.sax.SAXParseException) */
	@Override
	public void warning(SAXParseException e) throws SAXException {
		// TODO implement
		e.printStackTrace();
		super.warning(e);
	}

	protected void defineHandler(String uri, String name, JbiHandler<? extends Object> handler) {
		String ns = (uri == null) ? JbiDescriptor.JBI_NS : uri;
		if (handler != null) {
			mDelegateHandlers.put(new QName(ns, name), handler);
		}
	}

	protected JbiHandler<? extends Object> lookupDelegate(String uri, String name) {
		return mDelegateHandlers.get(new QName(uri, name));
	}
	
	protected String getContent() {
		String str = mContent.toString().trim();
		mContent.reset();
		return str;
	}
	
	protected JbiHandler<? extends Object> getParent() {
		return mParent;
	}
	
	protected T getValue() {
		return mValue;
	}
	
	protected Object getValue(String uri, String handlerName) {
		JbiHandler<? extends Object> handler = 
				mDelegateHandlers.get(new QName(uri, handlerName));
		return (handler == null) ? null : handler.getValue();
	}
	
	protected void setValue(T val) {
		mValue = val;
	}
	
	protected boolean parse(File file, JbiHandler<T> handler) throws Exception {
		return (file == null) 
				? false : parse(new InputSource(new FileReader(file)), handler);
	}
	
	protected boolean parse(InputSource source, JbiHandler<T> handler) throws Exception {
//		try {
			SAXParser parser = mSaxFactory.newSAXParser();
			parser.parse(source, handler);
			return true;
//		}
//		catch (ParserConfigurationException pce) {
//			// failed to create SAXParser
//			
//		}
//		catch (SAXException se) {
//			// for SAX processing errors
//			
//		} 
//		catch (IOException e) {
//			// for I/O errors
//			
//		}
//		
//		return false;
	}
		
	protected JbiNamespaceContext enterNS(Attributes attr) {
		JbiNamespaceContext ctx = new JbiNamespaceContext();
		
		for (int i = 0, n = attr.getLength(); i < n; i++) {
			String name = attr.getQName(i);
			if (name.startsWith(XMLConstants.XMLNS_ATTRIBUTE)) {
				String prefix = name.substring(1 + name.indexOf(":"));
				ctx.addNamespace(prefix, attr.getValue(i));
			}
		}
		
		mNSCtxStack.add(ctx);
		return ctx;
	}
	
	protected JbiNamespaceContext exitNS() {
		return mNSCtxStack.remove(mNSCtxStack.size() - 1);
	}
	
	/**
	 * Returns a {@link Logger} for this parser.
	 * @return a <code>Logger</code> for this parser.
	 */
	protected Logger log() {
		return mLogger;
	}
	
	protected boolean resolveBoolean(Attributes attr, String name) {
		String val = attr.getValue(name);
		return (Util.isEmpty(val) ? false : Boolean.valueOf(val).booleanValue());
	}
	
	/**
	 * Resolves an endpoint description from the specified element.
	 * <p>
	 * The element <b>must</b> contain a {@link JbiDescriptor#ENDPOINT_ATTR} and
	 * a {@link JbiDescriptor#SERVICE_ATTR}.  The element can optionally
	 * contain a {@link JbiDescriptor#INTERFACE_ATTR} and/or a 
	 * {@link JbiDescriptor#LINK_TYPE_ATTR}.
	 * 
	 * @param attr Attributes defined on currently parsed element.
	 * @param isProvides <code>true</code> if configuration describes a 
	 *                   provisioning endpoint, else <code>false</code>.
	 * @return An endpoint configuration.
	 */
	protected EndpointInfo resolveEndpoint(Attributes attr, boolean isProvides) {
		String endptNm = attr.getValue(JbiDescriptor.ENDPOINT_ATTR);
		QName interfaceNm = resolveQName(attr, JbiDescriptor.INTERFACE_ATTR);
		QName serviceNm = resolveQName(attr, JbiDescriptor.SERVICE_ATTR);
		
		LinkType linkType = null;
		String value = attr.getValue(ServicesDescriptor.LINK_TYPE_ATTR);
		if (!isProvides && !Util.isEmpty(value)) {
			linkType = LinkType.valueOf(value);
		}
		
		return new EndpointInfo(isProvides, endptNm, interfaceNm,
								serviceNm, linkType);
	}

	protected String resolveNS(String prefix) {
		String uri = null;
		for (int i = mNSCtxStack.size() - 1; i >= 0; i--) {
			uri = mNSCtxStack.get(i).getNamespaceURI(prefix);
			if (!Util.isEmpty(uri)) break;
		}
		
		return (Util.isEmpty(uri) && getParent() != null)
				? getParent().resolveNS(prefix) : uri;
	}
	
	/**
	 * Resolves a {@link QName} from an attribute of the specified element.
	 * @param attr Attributes defined on currently parsed element.
	 * @param name The specified attribute name.
	 * @return a <code>QName</code> or <code>null</code> if the value cannot be resolved.
	 */
	protected QName resolveQName(Attributes attr, String name) {
		if (attr == null || name == null) {
			return null;
		}
		
		String qname = attr.getValue(name);
	    if (Util.isEmpty(qname)) {
	    	return null;
	    }
	    
		String nsURI = null, localPart = null, prefix = null;
		int index = qname.indexOf(":");
		if (index < 0) {    // this should never happen
			prefix = XMLConstants.DEFAULT_NS_PREFIX;
			localPart = qname;  
			nsURI = JbiDescriptor.JBI_NS;	// TODO fix this?
		}
		else {
			prefix = qname.substring(0, index);
			localPart = qname.substring(index + 1);
			nsURI = resolveNS(prefix);
		}

		return new QName(nsURI, localPart, prefix);
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
}
