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
 * @(#)StackableHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers.sax;

import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * 
 * @author Kevan Simpson
 */
public class StackableHandler<T> extends JbiHandler<T> {
	// stack of sax handlers
	private Stack<JbiHandler<? extends Object>> mHandlerStack;
	// root handler
	private JbiHandler<T> mRoot;
	// stack of tag names for which delegates have been assigned
	private Stack<String> mDelegatedStack;
	
	public StackableHandler(JbiHandler<T> root) {
		mDelegatedStack = new Stack<String>();
		mHandlerStack = new Stack<JbiHandler<? extends Object>>();
		mRoot = root;
		mRoot.initialize(this);
		push("ROOT", mRoot);
	}
	
	/** @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int) */
	@Override
	public void characters(char[] ch, int start, int length)
			throws SAXException {
		peek().characters(ch, start, length);
	}

	/** @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
	@Override
	public void startElement(String uri, String localName, String name,
							 Attributes attr) throws SAXException {
		// delegate?
		JbiHandler<? extends Object> delegate = delegate(uri, localName);
		if (delegate != null) {
			// TODO log
//			System.out.println("delegating "+ localName);
		}

		peek().enterNS(attr);
		peek().startElement(uri, localName, name, attr);
	}		

	/** @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		peek().endElement(uri, localName, name);
		peek().exitNS();
		if (pop(localName) != null) {
			peek().endElement(uri, localName, name);
		}
	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#getValue() */
	@Override
	public T getValue() {
		return mRoot.getValue();
	}

	protected JbiHandler<? extends Object> delegate(String uri, String localName) 
			throws SAXException { 

		JbiHandler<? extends Object> handler = peek().lookupDelegate(uri, localName);
		if (handler != null) {
			handler.initialize(peek());
			push(localName, handler);
		}

		return handler;
	}

	protected JbiHandler<? extends Object> peek() {
		return (mHandlerStack.isEmpty() ? null : mHandlerStack.peek());
	}

	protected JbiHandler<? extends Object> pop(String tag) {
		if (tag.equals(mDelegatedStack.peek())) {
			mDelegatedStack.pop();
			return (mHandlerStack.isEmpty() ? null : mHandlerStack.pop());
		}
		
		return null;
	}

	protected void push(String tag, JbiHandler<? extends Object> handler) {
		mDelegatedStack.push(tag);
		mHandlerStack.push(handler);
	}

}
