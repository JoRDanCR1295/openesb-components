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
 * @(#)ServiceAssemblyHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers.sax;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Connection;

/**
 * 
 * @author Kevan Simpson
 */
public class ConnectionHandler extends JbiHandler<Connection[]> {
	private EndpointInfo mConsumer, mProvider;
	private List<Connection> mConnList;
	
	/**
	 * 
	 */
	public ConnectionHandler() {
		mConnList = new ArrayList<Connection>();
	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		if (localName.equals(JbiDescriptor.CONNECTION_ELEM)) {
			mConnList.add(new Connection(mConsumer, mProvider));
		}
		else if (localName.equals(JbiDescriptor.CONNECTION_LIST_ELEM)) {
			setValue(mConnList.toArray(new Connection[mConnList.size()]));
		}
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
	@Override
	public void startElement(String uri, String localName, String name,
							 Attributes attr) throws SAXException {
		// parse consumes and provides
		if (localName.equals(JbiDescriptor.CONSUMER_ELEM)) {
			mConsumer = resolveEndpoint(attr, false);
		}
		else if (localName.equals(JbiDescriptor.PROVIDER_ELEM)) {
			mProvider = resolveEndpoint(attr, true);
		}
	}
}
