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

import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.model.Identification;

/**
 * 
 * @author Kevan Simpson
 */
public class ServiceAssemblyHandler extends JbiHandler<ServiceAssembly> {
	private Identification mId;
	private List<AssemblyUnit> mUnitList;
	private Connection[] mConns = null;
	
	/**
	 * 
	 */
	public ServiceAssemblyHandler() {
		mUnitList = new ArrayList<AssemblyUnit>();
		defineHandler(null, JbiDescriptor.CONNECTION_LIST_ELEM, new ConnectionHandler());
		defineHandler(null, JbiDescriptor.SERVICE_UNIT_ELEM, new UnitHandler());
		defineHandler(null, JbiDescriptor.IDENTIFICATION_ELEM, new IdentificationHandler());
	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		if (localName.equals(JbiDescriptor.CONNECTION_LIST_ELEM)) {
			mConns = (Connection[]) getValue(uri, localName);
		}
		else if (localName.equals(JbiDescriptor.SERVICE_UNIT_ELEM)) {
			mUnitList.add((AssemblyUnit) getValue(uri, localName));
		}
		else if (localName.equals(JbiDescriptor.IDENTIFICATION_ELEM)) {
			mId = (Identification) getValue(uri,localName);
		}
		else if (localName.equals(JbiDescriptor.SERVICE_ASSEMBLY_ELEM)) {
			AssemblyUnit[] units = 
					mUnitList.toArray(new AssemblyUnit[mUnitList.size()]);
			setValue(new ServiceAssembly(mId, units, mConns));
		}
	}
}
