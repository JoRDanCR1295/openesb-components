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
 * @(#)ServicesHandler.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers.sax;

import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Identification;
import com.sun.jbi.common.descriptor.model.Target;

/**
 * 
 * @author Kevan Simpson
 */
public class UnitHandler extends JbiHandler<AssemblyUnit> {
	private Identification mId;
	private Target mTarget;
	
	public UnitHandler() {
		defineHandler(null, JbiDescriptor.IDENTIFICATION_ELEM, new IdentificationHandler());
		defineHandler(null, JbiDescriptor.TARGET_ELEM, new TargetHandler());
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		// set value
		if (localName.equals(JbiDescriptor.SERVICE_UNIT_ELEM)) {
			setValue(new AssemblyUnit(mTarget, mId));
		}
		else if (localName.equals(JbiDescriptor.IDENTIFICATION_ELEM)) {
			mId = (Identification) getValue(uri, localName);
		}
		else if (localName.equals(JbiDescriptor.TARGET_ELEM)) {
			mTarget = (Target) getValue(uri, localName);
		}
	}
}
