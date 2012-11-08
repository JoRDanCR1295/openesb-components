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

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Target;

/**
 * 
 * @author Kevan Simpson
 */
public class TargetHandler extends JbiHandler<Target> {
	private String mCompName, mArtifact;
	
	public TargetHandler() {
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#endElement(java.lang.String, java.lang.String, java.lang.String) */
	@Override
	public void endElement(String uri, String localName, String name)
			throws SAXException {
		// set value
		if (localName.equals(JbiDescriptor.TARGET_ELEM)) {
			setValue(new Target(mArtifact, mCompName));
		}
		else if (localName.equals(JbiDescriptor.COMPONENT_NAME_ELEM)) {
			mCompName = getContent();
		}
		else if (localName.equals(JbiDescriptor.ARTIFACT_ELEM)) {
			mArtifact = getContent();
		}
		
		// delegate to top of stack
		super.endElement(uri, localName, name);
	}

	/** @see com.sun.jbi.common.descriptor.parsers.sax.JbiHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
	@Override
	public void startElement(String uri, String localName, String name,
			Attributes attr) throws SAXException {
		// read ns...
		super.startElement(uri, localName, name, attr);
	}

}
