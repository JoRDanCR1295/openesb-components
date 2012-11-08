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
 * @(#)XMLNodeEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model;

import java.util.EventObject;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class XMLNodeEvent extends EventObject {
	
	private XMLNode sourceElement;
	
	private XMLNode childElement;
	
	public XMLNodeEvent(XMLNode source) {
		super(source);
	}
	
	public XMLNodeEvent(XMLNode source, XMLNode child) {
		this(source);
		this.sourceElement = source;
		this.childElement = child;
	}
		
	public XMLNode getSourceElement() {
		return this.sourceElement;
	}
	
	public XMLNode getChildElement() {
		return this.childElement;
	}
	
	public void setChildElement(XMLElement child) {
		this.childElement = child;
	}
	
}
