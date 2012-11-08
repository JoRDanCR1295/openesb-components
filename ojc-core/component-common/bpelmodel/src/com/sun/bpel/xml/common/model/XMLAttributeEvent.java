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
 * @(#)XMLAttributeEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model;

import java.util.EventObject;
import javax.xml.namespace.QName;

/**
 * @author Sun Microsystems
 *
 * event class for XMLAttribute related events.
 */
public class XMLAttributeEvent extends EventObject {

	private XMLElement sourceElement;
	
	private QName attributeQName;
	
	private Object newValue;
	
	private Object oldValue;
	
	
	public XMLAttributeEvent(XMLElement src, 
							QName attrQName, 
							Object newVal, 
							Object oldVal) {
		super(src);
		this.sourceElement = src;
		this.attributeQName = attrQName;
		this.newValue = newVal;
		this.oldValue = oldVal;
	}
	
	public Object getOldValue() {
		return this.oldValue;
	}
	
	public Object getNewValue() {
		return this.newValue;
	}
	
	public QName getAttributeName() {
		return this.attributeQName;
	}
	
	public XMLElement getSourceElement() {
		return this.sourceElement;
	}
}
