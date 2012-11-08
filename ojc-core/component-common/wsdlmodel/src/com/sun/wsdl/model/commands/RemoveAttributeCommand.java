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
 * @(#)RemoveAttributeCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.commands;

import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLElement;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class RemoveAttributeCommand extends Command {
	
	private XMLElement mElement;
	
	private String mAttributeName;
	
	private String mAttributeValue;
	
	private XMLAttribute mOldAttribute;
	
	public RemoveAttributeCommand(XMLElement element) {
		this.mElement = element;
	}
	
	public void setAttributeNameValue(String attributeName, String attributeValue) {
		this.mAttributeName = attributeName;
		this.mAttributeValue = attributeValue;
	}
	
	public boolean canUndo() {
		return true;
	}
	
	public void execute() {
		this.mOldAttribute = this.mElement.getAttribute(this.mAttributeName);
		this.mElement.setAttribute(this.mAttributeName, null);
	}
	
	
	public void undo() {
		if(this.mOldAttribute != null) {
			this.mElement.setAttribute(this.mAttributeName, this.mOldAttribute.getValue());
		}
		
	}

}
