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
 * @(#)OtherAttributeRenameCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.commands;

import java.util.Map;

import com.sun.wsdl.model.common.model.XMLElement;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class OtherAttributeRenameCommand extends Command {

	private XMLElement mElement;
	
	private String mAttributeOldName;
	
	private String mAttributeNewName;
	
	private String mOldAttributeValue = null;
	
	public OtherAttributeRenameCommand(XMLElement element) {
		this.mElement = element;
	}
	
	public void setAttributeOldName(String attributeOldName) {
		this.mAttributeOldName = attributeOldName;
	}
	
	public void setAttributeNewName(String attributeNewName) {
		this.mAttributeNewName = attributeNewName;
	}
	
	public boolean canUndo() {
		return true;
	}
	
	public void execute() {
		Map otherAttrs = this.mElement.getOtherAttributes();
		if(otherAttrs != null) {
			this.mOldAttributeValue = (String) otherAttrs.get(this.mAttributeOldName);
		}
		
		this.mElement.setOtherAttributes(this.mAttributeOldName, null);
		this.mElement.setOtherAttributes(this.mAttributeNewName, this.mOldAttributeValue);
	}
	
	
	public void undo() {
		this.mElement.setOtherAttributes(this.mAttributeNewName, null);
		this.mElement.setOtherAttributes(this.mAttributeOldName, this.mOldAttributeValue);
			
	}
}
