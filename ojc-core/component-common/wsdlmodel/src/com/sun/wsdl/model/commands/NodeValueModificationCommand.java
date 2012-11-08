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
 * @(#)NodeValueModificationCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.commands;

import com.sun.wsdl.model.common.model.XMLNode;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class NodeValueModificationCommand extends Command {

	private XMLNode mNode;
	
	private String mValue;
	
	private String mOldValue = null;
	
	public NodeValueModificationCommand(XMLNode node) {
		this.mNode = node;
	}
	
	public void setNodeValue(String value) {
		this.mValue = value;
	}
	
	public boolean canUndo() {
		return true;
	}
	
	public void execute() {
		this.mOldValue = this.mNode.getValue();
		this.mNode.setValue(this.mValue);
	}
	
	
	public void undo() {
			this.mNode.setValue(this.mOldValue);
	}
}
