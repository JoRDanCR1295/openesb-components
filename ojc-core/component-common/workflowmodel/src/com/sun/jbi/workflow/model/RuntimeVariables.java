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
 * @(#)$Id: RuntimeVariables.java,v 1.1 2010/02/04 02:53:27 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model;

import java.util.HashMap;
import java.util.Set;

import org.apache.commons.jxpath.Variables;

public interface RuntimeVariables extends Variables {
	
	public static final String TASK_INPUT_VAR = "TaskInput";
	public static final String TASK_OUTPUT_VAR = "TaskOutput";
//	public static final String TASK_PAYLOAD = "TaskPayload";
	public static final String TASK_INSTANCE_PRIORITY = "TaskInstance.Priority";
	public static final String TASK_INSTANCE_TITLE= "TaskInstance.Title";
    public static final String TASK_INSTANCE_OWNER= "TaskInstance.Owner";
    public static final String TASK_ID = "TaskInstance.Id";
	
	
	public static enum VariableType {
		Element("Element"),
		Text ("Text");
		
		String typeName;
		VariableType (String typeName) {
			this.typeName = typeName;
		}
		
		
		public String getTypeName() {
			return typeName;
		}


		public static VariableType  getType (String typeStr) {
			if (typeStr.equals(Element.typeName)) {
				return Element;
			} else if (typeStr.equals(Text.typeName)) {
				return Text;
			}
			return null;
		}
		
		public static VariableType getType (Object obj) {
			if (obj instanceof org.w3c.dom.Element) {
				return Element;
			} else {
				return Text;
			} 
		}
	}
	
	 VariableType getVariableType (String varName);
	
    /**
     * Defines a new variable with the specified value or modifies
     * the value of an existing variable.
     *
     * @param varName is a variable name without the "$" sign
     * @param value is the new value for the variable, which can be null
     * @param the type of the variable
     */
    void declareVariable(String varName, Object value, VariableType type) ;
    
    int size ();
    
    Set getVars ();
    
    /**
     * Returns the variable map <varName, value>
     * @return
     */
    HashMap<String, Object> getVarMap ();
	
}
