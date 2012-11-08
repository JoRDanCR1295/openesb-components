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
 * @(#)VariableContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

/**
 * A logical hierarchical container for runtime variables.
 * @author Kevan Simpson
 */
public interface VariableContext {
	public enum VariableType { NODE, MESSAGE, STATUS, NULL, UNKNOWN };
	
	/**
	 * Returns the type of the variable with the specified name.
	 * @param name The variable name.
	 * @return a non-null <code>VariableType</code> enum.
	 */
	public VariableType getType(String name);
	
	/**
	 * Returns the value of a variable with the specified name, which is 
	 * expected to follow in a general sense the BPEL variable naming strategy.
	 * 
	 * @param name The variable name.
	 * @return a variable value or <code>null</code>.
	 */
	public Object getVariable(String name);
	
	/**
	 * Sets the value of a variable.
	 * 
	 * @param name The variable name.
	 * @param val The variable value.
	 */
	public void setVariable(String name, Object val);
	
	/**
	 * Stores message properties for as yet undefined {@link WSMessage} variables.
	 * 
	 * @param varName The name of the variable.
	 * @param prop The property key.
	 * @param value The property value.
	 */
	public void storeMessageProperty(String varName, String prop, String value);
}
