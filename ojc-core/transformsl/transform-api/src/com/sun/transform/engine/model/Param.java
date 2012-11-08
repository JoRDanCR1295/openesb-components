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
 * @(#)Param.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model;


/**
 * Represents data to be passed to a transformation stylesheet at runtime.
 * 
 * @author Kevan Simpson
 */
public interface Param {
	/** Enum to describe the types of supported transformation parameters. */
	public enum Type { PART, URI, LITERAL };

	/**
	 * Returns the parameter name.
	 * @return the parameter name.
	 */
	public String getName();

	/**
	 * Returns the parameter value.
	 * @return the parameter value.
	 */
	public Object getValue();

	/**
	 * Returns the parameter type.
	 * @return the parameter type.
	 */
	public Type getType();
	
	/**
	 * Sets the parameter value.
	 * @param val The parameter value.
	 */
	public void setValue(Object val);
}
