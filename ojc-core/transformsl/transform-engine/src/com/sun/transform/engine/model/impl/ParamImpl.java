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
 * @(#)ParamImpl.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.model.impl;

import com.sun.transform.engine.model.Param;

/**
 * Represents data to be passed to a transformation stylesheet at runtime.
 * @author Kevan Simpson
 */
public class ParamImpl implements Param {
	private String mName;
	private Object mValue;
	private Type mType;
	
	public ParamImpl(String name, Type type) {
		mName = name;
		mType = type;
	}

	/** @see com.sun.transform.engine.model.Param#getName() */
	public String getName() {
		return mName;
	}

	/** @see com.sun.transform.engine.model.Param#getValue() */
	public Object getValue() {
		return mValue;
	}

	/** @see com.sun.transform.engine.model.Param#getType() */
	public Type getType() {
		return mType;
	}
	
	/** @see com.sun.transform.engine.model.Param#setValue(java.lang.Object) */
	public void setValue(Object val) {
		mValue = val;
	}

	/** @see java.lang.Object#toString() */
	public String toString() {
		StringBuffer buff = new StringBuffer();
		buff.append("Param[name=").append(getName())
			.append(", type=").append(getType())
			.append(", value=")
			.append(getValue() == null ? "NULL" 
									   : getValue().getClass().getSimpleName())
			.append("]");
			
		return buff.toString();
	}
}
