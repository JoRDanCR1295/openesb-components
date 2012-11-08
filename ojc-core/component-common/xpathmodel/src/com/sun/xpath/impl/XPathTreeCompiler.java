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
 * @(#)XPathTreeCompiler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xpath.impl;

import org.apache.commons.jxpath.ri.compiler.TreeCompiler;
import org.apache.commons.jxpath.ri.compiler.Constant;

/**
 * @author Sun Microsystems
 *
 * Extended TreeCompiler for handling of Number values.
 * We want to distinguish between Long and Double
 * value of the Constant.
 */
public class XPathTreeCompiler extends TreeCompiler {
	
	/**
	 * overriden this method to create
	 * appropriate Long or Double as the value stored in
	 * Constant.
	 */
	public Object number(String value) {
		//distinguish between Long and Double 
		try {
			int intVal = Integer.parseInt(value);
			return new Constant(new Long(value));
		} catch(NumberFormatException ex) {
			//Do Nothing
		}
		
        return new Constant(new Double(value));
    }
}
