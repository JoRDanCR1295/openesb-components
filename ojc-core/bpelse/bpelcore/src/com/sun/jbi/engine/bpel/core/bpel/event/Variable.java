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
 * @(#)$Id: Variable.java,v 1.4 2009/03/07 02:10:03 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;


public interface Variable {

	 static enum DataType {
			String,
			Number,
			Boolean,
			Date,
			Simple_Type_Other,
			Complex_Type,
			Element,
			Message
		 }	
	 
	 DataType getType ();
	
	 long getScopeId ();	 
	 
	 long getVarId ();
	 
	 String getVarName ();
	 
	 String getValue ();
	 
	 QName getFaultName ();
	 
	 boolean isFault ();	 
	 
	VariableScope getVariableScope ();
	 
}
