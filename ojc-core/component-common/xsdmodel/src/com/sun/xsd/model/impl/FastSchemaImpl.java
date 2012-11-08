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
 * @(#)FastSchemaImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xsd.model.impl;

import java.util.ArrayList;
import java.util.List;

import com.sun.xsd.model.FastSchema;







/**
 * @author Sun Microsystems
 *
 * A FastWSDLDefinitions represent a wsdl document
 * with only some content of the wsdl document parsed in it.
 */
public class FastSchemaImpl implements FastSchema {
	
	private String targetNamespace; 
	
	private String parseErrorMessage;
	
	private List imports = new ArrayList();
	
	public FastSchemaImpl() {
		
	}
	
	public String getTargetNamespace() {
		return this.targetNamespace;
	}
	
	public void setTargetNamespace(String tNamespace) {
		this.targetNamespace = tNamespace;
	}
	
	public String getParseErrorMessage() {
		return this.parseErrorMessage;
	}
	
	public void setParseErrorMessage(String errorMessage) {
		this.parseErrorMessage = errorMessage;
	}
	
}
