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
 * @(#)FastWSDLDefinitions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import java.util.List;


/**
 * @author Sun Microsystems
 *
 * A FastWSDLDefinitions represent a wsdl document
 * with only some content of the wsdl document parsed in it.
 */
public interface FastWSDLDefinitions {
	
	/**
	 * isWSDL can be used to check if it is really a wsdl
	 * sometimes if wsdl is at url and extension is not know
	 * then calling isWSDL will let you know whether it is really a wsdl
	 * @return
	 */
	public boolean isWSDL();
	
	public String getTargetNamespace();
	
	public void setTargetNamespace(String tNamespace);
	
	public String getParseErrorMessage();
	
	public void setParseErrorMessage(String errorMessage);
	
	public Import createImport();
	
	public void addImport(Import imp);
	
	public List getImports();
	
	public Import getImport(String namespace);
}
