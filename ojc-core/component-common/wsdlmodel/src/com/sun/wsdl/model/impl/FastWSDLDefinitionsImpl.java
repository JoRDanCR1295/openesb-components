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
 * @(#)FastWSDLDefinitionsImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sun.wsdl.model.FastWSDLDefinitions;
import com.sun.wsdl.model.Import;





/**
 * @author Sun Microsystems
 *
 * A FastWSDLDefinitions represent a wsdl document
 * with only some content of the wsdl document parsed in it.
 */
public class FastWSDLDefinitionsImpl implements FastWSDLDefinitions {
	
	private String targetNamespace; 
	
	private String parseErrorMessage;
	
	private List imports = new ArrayList();
	
	private boolean isWSDL = false;
	
	public FastWSDLDefinitionsImpl() {
		
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
	
	public void addImport(Import imp) {
		this.imports.add(imp);
	}
	
	public List getImports() {
		return this.imports;
	}
	
	public Import getImport(String namespace) {
		if(namespace == null) {
			return null;
		}
		
		Import imp = null;
		Iterator it = this.imports.iterator();
		
		while(it.hasNext()) {
			Import im = (Import) it.next();
			if(namespace.equals(im.getNamespaceAttr())) {
				imp = im;
				break;
			}
		}
		
		return imp;
	}
	
	public Import createImport() {
		return new ImportImpl();
	}
	
	public boolean isWSDL() {
		return isWSDL;
	}
	
    void setWSDL(boolean wsdl) {
    	this.isWSDL = wsdl;
	}
}
