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
 * @(#)WSDLURILocation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.uri.impl;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import org.exolab.castor.net.URILocation;

/**
 * @author Sun Microsystems
 *
 * This URILocation delegated to local wsdl.xsd
 * for any reference to http://schemas.xmlsoap.org/wsdl/
 * in a xsd import statement.
 * 
 * Later we could delegate to xml catalog.
 */
public final class WSDLURILocation extends URILocation {
	
	public static final String WSDL_NAMESPACE = "http://schemas.xmlsoap.org/wsdl/";
	
	private static final String wsdlXSDUrl = "/com/sun/wsdl/model/uri/impl/wsdl.xsd";
	
	public String getAbsoluteURI() {
		return WSDL_NAMESPACE;
	}
	
	public String getBaseURI() {
		return WSDL_NAMESPACE;
	}
	
	public Reader getReader() throws IOException {
		InputStreamReader reader = new InputStreamReader(WSDLURILocation.class.getResourceAsStream(wsdlXSDUrl));
		return reader;
	}
	
	public String getRelativeURI() {
		return null;
	}
	
}
