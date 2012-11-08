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
 * @(#)WSDLDocumentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.wsdl.impl;

import javax.wsdl.Definition;

import com.sun.bpel.xml.wsdl.WSDLDocument;

/**
 * Implementation of <code>WSDLDocument</code> interface.
 * 
 * @author Jun Xu
 * @version $Revision: 1.2 $
 */
public class WSDLDocumentImpl implements WSDLDocument {

    protected final Definition _definition;
    protected final String _baseURI;
    
    public WSDLDocumentImpl(String baseURI, Definition wsdlDef) {
        _baseURI = baseURI;
        _definition = wsdlDef;
    }
    
    public Definition getDefinition() {
        return _definition;
    }

    public String getURI() {
        return _baseURI;
    }
}
