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
 * @(#)MessagePropertyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;
import com.sun.wsdl4j.ext.bpel.MessageProperty;

/**
 * Implementation of the <code>MessageProperty</code> interface.
 * 
 * @author Jun Xu
 * @version $Revision: 1.4 $
 */
public class MessagePropertyImpl extends BPELExtElementImpl
        implements MessageProperty {
    
    private static final long serialVersionUID = 1L;
    
    protected QName _name;
    protected QName _type;
    protected NamespaceDeclarations _namespaceDeclarations;

    ///////////////////////////////////////
    //Methods for interface MessageProperty
    ///////////////////////////////////////
    
    public QName getName() {
        return _name;
    }

    public QName getType() {
        return _type;
    }

    public NamespaceDeclarations getNamespaceDeclarations() {
        return _namespaceDeclarations;
    }

    ///////
    //Other
    ///////
    
    public void setName(QName name) {
        _name = name;
    }
    
    public void setType(QName type) {
        _type = type;
    }
    
    public void setNamespaceDeclarations(NamespaceDeclarations declarations) {
        _namespaceDeclarations = declarations;
    }
}
