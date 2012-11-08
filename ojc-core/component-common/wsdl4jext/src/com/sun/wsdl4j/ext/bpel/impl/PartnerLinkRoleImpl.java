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
 * @(#)PartnerLinkRoleImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;

public class PartnerLinkRoleImpl extends BPELExtElementImpl
        implements PartnerLinkRole {

    private static final long serialVersionUID = 1L;
    
    protected final Definition _wsdlDef;
    
    protected String _name;
    protected PortType _portType;
    protected QName _portTypeName;
    protected NamespaceDeclarations _namespaceDeclarations;

    public PartnerLinkRoleImpl(Definition wsdlDef) {
        _wsdlDef = wsdlDef;
    }
    
    ///////////////////////////////////////
    //Methods for interface PartnerLinkRole
    ///////////////////////////////////////
    
    public String getName() {
        return _name;
    }

    public PortType getPort() {
        if (_portType == null) {
            _portType = _wsdlDef.getPortType(_portTypeName);
        }
        return _portType;
    }

    public NamespaceDeclarations getNamespaceDeclarations() {
        return _namespaceDeclarations;
    }
    
    ///////
    //Other
    ///////
    
    public void setName(String name) {
        _name = name;
    }
    
    public void setPortType(PortType portType) {
        _portType = portType;
    }
    
    public QName getPortTypeName() {
        return _portTypeName;
    }
    
    public void setPortTypeName(QName portTypeName) {
        _portTypeName = portTypeName;
    }
    
    public void setNamespaceDeclarations(NamespaceDeclarations declarations) {
        _namespaceDeclarations = declarations;
    }
}
