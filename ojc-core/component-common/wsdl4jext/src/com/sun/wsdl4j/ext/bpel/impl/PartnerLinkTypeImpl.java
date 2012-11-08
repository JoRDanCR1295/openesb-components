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
 * @(#)PartnerLinkTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;
import com.sun.wsdl4j.ext.bpel.PartnerLinkRole;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

public class PartnerLinkTypeImpl extends BPELExtElementImpl
        implements PartnerLinkType {

    private static final long serialVersionUID = 1L;

    protected QName _name;
    protected Collection<PartnerLinkRole> _roles =
        new ArrayList<PartnerLinkRole>();
    protected Map<String, PartnerLinkRole> _roleMap;
    protected NamespaceDeclarations _namespaceDeclarations;

    ///////////////////////////////////////
    //Methods for interface PartnerLinkType
    ///////////////////////////////////////
    
    public QName getName() {
        return _name;
    }

    public PartnerLinkRole getRole(String name) {
        return _roleMap.get(name);
    }

    public Collection<PartnerLinkRole> getRoles() {
        return _roles;
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
    
    public synchronized void setRoles(Collection<PartnerLinkRole> roles) {
        if (roles == null) {
            if (_roles.isEmpty()) {
                return;
            }
            _roles.clear();
            return;
        }
        _roles = roles;
        Map<String, PartnerLinkRole> roleMap =
            new HashMap<String, PartnerLinkRole>();
        for (PartnerLinkRole role : roles) {
            roleMap.put(role.getName(), role);
        }
        _roleMap = roleMap;
    }
    
    public void setNamespaceDeclarations(NamespaceDeclarations declarations) {
        _namespaceDeclarations = declarations;
    }
}
