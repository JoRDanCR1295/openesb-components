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
 * @(#)GetPropertyAliasVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;

/**
 * Visitor that collects all message property aliases or finds a set of
 * message property aliases based on a name from a WSDL and its imported ones.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class GetPropertyAliasVisitor extends AbstractVisitor {

    protected QName _aliasName;
    protected Set<MessagePropertyAlias> _propertyAliases =
        new LinkedHashSet<MessagePropertyAlias>();
    
    /**
     * Default constructor, which enables collecting all message property
     * aliases.
     */
    public GetPropertyAliasVisitor() {
    }
    
    /**
     * Constructs from a property name, which enables collecting a set of
     * message properties based on this name.
     * 
     * @param name
     */
    public GetPropertyAliasVisitor(QName name) {
        _aliasName = name;
    }
    
    @Override
    public boolean visit(DefinitionEx parentDef, DefinitionEx currentDef) {
        if (_aliasName != null) {
            _propertyAliases.addAll(
                    currentDef.getMessagePropertyAliases(_aliasName));
        } else {
            _propertyAliases.addAll(
                    currentDef.getMessagePropertyAliases());
        }
        return true;
    }
    
    public Collection<MessagePropertyAlias> getPropertyAliases() {
        return _propertyAliases;
    }
}
