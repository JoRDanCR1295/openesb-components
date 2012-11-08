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
 * @(#)GetMessagePropertyVisitor.java 
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

import com.sun.wsdl4j.ext.bpel.MessageProperty;

/**
 * Visitor that collects all message properties or finds one specific
 * message property from a WSDL and its imported ones.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class GetMessagePropertyVisitor extends AbstractVisitor {
    
    protected QName _propName;
    protected MessageProperty _property;
    protected Set<MessageProperty> _properties =
        new LinkedHashSet<MessageProperty>();
    
    /**
     * Default constructor, which enables getting all message properties.
     */
    public GetMessagePropertyVisitor() {
    }

    /**
     * Constructs from a property name, which enables get a specific
     * message property by its name.
     * 
     * @param propName The property name
     */
    public GetMessagePropertyVisitor(QName propName) {
        _propName = propName;
    }
    
    @Override
    public boolean visit(DefinitionEx parentDef, DefinitionEx currentDef) {
        if (_propName != null) {
            MessageProperty prop = currentDef.getMessageProperty(_propName);
            if (prop != null) {
                _property = prop;
                return false;
            }
            return true;
        }
        _properties.addAll(currentDef.getMessageProperties());
        return true;
    }
    
    public MessageProperty getMessageProperty() {
        return _property;
    }
    
    public Collection<MessageProperty> getMessageProperties() {
        return _properties;
    }
}
