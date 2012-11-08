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
 * @(#)AbstractVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.impl;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.xmlbeans.SchemaTypeSystem;

/**
 * Visitor that collects all schema type systems from a WSDL and its
 * imported WSDLs (transitively).
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class GetTypeSystemVisitor extends AbstractVisitor {

    protected Set<SchemaTypeSystem> _typeSystems =
        new LinkedHashSet<SchemaTypeSystem>();
    
    @Override
    public boolean visit(DefinitionEx parentDef, DefinitionEx currentDef) {
        if (currentDef.getSchemaTypeSystem() != null) {
            _typeSystems.add(currentDef.getSchemaTypeSystem());
        }
        return true;
    }

    /**
     * Gets all schema type systems collected via visiting.
     * 
     * @return A set of schema type systems. 
     */
    public Collection<SchemaTypeSystem> getSchemaTypeSystems() {
        return _typeSystems;
    }
}
