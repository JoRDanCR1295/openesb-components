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

/**
 * Abstract class for visiting a WSDL definition.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
abstract class AbstractVisitor {
    
    /**
     * Visits a WSDL definition. Concrete class extended from this class
     * will provide the real work to do.
     * 
     * @param parentDef the parent WSDL definition
     * @param currentDef the current WSDL definition being visited
     * @return <code>false</code> to stop further visiting, <code>true</code>
     *         to continue visiting.
     */
    public abstract boolean visit(DefinitionEx parentDef,
            DefinitionEx currentDef);
}
