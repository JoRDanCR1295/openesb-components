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
 * @(#)SwareType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * Generic wrapping interface for an XML type definition.  More methods might
 * be added if more information is needed from the wrapper.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareType extends ImplSpecificComponent,
    SchemaComponentWrapper, SwareSchemaComponent {

    /**
     * Tests if the type definition is an any type.
     * 
     * @return <code>true</code> if it is an any type, otherwise
     *          <code>false</code>
     */
    public boolean isAnyType();
    
    /**
     * Tests if the type definition is a complex type definition.
     * 
     * @return <code>true</code> if it is a complex type definition,
     *          otherwise <code>false</code>
     */
    public boolean isComplexType();
    
    /**
     * Tests if the type definition is a simple type definition.
     * 
     * @return <code>true</code> if it is a simple type definition,
     *          otherwise <code>false</code>
     */
    public boolean isSimpleType();
}
