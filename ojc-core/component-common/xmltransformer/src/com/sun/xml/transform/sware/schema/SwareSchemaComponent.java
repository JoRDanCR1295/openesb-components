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
 * @(#)SwareSchemaComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

import javax.xml.namespace.QName;

/**
 * This is a generic interface that will be extended by all concrete schema
 * component interfaces.  It provides basic information of a schema component.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareSchemaComponent {

    /**
     * Gets the component type of the schema component.
     * 
     * @return the component type of the schema component
     */
    public ComponentType getComponentType();
    
    /**
     * Gets the qualified name of the schema component if it is a named one.
     * 
     * @return the qualified name of the schema component or <code>null</code>
     *         if it it not named. 
     */
    public QName getName();
}
