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
 * @(#)ImplSpecificComponent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * An interface that all implementation specific components (the components
 * that tie up to a specific XML schema model implementation)
 * must implement.  With this interface in place, there is a way to check if
 * an instance of a schema component interface is compatible with a specific
 * implementation.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface ImplSpecificComponent {
    
    /**
     * Gets the implementation type of a schema component.
     * 
     * @return the implementation type
     */
    public ImplementationType getImplementationType();
}
