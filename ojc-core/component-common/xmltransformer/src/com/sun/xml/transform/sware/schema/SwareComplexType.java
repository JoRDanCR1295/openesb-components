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
 * @(#)SwareComplexType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

/**
 * Wrapping interface for complex type definition.  More methods might
 * be added if more information is needed from the wrapper.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareComplexType extends SwareType {
    
    /**
     * Tests if the complex type definition represents simple content.
     * 
     * @return <code>true</code> if is simple content, otherwise <code>false</code>
     */
    public boolean isSimpleContent();
    
    /**
     * Tests if the complex type definition represents complex content.
     * 
     * @return <code>true</code> if is complex content, otherwise <code>false</code>
     */
    public boolean isComplexContent();
    
    /**
     * Gets the container element of this complex type.
     * 
     * @return the container element.
     */
    public SwareElement getContainerElement();
}
