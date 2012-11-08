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
 * @(#)MessageProperty.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;

/**
 * WSDL extension element that represents a BPEL message property.
 *  
 * @author Jun Xu
 * @version $Revision: 1.4 $
 */
public interface MessageProperty extends ExtensibilityElement {
    
    /**
     * Gets the qualified name of the property.
     * 
     * @return the qualified name of the property
     */
    QName getName();

    /**
     * Gets the qualified name of the type.
     * 
     * @return the qualified name of the type
     */
    QName getType();
    
    /**
     * Gets namespace declarations for this extensibility element. The returned
     * instance contains all namespace declarations of this extensibility
     * element and its ancestors.
     * 
     * @return A instance of <code>NamespaceDeclarations</code>
     */
    NamespaceDeclarations getNamespaceDeclarations();
}
