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
 * @(#)MessagePropertyAlias.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.NamespaceDeclarations;

/**
 * An instance of this interface represents a BPEL message property alias.
 * 
 * @author Jun Xu
 * @version $Revision: 1.6 $
 */
public interface MessagePropertyAlias extends ExtensibilityElement {
    
    /**
     * Gets the property name of this alias.
     * 
     * @return the qualified property name of the alias
     */
    QName getName();
    
    /**
     * Gets the message type of this property alias.
     * 
     * @return The message type. May return <code>null</code> if the property
     *         alias is defined using an element declaration or an XML
     *         type. 
     */
    Message getMessageType();
    
    /**
     * Gets the part name.
     * 
     * @return The part name. May return <code>null</code> if the property
     *         alias is defined using an element declaration or an XML
     *         type. 
     */
    String getPartName();
    
    /**
     * Gets the part.
     * 
     * @return The part. May return <code>null</code> if the property
     *         alias is defined using an element declaration or an XML
     *         type. 
     */
    Part getPart();
    
    /**
     * Gets the element declaration of this property alias.
     * 
     * @return The qualified name of the element declaration.  May return
     *         <code>null</code> if the property alias is defined using
     *         a message type or an XML type.
     */
    QName getElementName();
    
    /**
     * Gets the XML type of this property alias.
     * 
     * @return The qualified name of the XML type.  May return
     *         <code>null</code> if the property alias is defined using
     *         a message type or an element declaration.
     */
    QName getTypeName();
    
    /**
     * Gets the query string.
     * 
     * @return the query string
     */
    Query getQuery();
    
    /**
     * Gets namespace declarations for this extensibility element. The returned
     * instance contains all namespace declarations of this extensibility
     * element and its ancestors.
     * 
     * @return A instance of <code>NamespaceDeclarations</code>
     */
    NamespaceDeclarations getNamespaceDeclarations();

    String getNMProperty();
    
    public interface Query {
        
        String getLanguage();
        
        String getQueryString();
        
        /**
         * Gets namespace declarations for this extensibility element. The returned
         * instance contains all namespace declarations of this extensibility
         * element and its ancestors.
         * 
         * @return A instance of <code>NamespaceDeclarations</code>
         */
        NamespaceDeclarations getNamespaceDeclarations();
    }
}
