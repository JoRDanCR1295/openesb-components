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
 * @(#)WSDLElementImm.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.immutable;


import java.util.Map;


/**
 * Describes an immutable WSDLElement element.
 *
 * @author Sun Microsystems
 * @version 
 * @since   5.1.0
 */
public interface WSDLElementImm {    
    /** Getter for other attributes.
     * @return  <code>Map</code> representing other attributes
     */
    Map getOtherAttributes();
    
    /** Gets the default namespace URI declartion.  Note, this method should be recursive
     * and search in the element first and then in its parent and so forth.
     * @return  Default namespace URI; <code>null</code> if none.
     */
    String getNamespace();
    
    /** Gets a namespace URI declartion.  Note, this method should be recursive
     * and search in the element first and then in its parent and so forth.
     * @param   prefix  Prefix for namespace.
     * @return  A namespace URI; <code>null</code> if none.
     */
    String getNamespace(String prefix);
    
    /**
     * Gets the prefix associated with the given namespace URI.  Note, this method should be recursive
     * and search in the element first and then in its parent and so forth.
     * @param namespaceURI the namespace URI
     * @return the namespace prefix or null if the namespace URI is not
     * currently associated with a prefix
     */
    String getNamespacePrefix(String namespaceURI);
    
    /** Gets the XML document that owns this element.
     * @return  Owner BPEL Document.
     */
    WSDLDocumentImm getOwnerDocument();
}
