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
 * @(#)Import.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;



/**
 * Describes the &lt;import&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Import extends WSDLElement {
    /** Tag for this element */
    public static final String TAG = "import";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends WellKnownAttr {
        
        /** "namespace" attribute token */
        public static final String NAMESPACE = "namespace";
        
        /** "location" attribute token */
        public static final String LOCATION = "location";
    }
    /** Ordinal position for namespace attribute */
    public static final int NAMESPACE = 0;
    
    /** Ordinal position for location attribute */
    public static final int LOCATION = 1;
    
    /** Getter for property namespace.
     * @return Value of property namespace.
     *
     */
    String getNamespaceAttr();
    
    /** Setter for property namespace.
     * @param namespace New value of property namespace.
     *
     */
    void setNamespaceAttr(String namespace);
    
    /** Getter for property location.
     * @return Value of property location.
     *
     */
    String getLocation();
    
    /** Setter for property location.
     * @param location New value of property locatoin.
     *
     */
    void setLocation(String location);
    
    /**
     * set the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @param document top level document
     */
    void setImportedObject(Object document);

    /**
     * get the top level object which this import represents.
     * this will be either WSDLDocument or XMLSchema
     * @return WSDLDocument or XMLSchema
     */
    Object getImportedObject();
}
