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
 * @(#)PropertyAlias.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.bpel;

import javax.xml.namespace.QName;
import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the &lt;propertyAlias&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface PropertyAlias extends ExtensibilityElement {
    
    /** Tag for this element. */
    public static final String TAG = "propertyAlias";
    
    /** Describes attributes of this element.
     */
    public interface ATTR {
        
        /** "propertyName" attribute token */
        public static final String PROPERTY_NAME = "propertyName";
        
        /** "messageType" attribute token */
        public static final String MESSAGE_TYPE = "messageType";
        
        /** "part" attribute token */
        public static final String PART = "part";
        
    }
    /** Ordinal position of attribute: propertyName */
    public static final int PROPERTY_NAME = 0;
    
    /** Ordinal position of attribute: messageType */
    public static final int MESSAGE_TYPE = PROPERTY_NAME + 1;
    
    /** Ordinal position of attribute: part */
    public static final int PART = MESSAGE_TYPE + 1;
    
    /** Getter for propertyName attribute.
     * @return  Value of propertyName attribute.
     */
    QName getPropertyName();
    
    /** Setter for propertyName attribute.
     * @param   n   Value of propertyName attribute.
     */
    void setPropertyName(QName n);
    
    /** Getter for messageType attribute.
     * @return  Value of messageType attribute.
     */
    QName getMessageType();
    
    /** Setter for messageType attribute.
     * @param   t   Value of messageType attribute.
     */
    void setMessageType(QName t);
    
    /** Getter for part attribute.
     * @return  Value of part attribute.
     */
    String getPart();
    
    /** Setter for part attribute.
     * @param   p   Value of part attribute.
     */
    void setPart(String p);
    
    /** Getter for query attribute.
     * @return  Value of query attribute.
     */
    String getQuery();
    
    /** Setter for query attribute.
     * @param   s   Value of query attribute.
     */
    void setQuery(String s);
    
    /**
     * Get the Query Object
     * @return Query
     */
    Query getQueryObject();
    
    /**
     * Set Querty object
     * @param query
     */
    void setQueryObject(Query query);
}
