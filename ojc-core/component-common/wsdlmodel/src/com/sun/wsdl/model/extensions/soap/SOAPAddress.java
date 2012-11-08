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
 * @(#)SOAPAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.extensions.soap;

import com.sun.wsdl.model.extensions.ExtensibilityElement;

/**
 * Describes the SOAP address extension.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface SOAPAddress extends ExtensibilityElement, SOAPConstants {
    
    /** Tag for this element */
    public static final String TAG = "address";
    
    /** Qualified Tag for this element */
    public static final String QTAG = PREFIX + ":" + TAG;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR {
        
        /** "location" attribute token */
        public static final String LOCATION = "location";
    }
    
    /** Ordinal position for location attribute */
    public static final int LOCATION = 0;
    
    /** Gets the location attribute for this SOAP address.
     * @return  The location attribute.
     */
    String getLocation();
    
    /** Sets the location attribute for this SOAP address.
     * @param   location    Value of the location attribute.
     */
    void setLocation(String location);
    
    /** Sets the location attribute for this SOAP address.
     * @param   qName       Qualified name of attribute.
     * @param   location    Value of the location attribute.
     */
    void setLocation(String qName, String location);
}
