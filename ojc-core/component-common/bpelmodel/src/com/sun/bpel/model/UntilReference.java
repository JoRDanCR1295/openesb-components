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
 * @(#)UntilReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.xml.common.model.XMLElement;

/**
 *
 * @author Sun Microsystems
 */
public interface UntilReference extends XMLElement {
    
    
    /** "until" wait type */
    public static final String TYPE_UNTIL = "until";
   
    
    /** Getter for the until attribute.
     * @return  Value of the until attribute.
     */
    String getUntil();
    
    /** Setter for the until attribute.
     * @param   u   Value of the until attribute.
     */
    void setUntil(String u);
    
    
    
    /**
     * set Until child element for this onAlarm.
     * @param u Until child element
     */
    void setBPELUntil(Until u);
    
    /**
     * get Untill child elememt for this onAlarm.
     * @return Until child element.
     */
    Until getBPELUntil();
}
