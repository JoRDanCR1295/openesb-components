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
 * @(#)Copy.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


/**
 * Describes the &lt;copy&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Copy extends BPELElement {
    /** Tag for this element. */
    public static final String TAG = "copy";
    
    /** Enumerated values for an optional BINARY_COPY type */
    public static final String[] BINARY_COPY_ENUM_VALS = {"inlined", "attachment"};

    
    /** Describes the attributes of this element.
     */
    public interface ATTR {
        
        /** "ignoreMissingFromData" attribute token */
        public static final String IGNORE_MISSING_FROM_DATA = "ignoreMissingFromData";
        
        /** "binaryCopy" attribute token */
        public static final String BINARY_COPY = "binaryCopy";
    
    }
    
    /* we don't support validate yet. Until then this is at position 0 */
    /** Ordinal position of ignoreMissingFromData attribute */
    public static final int IGNORE_MISSING_FROM_DATA = 0;
    
    /** ordinal position of binaryCopy attribute */
    public static final int BINARY_COPY = IGNORE_MISSING_FROM_DATA + 1;
    
    /** Getter for element from.
     * @return Value of the element.
     *
     */
    From getFrom();
    
    /** Setter for element from.
     * @param f New value of the element.
     *
     */
    void setFrom(From f);
    
    /** Getter for element to.
     * @return Value of the element.
     *
     */
    To getTo();
    
    /** Setter for element to.
     * @param t New value of the element.
     *
     */
    void setTo(To t);
    
    /** Getter for property faultName.
     * @return Value of property faultName.
     *
     */
    String getIgnoreMissingFromData();
    
    /** Setter for property faultName.
     * @param faultName New value of property faultName.
     *
     */
    void setIgnoreMissingFromData(String ignoreMissingFromData);
    
    /**
     * Getter for the value of the SUN Ext attribute binaryCopy
     * @return value of the attribute binaryCopy
     */
    String getBinaryCopy();
    
    /**
     * Setter for the value of the SUN Ext attribute binaryCopy
     * @param binaryCopy, new value of the attribute 
     */
    void setBinaryCopy(String binaryCopy);
}
