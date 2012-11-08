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
 * @(#)Correlation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;correlation&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Correlation extends BPELElement {
    
    /** Tag for this element */
    public static final String TAG = "correlation";
    final String INITIATE_YES = "yes";
    final String INITIATE_JOIN = "join";
    final String INITIATE_NO = "no";
    
    String PATTERN_REQUEST = "request";
    String PATTERN_RESPONSE = "response";
    String PATTERN_REQ_RESP = "request-response";    
    
    /** Describes the attributes of this element.
     */
    public interface ATTR {
        
        /** "set" attribute token */
        public static final String SET = "set";
        
        /** "initiation" attribute token */
        public static final String INITIATE = "initiate";
        
        /** "pattern" attribute token */
        public static final String PATTERN = "pattern";
        
        public static final String[] INITIATE_ENUM_VALS 
            = {INITIATE_YES, INITIATE_JOIN, INITIATE_NO};
        
        public static final String[] PATTERN_ENUM_VALS = {PATTERN_REQUEST, PATTERN_RESPONSE, 
            PATTERN_REQ_RESP};
    }
    /** Ordinal position of set attribute */
    public static final int SET = 0;
    
    /** Ordinal position of initiation attribute */
    public static final int INITIATE = SET + 1;
    
    /** Ordinal position of pattern attribute */
    public static final int PATTERN = INITIATE + 1;
    
    /** Getter for property set.
     * @return Value of property set.
     *
     */
    String getSet();
    
    /** Setter for property set.
     * @param set New value of property set.
     *
     */
    void setSet(String set);
    
    /** Setter for property set.
     * @param qName New qName of property set.
     * @param set   New value of property set.
     *
     */
    void setSet(String qName, String set);
    
    /** Getter for property initiation.
     * @return Value of property initiation.
     *
     */
    String getInitiate();
    
    /** Setter for property initiation.
     * @param initiation New value of property initiation.
     *
     */
    void setInitiate(String initiation);
    
    /** Getter for property pattern.
     * @return Value of property pattern.
     *
     */
    String getPattern();
    
    /** Setter for property pattern.
     * @param pattern New value of property pattern.
     *
     */
    void setPattern(String pattern);
    
    /** Setter for property pattern.
     * @param qName        New qName of property pattern.
     * @param pattern   New value of property pattern.
     *
     */
    void setPattern(String qName, String pattern);
    
    /**
     * @return the CorrelationSet object corresponding to the set name from getSet() API
     */
    CorrelationSet getBPELCorrelationSet();
    
}
