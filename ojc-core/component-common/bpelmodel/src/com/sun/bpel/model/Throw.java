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
 * @(#)Throw.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.xml.namespace.QName;

/**
 * Describes the &lt;throw&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Throw extends Activity {
    
    /** Tag for this element. */
    public static final String TAG = Tags.THROW;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "faultName" attribute token */
        public static final String FAULT_NAME = "faultName";
        
        /** "faultVariable" attribute token */
        public static final String FAULT_VARIABLE = "faultVariable";
    }
    
    /** Ordinal position of faultName attribute. */
    public static final int FAULT_NAME = NUM_STANDARD_ATTRS;
    
    /** Ordinal position of faultVariable attribute. */
    public static final int FAULT_VARIABLE = FAULT_NAME + 1;

    /** Total number of attributes */
    public static final int NUM_ATTRS = FAULT_VARIABLE + 1;
        
    /** Getter for attribute faultName.
     * @return Value of attribute faultName.
     *
     */
    QName getFaultName();
    
    /** Setter for attribute faultName.
     * @param faultName New value of attribute faultName.
     *
     */
    void setFaultName(QName faultQName);
    
    
    /** Getter for attribute faultVariable.
     * @return Value of attribute faultVariable.
     *
     */
    String getFaultVariable();
    
    /** Setter for attribute faultVariable.
     * @param faultVariable New value of attribute faultVariable.
     *
     */
    void setFaultVariable(String faultVariable);
    
    /**
     * get fault variable
     * @return Variable
     */
    Variable getBPELFaultVariable();
    
    /**
     * set fault variable
     * @param variable Variable
     */
    void setBPELFaultVariable(Variable variable);
    
   
}
