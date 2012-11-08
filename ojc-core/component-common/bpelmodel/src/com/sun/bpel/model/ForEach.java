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
 * @(#)ForEach.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.wsdl4j.ext.DeferredActionAccepter;

/**
 *
 * @author Sun Microsystems
 */
public interface ForEach extends Activity, SingleActivityHolder, VariableScope,
        DeferredActionAccepter {
    /** Tag for this element. */
    public static final String TAG = "forEach";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "counterName" attribute token */
        public static final String COUNTERNAME = "counterName";
        
        /** "parallel" attribute token */
        public static final String PARALLEL = "parallel";
    }
    
    public static final int COUNTERNAME = NUM_STANDARD_ATTRS;
    
    public static final int PARALLEL = COUNTERNAME + 1;

    /** Total number of attributes */
    public static final int NUM_ATTRS = PARALLEL + 1;
    
    String getCounterName();
    
    void setCounterName(String counterName);
    
    String getParallel();
    
    void setParallel(String parallel);
    
    Iterator getIterator();
    
    void setIterator(Iterator it);
    
    CompletionCondition getCompletionCondition();
    
    void setCompletionCondition(CompletionCondition cCondition);

    /**
     * Return the BPEL variable for counterName
     * @return
     */
    Variable getCounterVariable(); 
    
    /**
     * Initializes members
     */    
    void initMembers();
    
    /**
     * Initializes members locally without resolving to XSD artifacts.
     */    
    void initMembersLocally();
}
