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
 * @(#)Scope.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;scope&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Scope extends Activity, 
                               SingleActivityHolder, 
                               BPELProcessOrScope {
    
    /** Tag for this element. */
    public static final String TAG = Tags.SCOPE;
    
    /** Describes the attributes of this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "isolated" attribute token */
        public static final String ISOLATED =
            "isolated";
    }
    /** Ordinal position of isolated attribute. */
    public static final int ISOLATED = NUM_STANDARD_ATTRS;

    /** Total number of attributes */
    public static final int NUM_ATTRS = ISOLATED + 1;
    
    /** Getter for isolated attribute.
     * @return  Value of isolated attribute.
     */
    String getIsolated();
    
    
    /** Setter for isolated attribute.
     * @param   c   Value of isolated attribute.
     */
    void setIsolated(String c);
    
    /** Getter for sub-element terminationHandler.
     * @return Value of sub-element terminationHandler.
     *
     */
    TerminationHandler getTerminationHandler();
    
    /** Setter for sub-element terminationHandler.
     * @param compensationHandler New value of sub-element terminationHandler.
     *
     */
    void setTerminationHandler(TerminationHandler terminationHandler);
    
}
