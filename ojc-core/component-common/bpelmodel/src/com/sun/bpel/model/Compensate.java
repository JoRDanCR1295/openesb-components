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
 * @(#)Compensate.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;compensate&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Compensate extends Activity {
    /** Tag for this element */
    public static final String TAG = Tags.COMPENSATE;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "scope" attribute token */
        public static final String SCOPE = "scope";
    }
    /** Ordinal position of scope attribute. */
    public static final int SCOPE = NUM_STANDARD_ATTRS;;
    
    /** Total number of attributes */
    public static final int NUM_ATTRS = SCOPE + 1;
    
    /** Getter for the scope attribute.
     * @return  Value of scope attribute.
     */
    String getScope();
    
    /** Setter for the scope attribute.
     * @param   s   Value of scope attribute.
     */
    void setScope(String s);
    
}
