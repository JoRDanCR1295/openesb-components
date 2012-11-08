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
 * @(#)CompensateScope.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.bpel.model;

import com.sun.bpel.model.Activity.Tags;

public interface CompensateScope extends Activity {

	/** Tag for this element */
    public static final String TAG = Tags.COMPENSATESCOPE;
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "scope" attribute token */
        public static final String TARGET = "target";
    }
    /** Ordinal position of target attribute. */
    public static final int TARGET = NUM_STANDARD_ATTRS;

    /** Total number of attributes */
    public static final int NUM_ATTRS = TARGET + 1;
    
    /** Getter for the target attribute.
     * @return  Value of target attribute.
     */
    String getTarget();
    
    /** Setter for the target attribute.
     * @param   s   Value of scope attribute.
     */
    void setTarget(String s);
	

}
