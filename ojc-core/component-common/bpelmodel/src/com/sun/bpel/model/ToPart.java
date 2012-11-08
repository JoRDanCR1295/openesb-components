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
 * @(#)ToPart.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.wsdl.Part;

/**
 *
 * @author Sun Microsystems
 */
public interface ToPart extends BPELElement {
    /** Tag for this element. */
    public static final String TAG = "toPart";
    
    /** Describes the attributes for this element.
     */
    public interface ATTR extends Activity.ATTR {
        
        /** "part" attribute token */
        public static final String PART = "part";
        
        /** "fromVariable" attribute token */
        public static final String FROM_VARIABLE = "fromVariable";
    }
    
    /** Ordinal position of the part attribute */
    public static final int PART = 0;
    
    /** Ordinal position of the fromVariable attribute */
    public static final int FROM_VARIABLE = PART + 1;
    
    String getPart();
    
    void setPart(String part);
    
    String getFromVariable();
    
    void setFromVariable(String variable);
    
    Part getWSDLPart();
    
    Variable getBPELFromVariable();
}
