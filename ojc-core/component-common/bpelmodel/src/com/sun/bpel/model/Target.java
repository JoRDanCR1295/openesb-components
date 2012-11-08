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
 * @(#)Target.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 * Describes the &lt;target&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Target extends BPELElement {
    
    /** Tag for this element. */
    public static final String TAG = "target";
    
    /** Describes attributes of this element.
     */
    public interface ATTR {
        
        /** "linkName" attribute token */
        public static final String LINK_NAME = "linkName";
    }
    /** Ordinal position of linkName attribute */
    public static final int LINK_NAME = 0;
    
    /** Getter for property linkName.
     * @return Value of property linkName.
     *
     */
    String getLinkName();
    
    /** Setter for property linkName.
     * @param linkName New value of property linkName.
     *
     */
    void setLinkName(String linkName);
    
    /**
     * get the Link for this Target.
     * @return Link
     */
    Link getBPELLink();
    
    /**
     * set the Link for this Source.
     * @param link Link
     */
    void setBPELLink(Link link);
    
}
