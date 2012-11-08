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
 * @(#)Flow.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


/**
 * Describes the &lt;flow&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Flow extends Activity, MultipleActivityHolder {
    
    /** Tag for this element */
    public static final String TAG = Tags.FLOW;
    
    /** Getter for the links element.
     * @return  links element.
     */
    Links getLinks();
    
    /** Setter for the links element.
     * @param   l   links element
     */
    void setLinks(Links l);
    
    /**
     * Get a link given link name.
     * @param linkName nae of the link
     * @return Link or null if not found
     */
    Link getLink(String linkName);
}
