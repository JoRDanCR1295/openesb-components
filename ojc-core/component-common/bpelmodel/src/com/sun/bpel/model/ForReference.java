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
 * @(#)ForReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.xml.common.model.XMLElement;

/**
 *
 * @author Sun Microsystems
 */
public interface ForReference extends XMLElement {
    
    /** "for" wait type */
    public static final String TYPE_FOR = "for";
    
    /** Getter for the for attribute.
     * @return  Value of the for attribute.
     */
    String getFor();
    
    /** Setter for the for attribute.
     * @param   f   Value of the for attribute.
     */
    void setFor(String f);
    
    
    /**
     *  set the For child element for this onAlarm.
     * @param f for child element.
     */
    void setBPELFor(For f);
    
    /**
     * get For child element of this onAlarm.
     * @return For
     */
    For getBPELFor();
    
}
