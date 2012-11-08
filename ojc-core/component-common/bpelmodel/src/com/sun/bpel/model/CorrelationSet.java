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
 * @(#)CorrelationSet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.Collection;
import javax.xml.namespace.QName;

/**
 * Describes the &lt;correlationSet&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface CorrelationSet extends BPELElement, NamedElement {
    
    /** Tag for this element */
    public static final String TAG = "correlationSet";
    
    /** Describes attributes of this element.
     */
    public interface ATTR extends NamedElement.ATTR {
        /** "properties" attribute token */
        public static final String PROPERTIES = "properties";
    }
    
    /** Ordinal position of properties attribute */
    public static final int PROPERTIES = NAME + 1;
    
    
    /**
     * Get properties of this corrleation set as QNames
     * @return properties as QName
     */
    QName[] getProperties();
    
    /**
     * set properties as QNames
     * @param properties
     */
    void setProperties(QName[] properties);
    
    /**
     * Get Properties object for each QName property
     * @return collection of actual properties object
     * which a property QName refers to
     */
    Collection getBPELProperties();
        
    /**
     * set Collection of Property objects.
     * @param properties
     */
    void setBPELProperties(Collection properties);
}
