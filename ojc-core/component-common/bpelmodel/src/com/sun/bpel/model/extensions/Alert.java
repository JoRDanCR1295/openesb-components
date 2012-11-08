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
package com.sun.bpel.model.extensions;

import com.sun.bpel.model.From;
import com.sun.bpel.xml.common.model.XMLElement;

/*
 * @(#)Alert.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
public interface Alert extends XMLElement {

	/** Tag for this element. */
    public static final String TAG = "alert";

    /** Describes attributes for this element.
     */
    public interface ATTR {

        /** "level" attribute token */
        public static final String LEVEL = "level";
        
        /** "location" attribute token */
        public static final String LOCATION = "location";
    }
    
    /** Ordinal position for level attribute */
    public static final int LEVEL = 0;
    
    /** Ordinal position for location attribute */
    public static final int LOCATION = LEVEL + 1;
    
    /** */
    public static final String INFO = "info";
    
    /** */
    public static final String WARNING = "warning";
    
    /** */
    public static final String MINOR = "minor";
    
    /** */
    public static final String MAJOR = "major";
    
    /** */
    public static final String CRITICAL = "critical";
    
    /** */
    //public static final String FATAL = "fatal";
    
    /** */
    public static final String DEFAULT_LEVEL = INFO;
    
    /** */
    public static final String DEFAULT_LOCATION = Trace.ON_COMPLETE;
    
    /**
     * 
     * @param level
     */
    public void setLevel(String level);
    
    /**
     * 
     * @param location
     */
    public void setLocation(String location);
    
    /**
     * 
     * @param from
     */
    public void setFrom(From from);
    
    /**
     * 
     * @return
     */
    public String getLevel();
    
    /**
     * 
     * @return
     */
    public String getLocation();
    
    /**
     * 
     * @return
     */
    public From getFrom();
	
}
