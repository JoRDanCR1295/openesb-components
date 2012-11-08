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
 * @(#)Expression.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import com.sun.bpel.xml.common.model.XMLCharacterData;
import com.sun.bpel.xml.common.model.XMLElement;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public interface Expression extends XMLElement, XMLCharacterData {
	
    /** Describes attributes of this element.
     */
    public interface ATTR {
        
        /** "expressionLanguage" attribute token */
        public static final String EXPRESSIONLANGUAGE = "expressionLanguage";
    }
    
    /** Ordinal position of expressionLanguage attribute. */
    public static final int EXPRESSIONLANGUAGE = 0;
    
    /**
     * getter for attribute expressionLanguage
     * @return expressionLanguage
     */
    String getExpressionLanguage();
    
    /**
     * setter for attribute expressionLanguage 
     * @param expressionLanguage
     */
    void setExpressionLanguage(String expressionLanguage);
}
