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
 * @(#)XPathCoreFunction.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath;

/**
 * Represents a core XPath function.
 * 
 * @author Sun Microsystems
 * @version 
 */
public interface XPathCoreFunction extends XPathExpression {
            
    /** Function code: last */
    public static final int FUNC_LAST = 1;
    
    /** Function code: position */
    public static final int FUNC_POSITION = 2;
    
    /** Function code: count */
    public static final int FUNC_COUNT = 3;
    
    /** Function code: id */
    public static final int FUNC_ID = 4;
    
    /** Function code: local-name */
    public static final int FUNC_LOCAL_NAME = 5;
    
    /** Function code: namespace-uri */
    public static final int FUNC_NAMESPACE_URI = 6;
    
    /** Function code: name */
    public static final int FUNC_NAME = 7;
    
    /** Function code: string */
    public static final int FUNC_STRING = 8;
    
    /** Function code: concat */
    public static final int FUNC_CONCAT = 9;
    
    /** Function code: starts-with */
    public static final int FUNC_STARTS_WITH = 10;
    
    /** Function code: contains */
    public static final int FUNC_CONTAINS = 11;
    
    /** Function code: substring-before */
    public static final int FUNC_SUBSTRING_BEFORE = 12;
    
    /** Function code: substring-after */
    public static final int FUNC_SUBSTRING_AFTER = 13;
    
    /** Function code: substring */
    public static final int FUNC_SUBSTRING = 14;
    
    /** Function code: string-length */
    public static final int FUNC_STRING_LENGTH = 15;
    
    /** Function code: normalize-space */
    public static final int FUNC_NORMALIZE_SPACE = 16;
    
    /** Function code: translate */
    public static final int FUNC_TRANSLATE = 17;
    
    /** Function code: boolean */
    public static final int FUNC_BOOLEAN = 18;
    
    /** Function code: not */
    public static final int FUNC_NOT = 19;
    
    /** Function code: true */
    public static final int FUNC_TRUE = 20;
    
    /** Function code: false */
    public static final int FUNC_FALSE = 21;
    
    /** Function code: lang */
    public static final int FUNC_LANG = 22;
    
    /** Function code: number */
    public static final int FUNC_NUMBER = 23;
    
    /** Function code: sum */
    public static final int FUNC_SUM = 24;
    
    /** Function code: floor */
    public static final int FUNC_FLOOR = 25;
    
    /** Function code: ceiling */
    public static final int FUNC_CEILING = 26;
    
    /** Function code: round */
    public static final int FUNC_ROUND = 27;
    
    /** Function code: null */
    public static final int FUNC_NULL = 28;
    
    /** Function code: key */
    public static final int FUNC_KEY = 29;
    
    /** Function code: format-number */
    public static final int FUNC_FORMAT_NUMBER = 30;
    
    /** Function code: exists */
    public static final int FUNC_EXISTS = 31;
    
    
    /**
     * Gets the function code.
     * @return the function code
     */
    int getFunction();
    
    
    /**
     * Sets the function code.
     * @param function the function code
     */
    void setFunction(int function);
    
    
    /**
     * Gets the name of the function.
     * @return the function name
     */
    String getName();
}
