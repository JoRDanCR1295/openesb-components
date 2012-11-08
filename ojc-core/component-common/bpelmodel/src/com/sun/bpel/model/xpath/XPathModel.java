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
 * @(#)XPathModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.xpath;


/**
 * Interface for an XPath parser wrapper.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XPathModel {

    /** XPath function names that are not considered part of the core. */
    String[] VALID_FUNCTION_NAMES = {
        "unparsed-entity-uri", "system-property",
        "string-literal", "element-available", "function-available",
        "document", "current", "generate-id", "stringToBytes", "bytesToString",
        "getCurrentTime", "getGUID", "getBPId", "convert", "incrementDatetime",
        "decrementDatetime", "exists"
    };
    
    /** BPEL4WS extensions wihtout the namespace prefix. */
    String[] VALID_BPWS_FUNCTION_NAMES = {
        "getContainerData", "getContainerProperty", "getLinkStatus"
    };

    /**
     * Parses an XPath expression.
     * @param expression the XPath expression to parse
     * @return an instance of XPathExpression
     * @throws XPathException for any parsing errors
     */
    XPathExpression parseExpression(String expression)
        throws XPathException;
}
