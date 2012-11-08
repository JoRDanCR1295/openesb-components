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
 * @(#)RExpressionElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta;

import java.util.Iterator;

import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;




/**
 * It is implemented by BPELElement s that hold xpath expressions.
 * Parser sets the expression while loading BPEL document.
 * Runtime reads the variables to set it on JXPath context 
 *
 * @author Sun Microsystems
 * @version 
 */
public interface RExpressionElement {
    /**
     * Get the list of xpath variables
     * @return
     */
    Iterator getVariables();

    /**
     * Set xpath expression
     * @param expression
     */
    void setXPathExpression(String expression) throws Exception;
    
    MessagePropertyAlias getPropertyAliasForVariableProperty(String key);
}
