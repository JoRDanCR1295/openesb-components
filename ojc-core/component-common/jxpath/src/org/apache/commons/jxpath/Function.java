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
 * @(#)Function.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * Extension function interface. Extension functions are grouped into
 * {@link Functions Functions} objects, which are installed on
 * JXPathContexts using the 
 * {@link JXPathContext#setFunctions JXPathContext.setFunctions()}
 * call.
 * <p>
 * The Function interface can be implemented directly. However,
 * most of the time JXPath's built-in implementations should suffice.
 * See {@link ClassFunctions ClassFunctions} and 
 * {@link PackageFunctions PackageFunctions}.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface Function {

    /**
     * Computes the value of the function. Each implementation of Function
     * is responsible for conversion of supplied parameters to the required
     * argument types.
     *
     * @param context can be used to acquire the context in which the
     *    function is being evaluted.
     */
    Object invoke(ExpressionContext context, Object[] parameters);
}
