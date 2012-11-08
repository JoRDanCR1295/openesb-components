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
 * @(#)Variables.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

/**
 * Variables provide access to a global set of values accessible via XPath.
 * XPath can reference variables using the <code>"$varname"</code> syntax.
 * To use a custom implementation of this interface, pass it to
 * {@link JXPathContext#setVariables JXPathContext.setVariables()}
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public interface Variables {

    /**
     * Returns true if the specified variable is declared.
     */
    boolean isDeclaredVariable(String varName);

    /**
     * Returns the value of the specified variable.
     * Throws IllegalArgumentException if there is no such variable.
     */
    Object getVariable(String varName);

    /**
     * Defines a new variable with the specified value or modifies
     * the value of an existing variable.
     * May throw UnsupportedOperationException.
     */
    void declareVariable(String varName, Object value);

    /**
     * Removes an existing variable. May throw UnsupportedOperationException.
     *
     * @param varName is a variable name without the "$" sign
     */
    void undeclareVariable(String varName);
}
