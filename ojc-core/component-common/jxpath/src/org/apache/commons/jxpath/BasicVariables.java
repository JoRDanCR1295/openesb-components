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
 * @(#)BasicVariables.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.HashMap;

/**
 * A basic implementation of the Variables interface that uses a HashMap.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class BasicVariables implements Variables {

    /**
     * Contains the values of declared variables
     */
    private HashMap vars = new HashMap();

    /**
     * Returns true if the variable has been defined, even if the
     * value of the variable is null.
     *
     * @param varName is a variable name without the "$" sign
     *
     * @return true if the variable is declared
     */
    public boolean isDeclaredVariable(String varName) {
        return vars.containsKey(varName);
    }

    /**
     * Returns the value of the variable if it is defined,
     * otherwise, throws IllegalArgumentException
     *
     * @param varName is a variable name without the "$" sign
     *
     * @return the value of the variable
     */
    public Object getVariable(String varName) {
        // Note that a variable may be defined with a null value

        if (vars.containsKey(varName)) {
            return vars.get(varName);
        }

        throw new IllegalArgumentException(
            "No such variable: '" + varName + "'");
    }

    /**
     * Defines a new variable with the specified value or modifies
     * the value of an existing variable.
     *
     * @param varName is a variable name without the "$" sign
     * @param value is the new value for the variable, which can be null
     */
    public void declareVariable(String varName, Object value) {
        vars.put(varName, value);
    }

    /**
     * Removes an existing variable. May throw UnsupportedOperationException.
     *
     * @param varName is a variable name without the "$" sign
     */
    public void undeclareVariable(String varName) {
        vars.remove(varName);
    }
    
    public String toString() {
        return vars.toString();
    }
}
