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
 * @(#)VariableReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

/**
 *
 * @author Sun Microsystems
 */
public interface VariableReference {
    
    public interface ATTR {
        /** "variable" attribute token */
        public static final String VARIABLE = "variable";
    }
    
    /** Getter for variable attribute.
     * @return  Value of variable attribute.
     */
    String getVariable();
    
    /** Setter for variable attribute.
     * @param   c   Value of variable attribute.
     */
    void setVariable(String c);
    
    /** Getter for property variable.
     * @return Value of property variable.
     *
     */
    Variable getBPELVariable();
    
    /** Setter for property variable.
     * @param container New value of property variable.
     *
     */
    void setBPELVariable(Variable container);
    
}
