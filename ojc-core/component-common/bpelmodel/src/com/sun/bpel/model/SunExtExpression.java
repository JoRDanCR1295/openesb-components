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
public interface SunExtExpression extends XMLElement, XMLCharacterData {
    /** Tag for this element */
    public static final String TAG = "Expression";

    /** Describes the attributes of this element.
     */
    public interface ATTR {

        public static final String EXPRESSION_LANGUAGE = "expressionLanguage" ;
        public static final String INPUT_VARS = "inputVars";
        public static final String OUTPUT_VARS = "outputVars";

    }

    /** Ordinal position of expressionLanguage attribute */
    public static final int EXPRESSION_LANGUAGE = 0;

    /** ordinal position of inputVar attribute */
    public static final int INPUT_VAR = EXPRESSION_LANGUAGE + 1;

    /** ordinal position of outputVar attribute */
    public static final int OUTPUT_VAR = INPUT_VAR + 1;

    /** Getter for expression attribute.
     * @return  expression attribute.
     */
    String getExpression();

    /** Setter for expression attribute.
     * @param   e   expression attribute.
     */
    void setExpression(String e);

    /** Getter for property expressionLanguage.
     * @return Value of property expressionLanguage.
     *
     */
    public String getExpressionLanguage();

    /** Setter for property expressionLanguage.
     * @param expressionLanguage New value of property.
     *
     */
    public void setExpressionLanguage(String expressionLanguage);

    /** Getter for property inputVar.
     * @return Value of property inputVar.
     *
     */
    public String getInputVars();

    /** Setter for property inputVars.
     * @param inputVars New value of property.
     *
     */
    public void setInputVars(String inputVars);

    /** Getter for property outputVars.
     * @return Value of property outputVars.
     *
     */
    public String getOutputVars();

    /** Setter for property outputVars.
     * @param outputVars New value of property.
     *
     */
    public void setOutputVars(String outputVars);

}
