/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 * @(#)GetVariableProperty.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * bpel:getVariableProperty('variable NCName', 'property QName').
 * implements the processing of this bpel xpath function. 
 * 
 * @author <a href="mailto:pVarghese@PVARGHESE-HP">Sun Microsystems</a>
 * @version 1.0
 */
public class GetVariableProperty implements Function {

    private VariableScope mVariableScope = null;
    private BPELElement mElement = null;


    public GetVariableProperty(VariableScope variableScope, BPELElement element) {
        mVariableScope = variableScope;
        mElement = element;
    }

    /*
     * (non-Javadoc)
     * @see org.apache.commons.jxpath.Function#invoke(org.apache.commons.jxpath.ExpressionContext, java.lang.Object[])
     */
    public Object invoke(ExpressionContext context, Object[] parameters) {
        // validation of the parameters is done on function creation. 
        String varName = (String) parameters[0];
        String propName = (String) parameters[1];

        RVariable variable = (RVariable) BPELHelper.getMatchingVariable(varName, mElement);
        RuntimeVariable var = mVariableScope.getRuntimeVariable(variable); 

        return Utility.evaluateProperty(mElement, varName, propName, var);
    }

}
