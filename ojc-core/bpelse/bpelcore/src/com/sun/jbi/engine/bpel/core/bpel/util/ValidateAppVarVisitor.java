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
 * @(#)ValidateVisitor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import com.sun.bpel.model.For;
import com.sun.bpel.model.Until;
import java.util.logging.Logger;
import com.sun.bpel.model.Else;
import com.sun.bpel.model.ElseIf;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.SunExtExpression;
import com.sun.bpel.model.From;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.visitor.AbstractVisitor;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.visitor.ParentChildrenVisitor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;

/**
 * Visits all the model nodes and validates that all used application variables are available.
 * @author Vitaly Bychkov
 */
public class ValidateAppVarVisitor extends AbstractVisitor implements ParentChildrenVisitor {

    private static final Logger LOGGER = Logger.getLogger(ValidateAppVarVisitor.class.getName());
    private Set<String> mAppVars;
    private Map<String,Object> mAppVarValues;
    private Set<String> mUsedAppVars;
    private Set<String> mUnknownVars;
    private Set<AppVarValueValidationResult> mAppVarValueValResult;
    private boolean mIsValidateValue = false;
    private static final String COMA = ", ";
    private static final String QUOTE = "'";

    /** Creates a new instance of ValidateAppVarVisitor */
    public ValidateAppVarVisitor() {
        super();
        mUsedAppVars = new HashSet<String>();
    }

    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>java.util.Set</code> of <code>java.lang.String</code>
     *              objects.</li>
     *              <li><code>java.util.Set</code> of <code>java.lang.String</code>
     *              objects.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        assert v != null;
        assert v.length == 3 || v.length == 2;
        assert v[0] instanceof Set || v[0] instanceof Map;
        assert v[1] instanceof Set;
        if (v[0] instanceof Map) {
            mAppVarValues = (Map<String, Object>) v[0];
            mAppVars = mAppVarValues.keySet();
        } else {
            mAppVars = (Set<String>) v[0];
        }

        mUnknownVars = (Set<String>) v[1];

        //
        if (v.length == 3) {
            assert v[2] instanceof Set;
            mIsValidateValue = mAppVarValues != null;
            mAppVarValueValResult = (Set<AppVarValueValidationResult>)v[2];
        }
    }

    @Override
    public boolean reset() {
        if (mAppVars != null) {
            mUnknownVars.clear();
        }
        if (mUsedAppVars != null) {
            mUsedAppVars.clear();
        }
        if (mUnknownVars != null) {
            mUnknownVars.clear();
        }
        return true;
    }

    public boolean visit(ElseIf d) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(Else d) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(ExtensionAssignOperation extensionAssignOperation) {
        // TODO Auto-generated method stub
        return true;
    }

    public boolean visit(SunExtExpression expression) {
        // TODO Auto-generated method stub
        return true;
    }

    @Override
    public boolean visit(For f) {
        Set<String> usedVars = new HashSet<String>();
        String dateTimeValue = f.getValue();
        try {
            usedVars = RApplicationVariablesHelper.getUsedAppVariables(dateTimeValue);
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, null, ex);
        }
        
        if (!usedVars.isEmpty()) {
            Set<String> unknownVars = RApplicationVariablesHelper.getUnregistredVars(usedVars, mAppVars);
            if (unknownVars != null || !unknownVars.isEmpty()) {
                mUnknownVars.addAll(unknownVars);
            }
        }
        
        // initiate value validation
        if(mIsValidateValue && !usedVars.isEmpty()) {
            try {
                AppVarValueValidationResult result = validateDateTimeExpr(f, usedVars,
                        RApplicationVariablesHelper.replaceAppVar2Value(dateTimeValue, mAppVarValues),
                        dateTimeValue, true);
                if (result != null) {
                    
                    mAppVarValueValResult.add(result);
                }
            } catch (Exception ex) {
                Logger.getLogger(ValidateAppVarVisitor.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        return true;
    }

    @Override
    public boolean visit(Until d) {
        Set<String> usedVars = new HashSet<String>();
        String dateTimeValue = d.getValue();
        try {
            usedVars = RApplicationVariablesHelper.getUsedAppVariables(dateTimeValue);
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, null, ex);
        }
        if (usedVars != null && !usedVars.isEmpty()) {
            Set<String> unknownVars = RApplicationVariablesHelper.getUnregistredVars(usedVars, mAppVars);
            if (unknownVars != null || !unknownVars.isEmpty()) {
                mUnknownVars.addAll(unknownVars);
            }
        }

        // initiate value validation
        if(mIsValidateValue && !usedVars.isEmpty()) {
            try {
                AppVarValueValidationResult result = validateDateTimeExpr(d, usedVars,
                        RApplicationVariablesHelper.replaceAppVar2Value(dateTimeValue, mAppVarValues),
                        dateTimeValue, false);
                if (result != null) {

                    mAppVarValueValResult.add(result);
                }
            } catch (Exception ex) {
                Logger.getLogger(ValidateAppVarVisitor.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return true;
    }

    @Override
    public boolean visit(From f) {
        Literal lit = f.getLiteral();
        if (lit == null) {
            return true;
        }

        Set<String> usedVars = null;

        try {
            if (lit.getEII() != null) {
                usedVars = RApplicationVariablesHelper.getUsedAppVariables(lit.getEII());
            } else {
                usedVars = RApplicationVariablesHelper.getUsedAppVariables(lit.getValue());
            }
        } catch (Exception ex) {
            LOGGER.log(Level.SEVERE, null, ex);
        }
        
        if (usedVars != null && !usedVars.isEmpty()) {
            Set<String> unknownVars = RApplicationVariablesHelper.getUnregistredVars(usedVars, mAppVars);
            if (unknownVars != null || !unknownVars.isEmpty()) {
                mUnknownVars.addAll(unknownVars);
            }
        }
        return true;
    }

    private AppVarValueValidationResult validateDateTimeExpr(XMLElement element, Set<String> usedVars,
            String modExpr, String origExpr, boolean isFor)
    {
        assert usedVars != null && !usedVars.isEmpty();
        if (!isAcceptableDateTimeExpr(modExpr)) {
            return null;
        }

        //remove quotes from begining and end of expr
        if (modExpr.startsWith(QUOTE) && modExpr.endsWith(QUOTE)) {
            modExpr = modExpr.substring(1, modExpr.length()-1);
        }

        AppVarValueValidationResult result = null;
        try {
            if (!isFor) {
                // <wait until="...">
                DateTime.parse(modExpr);
            } else {
                // <wait for="...">
                // presettedDateTime mostly used in tests
                Duration dur = Duration.parse(modExpr);
            }
        } catch (RuntimeException ex) {
            // Unfortunately DateTime and Duration parser throws RuntimeException and not a specific parser exception.
            // Because of that we can't differentiate a parse problem or a code problem and we convert all of them
            // to  InvalidExpressionValue fault
            Iterator<String> usedVarsIter = usedVars.iterator();
            String tmpUsedVar = usedVarsIter.next();
            StringBuilder usedVarAsString = new StringBuilder(tmpUsedVar);
            Map<String, Object> usedVarValues = new HashMap<String, Object>();

            while (usedVarsIter.hasNext()) {
                tmpUsedVar = usedVarsIter.next();
                usedVarValues.put(tmpUsedVar, mAppVarValues.get(tmpUsedVar));
                usedVarAsString.append(COMA).append(tmpUsedVar);
            }
            

            String resultMsg = I18n.loc("BPCOR-7027: After substitution of application variables [{0}] the resulted expression ''{1}'' at line number {2} " +
                    "is invalid: {3}. Associated BPEL artifact is: {4}", usedVarAsString,
                    modExpr ,element.getLocator().getLineNumber(), ex.getMessage(), element);


            result = new AppVarValueValidationResultImpl(element, modExpr, usedVarValues, resultMsg);
            LOGGER.log(Level.WARNING, resultMsg);
        }

        return result;
    }

    private boolean isAcceptableDateTimeExpr(String expr) {
        if (expr == null) {
            return false;
        }
        return expr.indexOf("$") == -1;
    }

}
