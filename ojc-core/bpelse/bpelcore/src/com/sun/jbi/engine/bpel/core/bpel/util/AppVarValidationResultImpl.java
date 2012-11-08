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
package com.sun.jbi.engine.bpel.core.bpel.util;

import com.sun.bpel.model.BPELProcess;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author Vitaly Bychkov
 */
public class AppVarValidationResultImpl implements AppVarValidationResult {

    private BPELProcess source;
    private Set<String> value;
    private Collection<Object> errors;
    private Set<AppVarValueValidationResult> appVarValResults;

    public AppVarValidationResultImpl(BPELProcess process,
            Set<String> unknownVars,
            Set<AppVarValueValidationResult> appVarValResults)
    {
        this.source = process;
        this.value = unknownVars;
        this.appVarValResults = appVarValResults;
        initErrors();
    }

    public BPELProcess getSource() {
        return source;
    }

    public Object getValue() {
        return value == null ? Collections.EMPTY_SET : value;
    }

    public Collection<Object> getErrors() {
        return errors;
    }

    private void initErrors() {
        Set<String> unknownVars = (Set<String>)getValue();
        Set<AppVarValueValidationResult> valResults = getValueValResult();

        if (!unknownVars.isEmpty()) {
            errors = new ArrayList<Object>();

            Iterator<String> uvIterator = unknownVars.iterator();
            StringBuilder listUVars = new StringBuilder(uvIterator.next());
            while(uvIterator.hasNext()) {
                listUVars.append(COMA).append(uvIterator.next());
            }
            errors.add(I18n.loc("BPCOR-7025: Bpel Process: ''{0}''; unknown variables: [{1}]", getSource().getName(), listUVars));
        }

        if (!valResults.isEmpty()) {
            errors = errors == null ? new ArrayList<Object>() : errors;
            for (AppVarValueValidationResult valResult : valResults) {
                errors.addAll(valResult.getErrors());
            }
        }

        if (errors == null) {
            errors = Collections.EMPTY_LIST;
        }
    }

    public Set<AppVarValueValidationResult> getValueValResult() {
        return appVarValResults == null ? Collections.EMPTY_SET : appVarValResults;
    }

    @Override
    public String toString() {
        StringBuilder errorMsgs = new StringBuilder();
        Collection<Object> errs = getErrors();
        if (errs != null && !errs.isEmpty()) {
            Iterator<Object> errorIt = errs.iterator();
            errorMsgs.append(errorIt.next().toString());
            while (errorIt.hasNext()) {
                errorMsgs.append(COMA).append(errorIt.next().toString());
            }
        }
        return errorMsgs.toString();
    }


}
