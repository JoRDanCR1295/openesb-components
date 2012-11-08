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

import com.sun.bpel.xml.common.model.XMLElement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

/**
 *
 * @author Vitaly Bychkov
 */
public class AppVarValueValidationResultImpl implements AppVarValueValidationResult {

    private XMLElement source;
    private String value;
    private Map<String, Object> usedAppVars;
    private Collection<Object> errors;

    public AppVarValueValidationResultImpl(XMLElement source,
            String expr, Map<String, Object> usedAppVars, String errorMsg)
    {
        this.source = source;
        this.value = expr;
        this.errors = new ArrayList<Object>();
        errors.add(errorMsg);
        this.usedAppVars = usedAppVars;
    }

    public XMLElement getSource() {
        return source;
    }

    public Object getValue() {
        return value;
    }

    public Collection<Object> getErrors() {
        return errors;
    }

    public Map<String, Object> getUsedAppVars() {
        return usedAppVars;
    }

}
