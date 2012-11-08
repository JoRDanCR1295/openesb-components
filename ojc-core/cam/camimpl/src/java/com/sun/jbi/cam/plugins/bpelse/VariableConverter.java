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
 * @(#)VariableConverter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.plugins.bpelse.datamodel.Variable;
import java.util.StringTokenizer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

/**
 *
 * @author nnahata
 */
public class VariableConverter implements Converter {
    
    /** Creates a new instance of VariableConverter */
    public VariableConverter() {
    }

    public Object getAsObject(FacesContext facesContext, UIComponent uIComponent, String string) {
        if (string == null) {
            return null;
        }
        com.sun.jbi.cam.plugins.bpelse.datamodel.VariablePK id = new com.sun.jbi.cam.plugins.bpelse.datamodel.VariablePK();
        StringTokenizer idTokens = new StringTokenizer(string, ";");
        String params[] = new String[2];
        int i = 0;
        while(idTokens.hasMoreTokens()) {
            params[i++] = idTokens.nextToken();
        }
        if (i != 2) {
            throw new IllegalArgumentException("Expected format of parameter string is a set of 2 IDs delimited by ;");
        }
        id.setBpid(params[0]);
        id.setId(Long.parseLong(params[1]));
        //JSF 1.2
//        VariableController controller = (VariableController) facesContext.getApplication().getELResolver().getValue(
//            facesContext.getELContext(), null, "variable");

        // JSF 1.1
        VariableController controller = (VariableController) facesContext.getApplication().getVariableResolver().resolveVariable(
                facesContext, "variable");

        return controller.findVariable(id);
    }

    public String getAsString(FacesContext facesContext, UIComponent uIComponent, Object object) {
        if (object == null) {
            return null;
        }
        if(object instanceof Variable) {
            Variable o = (Variable) object;
            return o.getVariablePK().getBpid() + ";" + o.getVariablePK().getId();
        } else {
            throw new IllegalArgumentException("object:" + object + " of type:" + object.getClass().getName() + "; expected type: com.sun.jbi.cam.components.bpelse.db.Variable");
        }
    }
    
}
