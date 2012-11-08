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
 * @(#)ScopesnapshotPKConverter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.plugins.bpelse.datamodel.ScopesnapshotPK;
import java.util.StringTokenizer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

/**
 *
 * @author nnahata
 */
public class ScopesnapshotPKConverter implements Converter {
    
    /** Creates a new instance of ScopesnapshotPKConverter */
    public ScopesnapshotPKConverter() {
    }

    public Object getAsObject(FacesContext facesContext, UIComponent uIComponent, String string) {
        if (string == null) {
            return null;
        }
        com.sun.jbi.cam.plugins.bpelse.datamodel.ScopesnapshotPK id = new com.sun.jbi.cam.plugins.bpelse.datamodel.ScopesnapshotPK();
        StringTokenizer idTokens = new StringTokenizer(string, ";");
        String params[] = new String[4];
        int i = 0;
        while(idTokens.hasMoreTokens()) {
            params[i++] = idTokens.nextToken();
        }
        if (i != 4) {
            throw new IllegalArgumentException("Expected format of parameter string is a set of 4 IDs delimited by ;");
        }
        id.setBpid(params[0]);
        id.setId(Long.parseLong(params[1]));
        id.setVarid(Long.parseLong(params[2]));
        id.setIteration(Long.parseLong(params[3]));
        return id;
    }

    public String getAsString(FacesContext facesContext, UIComponent uIComponent, Object object) {
        if (object == null) {
            return null;
        }
        if(object instanceof ScopesnapshotPK) {
            ScopesnapshotPK o = (ScopesnapshotPK) object;
            return o.getBpid() + ";" + o.getId() + ";" + o.getVarid() + ";" + o.getIteration();
        } else {
            throw new IllegalArgumentException("object:" + object + " of type:" + object.getClass().getName() + "; expected type: ScopesnapshotPK");
        }
    }
    
}
