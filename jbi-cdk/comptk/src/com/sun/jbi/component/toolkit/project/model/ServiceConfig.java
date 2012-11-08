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
 * @(#)ServiceConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.model;

import java.io.File;
import org.w3c.dom.Element;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.model.Expr.ProjectExpr;
import com.sun.jbi.component.toolkit.project.services.ServicesBuilder;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public class ServiceConfig extends XPathElement {
    /**
     * @param elem
     * @param exprs
     */
    public ServiceConfig(Element elem, File file) {
        super(file, elem, ProjectExpr.builders.getXPath(), 
                          ProjectExpr.services.getXPath(), 
                          ProjectExpr.units.getXPath());
    }

    public String getBuilderType(String name) {
        return lookup(ProjectExpr.builders, name, "type");
    }
    
    public String getUnitLocation(String name) {
        return lookup(ProjectExpr.units, name, "loc");
    }

    private String lookup(Expr expr, String text, String attr) {
        Element elem = (Element) getNode(expr.predicate("text() = '", text, "'"));
        return (elem != null) ? elem.getAttribute(attr) : null;
    }
    
    public ServicesBuilder loadBuilder(String name) {
        if (!Util.isEmpty(name)) {
            String type = getBuilderType(name);
            if (Util.isEmpty(type)) {
                throw new ProjectException("Unknown ServicesBuilder Type: "+ name);
            }
            
            try {
                Class<ServicesBuilder> clz = (Class<ServicesBuilder>) Class.forName(type);
                return clz.newInstance();
            }
            catch (Exception e) {
                throw new ProjectException("Failed to load ServicesBuilder "+ type +": "+ e.getMessage(), e);
            }
        }
        
        return null;
    }
}
