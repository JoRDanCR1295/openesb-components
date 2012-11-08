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
 * @(#)ScrubExpr.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.scrub;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;
import com.sun.jbi.component.toolkit.project.model.Expr;
import com.sun.jbi.component.toolkit.project.model.Regex;

/**
 * 
 * @author Kevan Simpson
 */
public enum ScrubExpr implements Expr {
    depend_id(PomExpr.depend_id),
    depend_version(PomExpr.depend_version),
    // scrub config
    profiles("profile", XPathConstants.NODESET, Regex.read_only),           // init
    unit_defs("unit-defs/unit", XPathConstants.NODESET, Regex.read_only),   // init
    files("file", XPathConstants.NODESET, Regex.read_only),                 // profile mode
    tokens("token", XPathConstants.NODESET, Regex.read_only),               // unit mode
    units("unit", XPathConstants.NODESET, Regex.read_only),                 // file mode
    // ojc-core pom
    ojc_core_module("profiles/profile[id/text() = 'default-profile']/modules", 
                    XPathConstants.NODE, Regex.read_only),
    // global versions pom
    global_version_properties(PomExpr.properties),
    global_version_dependencies("dependencyManagement/dependencies",
                                XPathConstants.NODE, Regex.read_only),
    global_version_dependency("dependencyManagement/dependencies/dependency",
                              XPathConstants.NODE, Regex.read_only)
    ;

    private Expr mExpr;
    
    private ScrubExpr(Expr expr) {
        mExpr = expr;
    }
    
    private ScrubExpr(String expr, QName type, Regex re) {
        this(new BasicExpr(expr, type, re));
    }
    
    /** @see com.sun.jbi.component.toolkit.project.model.Expr#getXPath() */
    public String getXPath() {
        return mExpr.getXPath();
    } 
    /** @see com.sun.jbi.component.toolkit.project.model.Expr#getRegex() */
    public Regex getRegex() {
        return mExpr.getRegex();
    }
    /** @see com.sun.jbi.component.toolkit.project.model.Expr#getType() */
    public QName getType() {
        return mExpr.getType();
    } 
    /** @see com.sun.jbi.component.toolkit.project.model.Expr#predicate(java.lang.String[]) */
    public String predicate(String... args) {
        return mExpr.predicate(args);
    }

    public String query(String... steps) {
        if (steps != null) {
            StringBuffer buff = new StringBuffer();
            buff.append(getXPath());
            for (String s : steps) {
                buff.append(s);
            }
            return buff.toString();
        }
        
        return "";
    }
}
