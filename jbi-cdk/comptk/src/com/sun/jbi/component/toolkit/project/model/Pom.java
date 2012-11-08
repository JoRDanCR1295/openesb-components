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
 * @(#)JbiComponent.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.model;

import java.io.File;
import org.w3c.dom.Element;
import com.sun.jbi.component.toolkit.project.model.Expr.PomExpr;
import com.sun.jbi.component.toolkit.project.util.XPathElement;


/**
 * 
 * @author Kevan Simpson
 */
public class Pom extends XPathElement {//XmlObject<PomExpr> {
    public enum Module {
        top_level, packaging, jbiadapter
    }
    
    public Pom(Element elem, File file) {
        super(file, elem);
//        super(elem, file, PomExpr.values());
    }
    
    public Pom(XPathElement elem) {
        super(elem);
    }
    
//    public Pom(File file) {
//        super(file, PomExpr.values());
//    }
    
    public String getName() {
        return getString(PomExpr.jbic_name.getXPath());
//        return (String) getValue(PomExpr.jbic_name);
    }
    
//    public static Pom loadGlobalPom(File ojcCoreDir) {
//        try {
//            File gc = new File(ojcCoreDir.getParentFile(), "global-common");
//            if (gc.exists()) {
//                File pomXml = new File(new File(gc, "ojc-versions"), "pom.xml");
//                if (pomXml.exists()) {
//                    return new Pom(pomXml);
//                }
//            }
//        }
//        catch (Exception e) {
//            // TODO log warning and continue
//            e.printStackTrace();
//        }
//        
//        return null;
//    }
}
