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
 * @(#)Expr.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.model;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathConstants;

/**
 * 
 * @author Kevan Simpson
 */
public interface Expr {
    public String getXPath();
    public Regex getRegex();
    public QName getType();
    public String predicate(String... args);
    
    public enum JbiExpr implements Expr {
        type("jbi:component/@type", XPathConstants.STRING, Regex.attribute),
        name("jbi:component/jbi:identification/jbi:name", XPathConstants.STRING, Regex.element),
        description("jbi:component/jbi:identification/jbi:description", XPathConstants.STRING, Regex.element),
        manager_class("jbi:component/jbi:component-class-name", XPathConstants.STRING, Regex.element),
        manager_description("jbi:component/jbi:component-class-name/@description", XPathConstants.STRING, Regex.attribute),
        component_path_element("jbi:component/jbi:component-class-path/jbi:path-element", XPathConstants.NODESET, Regex.element),
        component_classpath("jbi:component/jbi:component-class-path", XPathConstants.NODE, Regex.read_only),
        bootstrap_class("jbi:component/jbi:bootstrap-class-name", XPathConstants.STRING, Regex.element),
        bootstrap_path_element("jbi:component/jbi:bootstrap-class-path/jbi:path-element", XPathConstants.NODESET, Regex.element),
        bootstrap_classpath("jbi:component/jbi:bootstrap-class-path", XPathConstants.NODE, Regex.read_only),
        shared_libraries("jbi:component/jbi:shared-library", XPathConstants.NODESET, Regex.read_only),
        config("jbi:component/cfg:Configuration", XPathConstants.NODE, Regex.read_only),
        logging("jbi:component/log:Logging", XPathConstants.NODE, Regex.read_only);
        // value setting expressions
//        add_component_path_element("jbi:component/jbi:component-class-path/jbi:path-element", XPathConstants.NODE, Regex.add_element),
//        remove_component_path_element("jbi:component/jbi:component-class-path/jbi:path-element", XPathConstants.NODE, Regex.remove_element),
//        add_bootstrap_path_element("jbi:component/jbi:bootstrap-class-path/jbi:path-element", XPathConstants.NODE, Regex.add_element),
//        remove_bootstrap_path_element("jbi:component/jbi:bootstrap-class-path/jbi:path-element", XPathConstants.NODE, Regex.remove_element),
//        add_shared_library("jbi:component/jbi:shared-library", XPathConstants.NODE, Regex.add_element),
//        remove_shared_library("jbi:component/jbi:shared-library", XPathConstants.NODE, Regex.remove_element),
//        // TODO Configuration/Property add/remove + Logging/Logger add/remove
//        remove_config_property("jbi:component/"+ JbiComponent.CFG +":Configuration/"+ JbiComponent.CFG +":Property",
//                               XPathConstants.NODE, Regex.remove_element),
        // source display expressions
//        src_id("jbi:component/jbi:identification", XPathConstants.NODE, Regex.element),

        private BasicExpr mExpr;

        private JbiExpr(String expr, QName type, Regex re) {
            mExpr = new BasicExpr(expr, type, re);
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
    }

    public enum PomExpr implements Expr {
        group_id("groupId", XPathConstants.STRING, Regex.element),
        artifact_id("artifactId", XPathConstants.STRING, Regex.element),
        name("name", XPathConstants.STRING, Regex.element),
        version("version", XPathConstants.STRING, Regex.element),
        description("description", XPathConstants.STRING, Regex.element),
        depend_id("//artifactId", XPathConstants.NODE, Regex.element),
        depend_version("//version", XPathConstants.NODE, Regex.element),
        parent_id("parent/artifactId", XPathConstants.STRING, Regex.element),
        parent_group_id("parent/groupId", XPathConstants.STRING, Regex.element),
        parent_version("parent/version", XPathConstants.STRING, Regex.element),
        parent_path("parent/relativePath", XPathConstants.STRING, Regex.element),
        properties("properties", XPathConstants.NODE, Regex.read_only),
        // top-level
        modules("modules/module", XPathConstants.NODESET, Regex.read_only),
        // jbiadapter
        i18n_pattern("//i18n/@pattern", XPathConstants.STRING, Regex.attribute),
        msgs_pkg("properties/msgs.package", XPathConstants.STRING, Regex.element),
        // global-common
        gc_properties("properties", XPathConstants.NODE, Regex.read_only),
        // .jbic
        jbic_name("@name", XPathConstants.STRING, Regex.attribute),
        jbic_file("@file", XPathConstants.STRING, Regex.attribute);
        // value setting expressions
//        add_module("modules/module", XPathConstants.NODESET, Regex.add_element),
//        remove_module("modules/module", XPathConstants.NODE, Regex.remove_element);

        private BasicExpr mExpr;

        private PomExpr(String expr, QName type, Regex re) {
            mExpr = new BasicExpr(expr, type, re);
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
    }

    public enum ProjectExpr implements Expr {
        component("cdk:component", XPathConstants.NODE, Regex.read_only),
        component_root("cdk:component/cdk:root", XPathConstants.STRING, Regex.element),
        component_descriptor("cdk:component/cdk:descriptor", XPathConstants.STRING, Regex.element),
        poms("cdk:maven/cdk:pom", XPathConstants.NODESET, Regex.read_only),
        services("cdk:services", XPathConstants.NODE, Regex.read_only),
        builders("cdk:builder", XPathConstants.NODESET, Regex.read_only),
        units("cdk:unit", XPathConstants.NODESET, Regex.read_only),
//        as_base("cdk:appserver/cdk:base", XPathConstants.STRING, Regex.element),
        as_script("cdk:appserver/cdk:script", XPathConstants.STRING, Regex.element),
        maven("cdk:maven", XPathConstants.NODE, Regex.read_only);
        // value setting expressions
//        add_pom("cdk:maven/cdk:pom", XPathConstants.NODESET, Regex.add_element),
//        remove_pom("cdk:maven/cdk:pom", XPathConstants.NODESET, Regex.remove_element);

        private BasicExpr mExpr;

        private ProjectExpr(String expr, QName type, Regex re) {
            mExpr = new BasicExpr(expr, type, re);
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
    }
    
    public enum AxisExpr implements Expr {
        self(".", XPathConstants.NODE, Regex.read_only),
        children("*", XPathConstants.NODESET, Regex.read_only),
        parent("..", XPathConstants.NODE, Regex.read_only);

        private BasicExpr mExpr;

        private AxisExpr(String expr, QName type, Regex re) {
            mExpr = new BasicExpr(expr, type, re);
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
    }
    
    public static class BasicExpr implements Expr {
        private String mExpr;
        private QName mType;
        private Regex mRegex;
        
        public BasicExpr(String expr, QName type, Regex re) {
            mExpr = expr;
            mType = type;
            mRegex = re;
        }
        /** @see com.sun.jbi.component.toolkit.project.model.Expr#getXPath() */
        public String getXPath() {
            return mExpr;
        } 
        /** @see com.sun.jbi.component.toolkit.project.model.Expr#getRegex() */
        public Regex getRegex() {
            return mRegex;
        }
        /** @see com.sun.jbi.component.toolkit.project.model.Expr#getType() */
        public QName getType() {
            return mType;
        }
        /** @see com.sun.jbi.component.toolkit.project.model.Expr#predicate(java.lang.String[]) */
        public String predicate(String... args) {
            StringBuffer buff = new StringBuffer();
            buff.append(getXPath());
            if (args != null) {
                buff.append("[");
                for (String str : args) {
                    buff.append(str);
                }
                buff.append("]");
            }
            return buff.toString();
        }
    }
}
