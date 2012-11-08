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
 * @(#)Step.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.ri.Compiler;

/**
 * @author Dmitri Plotnikov
 * @version  
 */
public class Step {
    private int axis;
    private NodeTest nodeTest;
    private Expression[] predicates;

    protected Step(int axis, NodeTest nodeTest, Expression[] predicates) {
        this.axis = axis;
        this.nodeTest = nodeTest;
        this.predicates = predicates;
    }

    public int getAxis() {
        return axis;
    }

    public NodeTest getNodeTest() {
        return nodeTest;
    }

    public Expression[] getPredicates() {
        return predicates;
    }

    public boolean isContextDependent() {
        if (predicates != null) {
            for (int i = 0; i < predicates.length; i++) {
                if (predicates[i].isContextDependent()) {
                    return true;
                }
            }
        }
        return false;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        int axis = getAxis();
        if (axis == Compiler.AXIS_CHILD) {
            buffer.append(nodeTest);
        }
        else if (axis == Compiler.AXIS_ATTRIBUTE) {
            buffer.append('@');
            buffer.append(nodeTest);
        }
        else if (axis == Compiler.AXIS_SELF
                && nodeTest instanceof NodeTypeTest
                && ((NodeTypeTest) nodeTest).getNodeType()
                    == Compiler.NODE_TYPE_NODE) {
            buffer.append(".");
        }
        else if (axis == Compiler.AXIS_PARENT
                && nodeTest instanceof NodeTypeTest
                && ((NodeTypeTest) nodeTest).getNodeType()
                    == Compiler.NODE_TYPE_NODE) {
            buffer.append("..");
        }
        else if (axis == Compiler.AXIS_DESCENDANT_OR_SELF
                && nodeTest instanceof NodeTypeTest
                && ((NodeTypeTest) nodeTest).getNodeType()
                    == Compiler.NODE_TYPE_NODE 
                && (predicates == null || predicates.length == 0)) {
            buffer.append("");
        }
        else {
            buffer.append(axisToString(axis));
            buffer.append("::");
            buffer.append(nodeTest);
        }
        Expression[] predicates = getPredicates();
        if (predicates != null) {
            for (int i = 0; i < predicates.length; i++) {
                buffer.append('[');
                buffer.append(predicates[i]);
                buffer.append(']');
            }
        }
        return buffer.toString();
    }

    public static String axisToString(int axis) {
        switch (axis) {
            case Compiler.AXIS_SELF :
                return "self";
            case Compiler.AXIS_CHILD :
                return "child";
            case Compiler.AXIS_PARENT :
                return "parent";
            case Compiler.AXIS_ANCESTOR :
                return "ancestor";
            case Compiler.AXIS_ATTRIBUTE :
                return "attribute";
            case Compiler.AXIS_NAMESPACE :
                return "namespace";
            case Compiler.AXIS_PRECEDING :
                return "preceding";
            case Compiler.AXIS_FOLLOWING :
                return "following";
            case Compiler.AXIS_DESCENDANT :
                return "descendant";
            case Compiler.AXIS_ANCESTOR_OR_SELF :
                return "ancestor-or-self";
            case Compiler.AXIS_FOLLOWING_SIBLING :
                return "following-sibling";
            case Compiler.AXIS_PRECEDING_SIBLING :
                return "preceding-sibling";
            case Compiler.AXIS_DESCENDANT_OR_SELF :
                return "descendant-or-self";
        }
        return "UNKNOWN";
    }
}
