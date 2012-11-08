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
 * @(#)NodeTypeTest.java 
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
public class NodeTypeTest extends NodeTest {
    private int nodeType;

    public NodeTypeTest(int nodeType) {
        this.nodeType = nodeType;
    }

    public int getNodeType() {
        return nodeType;
    }

    public String toString() {
        return nodeTypeToString(nodeType) + "()";
    }

    public static String nodeTypeToString(int code) {
        switch (code) {
            case Compiler.NODE_TYPE_NODE :
                return "node";
            case Compiler.NODE_TYPE_TEXT :
                return "text";
            case Compiler.NODE_TYPE_COMMENT :
                return "comment";
            case Compiler.NODE_TYPE_PI :
                return "processing-instruction";
        }
        return "UNKNOWN";
    }
}
