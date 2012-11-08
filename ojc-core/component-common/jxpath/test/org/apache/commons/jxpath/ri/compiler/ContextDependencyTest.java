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
 * @(#)ContextDependencyTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.JXPathTestCase;
import org.apache.commons.jxpath.ri.Parser;

/**
 * Tests the determination of whether an expression is context dependent.
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class ContextDependencyTest extends JXPathTestCase {
    public ContextDependencyTest(String name) {
        super(name);
    }

    public void testContextDependency() {
        testContextDependency("1", false);
        testContextDependency("$x", false);
        testContextDependency("/foo", false);
        testContextDependency("foo", true);
        testContextDependency("/foo[3]", false);
        testContextDependency("/foo[$x]", false);
        testContextDependency("/foo[bar]", true);
        testContextDependency("3 + 5", false);
        testContextDependency("test:func(3, 5)", true);
        testContextDependency("test:func(3, foo)", true);
    }

    public void testContextDependency(String xpath, boolean expected) {
        Expression expr =
            (Expression) Parser.parseExpression(xpath, new TreeCompiler());

        assertEquals(
            "Context dependency <" + xpath + ">",
            expected,
            expr.isContextDependent());
    }
}
