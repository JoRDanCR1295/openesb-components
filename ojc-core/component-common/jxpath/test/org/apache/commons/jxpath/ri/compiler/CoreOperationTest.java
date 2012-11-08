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
 * @(#)CoreOperationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.compiler;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathTestCase;
import org.apache.commons.jxpath.Variables;

/**
 * Test basic functionality of JXPath - infoset types,
 * operations.
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class CoreOperationTest extends JXPathTestCase {
    private JXPathContext context;

    /**
     * Construct a new instance of this test case.
     *
     * @param name Name of the test case
     */
    public CoreOperationTest(String name) {
        super(name);
    }

    public void setUp() {
        if (context == null) {
            context = JXPathContext.newContext(null);
            Variables vars = context.getVariables();
            vars.declareVariable("integer", new Integer(1));
        }
    }

    public void testInfoSetTypes() {

        // Numbers
        assertXPathValue(context, "1", new Double(1.0));
        assertXPathPointer(context, "1", "1");
        assertXPathValueIterator(context, "1", list(new Double(1.0)));

        assertXPathPointerIterator(context, "1", list("1"));

        assertXPathValue(context, "-1", new Double(-1.0));
        assertXPathValue(context, "2 + 2", new Double(4.0));
        assertXPathValue(context, "3 - 2", new Double(1.0));
        assertXPathValue(context, "1 + 2 + 3 - 4 + 5", new Double(7.0));
        assertXPathValue(context, "3 * 2", new Double(3.0 * 2.0));
        assertXPathValue(context, "3 div 2", new Double(3.0 / 2.0));
        assertXPathValue(context, "5 mod 2", new Double(1.0));

        // This test produces a different result with Xalan?
        assertXPathValue(context, "5.9 mod 2.1", new Double(1.0));

        assertXPathValue(context, "5 mod -2", new Double(1.0));
        assertXPathValue(context, "-5 mod 2", new Double(-1.0));
        assertXPathValue(context, "-5 mod -2", new Double(-1.0));
        assertXPathValue(context, "1 < 2", Boolean.TRUE);
        assertXPathValue(context, "1 > 2", Boolean.FALSE);
        assertXPathValue(context, "1 <= 1", Boolean.TRUE);
        assertXPathValue(context, "1 >= 2", Boolean.FALSE);
        assertXPathValue(context, "3 > 2 > 1", Boolean.FALSE);
        assertXPathValue(context, "3 > 2 and 2 > 1", Boolean.TRUE);
        assertXPathValue(context, "3 > 2 and 2 < 1", Boolean.FALSE);
        assertXPathValue(context, "3 < 2 or 2 > 1", Boolean.TRUE);
        assertXPathValue(context, "3 < 2 or 2 < 1", Boolean.FALSE);
        assertXPathValue(context, "1 = 1", Boolean.TRUE);
        assertXPathValue(context, "1 = '1'", Boolean.TRUE);
        assertXPathValue(context, "1 > 2 = 2 > 3", Boolean.TRUE);
        assertXPathValue(context, "1 > 2 = 0", Boolean.TRUE);
        assertXPathValue(context, "1 = 2", Boolean.FALSE);

        assertXPathValue(context, "$integer", new Double(1), Double.class);

        assertXPathValue(context, "2 + 3", "5.0", String.class);

        assertXPathValue(context, "2 + 3", Boolean.TRUE, boolean.class);

        assertXPathValue(context, "'true'", Boolean.TRUE, Boolean.class);
    }
}
