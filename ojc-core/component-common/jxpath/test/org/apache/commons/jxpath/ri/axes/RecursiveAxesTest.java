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
 * @(#)RecursiveAxesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.axes;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathTestCase;

/**
 * Test for the protection mechanism that stops infinite recursion
 * in descent down a recursive graph. 
 */
public class RecursiveAxesTest extends JXPathTestCase {

    private RecursiveBean bean;
    private JXPathContext context;

    public RecursiveAxesTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        junit.textui.TestRunner.run(RecursiveAxesTest.class);
    }

    /**
     * @see TestCase#setUp()
     */
    protected void setUp() throws Exception {
        bean = new RecursiveBean("zero");
        RecursiveBean bean1 = new RecursiveBean("one");
        RecursiveBean bean2 = new RecursiveBean("two");
        RecursiveBean bean3 = new RecursiveBean("three");
        bean.setFirst(bean1);
        bean1.setFirst(bean2);
        bean2.setFirst(bean1);
        bean2.setSecond(bean3);

        context = JXPathContext.newContext(null, bean);
    }

    public void testInfiniteDescent() {
        // Existing scalar property
        assertXPathPointer(
            context,
            "//.[name = 'three']",
            "/first/first/second");
    }
}
