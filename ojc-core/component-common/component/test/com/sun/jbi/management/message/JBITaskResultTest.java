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
 * @(#)JBITaskResultTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.management.message.ComponentTaskResult;
import com.sun.jbi.management.message.JBIMessageException;
import com.sun.jbi.management.message.JBITaskResult;

/**
 *
 * @author Sun Microsystems
 *
 */
public class JBITaskResultTest extends JBIMessageTestCase {

    private JBITaskResult jbiTaskResult = null;

    public void setUp() {
        jbiTaskResult = new JBITaskResult();
    }

    public void tearDown() {
        jbiTaskResult = null;
    }

    public void testFrameworkTaskResult() {
        try {
            jbiTaskResult.setFrameworkTaskResult(null);
            fail("Framework task result cannot be null");
        } catch (JBIMessageException jme) {}

        if (jbiTaskResult.getFrameworkTaskResult() == null) {
            fail("Default framework task result is null");
        }
    }

    public void testComponentTaskResults() {
        try {
            jbiTaskResult.setComponentTaskResults(null);
            fail("Component task results cannot be set null");
        } catch (JBIMessageException jme) {}

        ComponentTaskResult ctr_1 = ComponentTaskResultTest.newInstance();
        try {
            jbiTaskResult.removeComponentTaskResult(ctr_1);
            fail("Cannot remove component from empty list");
        } catch (JBIMessageException jme) {}

        List list = jbiTaskResult.getComponentTaskResults();
        if (list == null) {
            fail("Default component task result list is null");
            list = new ArrayList();
        } else  if (list.size() != 0) {
            fail("Default component task result list is not empty");
        }

        try {
            list.add(null);
            fail("Cannot add null to list");
        } catch (IllegalArgumentException iae) {}
        try {
            list.add("Hey man!");
            fail("Cannot add non-ComponentTaskResult element to list");
        } catch (ClassCastException iae) {}
        try {
            jbiTaskResult.addComponentTaskResult(null);
            fail("Cannot add null component task result");
        } catch (JBIMessageException jme) {}

        jbiTaskResult.addComponentTaskResult(ctr_1);
        assertEquals(jbiTaskResult.getComponentTaskResults().size(), 1);
        try {
            list.add(ctr_1);
            fail("Cannot add duplicate component task result element to list");
        } catch (IllegalArgumentException iae) {}

        ComponentTaskResult ctr_2 = jbiTaskResult.newComponentTaskResult();
        assertEquals(jbiTaskResult.getComponentTaskResults().size(), 2);

        try {
            jbiTaskResult.removeComponentTaskResult(null);
            fail("Cannot remove null component task result");
        } catch (JBIMessageException jme) {}

        try {
            ComponentTaskResult ctr_3 = ComponentTaskResultTest.newInstance();
            jbiTaskResult.removeComponentTaskResult(ctr_3);
            fail("Cannot remove not previoulsy added component task result");
        } catch (JBIMessageException jme) {}

        jbiTaskResult.removeComponentTaskResult(ctr_2);
        assertEquals(jbiTaskResult.getComponentTaskResults().size(), 1);

        jbiTaskResult.removeComponentTaskResult(ctr_1);
        assertEquals(jbiTaskResult.getComponentTaskResults().size(), 0);
    }

    public void testComponentTaskResult() {
        try {
            jbiTaskResult.removeComponentTaskResult(null);
            fail("Component task result to be removed cannot be null");
        } catch (JBIMessageException jme) {}

        List list = getComponentTaskResults();
        int count = list.size();
        ComponentTaskResult componentTaskResult = jbiTaskResult.newComponentTaskResult();
        if (componentTaskResult == null) {
            fail("New component task result is null");
        }
        if (getComponentTaskResults().size() != count + 1) {
            fail("newComponentTaskResult did not add element to list");
        }
        try {
            jbiTaskResult.removeComponentTaskResult(componentTaskResult);
            if (getComponentTaskResults().size() != count) {
                fail("removeComponentTaskResult did not remove element from list");
            }
        } catch (JBIMessageException jme) {
            fail("Added element was not properly removed");
        }
    }

    public void testGetString() {
        try {
            jbiTaskResult.getString();
            // fail("No proper validation on empty instance");
        } catch (JBIMessageException jme) {}

        validateXMLString(newInstance().getString(), "JBITaskResult");
    }

    private List getComponentTaskResults() {
        List list = jbiTaskResult.getComponentTaskResults();
        if (list == null) {
            fail("Null list of component task result was returned");
        }
        return list;
    }

    static JBITaskResult newInstance() {
        JBITaskResult jbiTaskResult = new JBITaskResult();
        jbiTaskResult.setFrameworkTaskResult(FrameworkTaskResultTest.newInstance());
        jbiTaskResult.addComponentTaskResult(ComponentTaskResultTest.newInstance());
        return jbiTaskResult;
    }
}
