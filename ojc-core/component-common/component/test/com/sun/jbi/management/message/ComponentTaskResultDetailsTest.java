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
 * @(#)ComponentTaskResultDetailsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import com.sun.jbi.management.message.ComponentTaskResultDetails;
import com.sun.jbi.management.message.JBIMessageException;

/**
 *
 * @author Sun Microsystems
 *
 */
public class ComponentTaskResultDetailsTest extends JBIMessageTestCase {
    private ComponentTaskResultDetails componentTaskResultDetails = null;

    public void setUp() {
        componentTaskResultDetails = new ComponentTaskResultDetails();
    }

    public void tearDown() {
        componentTaskResultDetails = null;
    }

    public void testDetails() {
        try {
            componentTaskResultDetails.setDetails(null);
            fail("Details cannot be null");
        } catch (JBIMessageException jme) {}

        if (componentTaskResultDetails.getDetails() == null) {
            fail("Default details are null");
        }
    }

    public void testValidate() {
        try {
            componentTaskResultDetails.setDetails(null);
            fail("Cannot set details to null");
        } catch (JBIMessageException jme) {}
    }

    public void testGetString() {
        validateXMLString(newInstance().getString(), "ComponentTaskResultDetails");
    }

    static ComponentTaskResultDetails newInstance() {
        ComponentTaskResultDetails componentTaskResultDetails = new ComponentTaskResultDetails();
        componentTaskResultDetails.setDetails(TaskResultDetailsTest.newInstance());
        return componentTaskResultDetails;
    }
}
