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
 * @(#)ComponentTaskResultTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import com.sun.jbi.management.message.ComponentTaskResult;
import com.sun.jbi.management.message.JBIMessageException;


/**
 *
 * @author Sun Microsystems
 *
 */
public class ComponentTaskResultTest extends JBIMessageTestCase {
    private ComponentTaskResult componentTaskResult = null;

    public void setUp() {
        componentTaskResult = new ComponentTaskResult();
    }

    public void tearDown() {
        componentTaskResult = null;
    }

    public void testComponentName() {
        try {
            componentTaskResult.setComponentName(null);
            fail("Component name cannot be null");
        } catch (JBIMessageException jme) {}
    }

    public void testDetails() {
        try {
            componentTaskResult.setDetails(null);
            fail("Component task result details cannot be null");
        } catch (JBIMessageException jme) {}

        if (componentTaskResult.getDetails() == null) {
            fail("Default component task result is null");
        }
    }

    public void testValidate() {
    	// These validations are now logger warnings
    	/*
        try {
            componentTaskResult.validate();
            fail("Validation passes with null component name");
        } catch (JBIMessageException jme) {}
        componentTaskResult.setComponentName("JFSE");
    	
        try {
            componentTaskResult.validate();
            fail("Validation passes with null details");
        } catch (JBIMessageException jme) {}
        componentTaskResult.setDetails(ComponentTaskResultDetailsTest.newInstance());

        try {
            componentTaskResult.validate();
        } catch (JBIMessageException jme) {
            fail("Validation fails with consistent state");
        }
		*/
    }

    public void testGetString() {
        validateXMLString(newInstance().getString(), "ComponentTaskResult");
    }

    static ComponentTaskResult newInstance() {
        ComponentTaskResult componentTaskResult = new ComponentTaskResult();
        componentTaskResult.setComponentName("myComponent");
        componentTaskResult.setDetails(ComponentTaskResultDetailsTest.newInstance());
        return componentTaskResult;
    }
}
