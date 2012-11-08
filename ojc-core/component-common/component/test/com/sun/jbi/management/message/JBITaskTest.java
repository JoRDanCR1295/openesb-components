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
 * @(#)JBITaskTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import com.sun.jbi.management.message.JBIMessageException;
import com.sun.jbi.management.message.JBITask;


/**
 *
 * @author Sun Microsystems
 *
 */
public class JBITaskTest extends JBIMessageTestCase {
    private JBITask jbiTask = null;

    public static final String VALID_VERSION = "1.0";
    public static final String INVALID_DECIMAL_VERSION = "1.0.1";
    public static final String ZERO_VERSION = "0";
    public static final String NEGATIVE_VERSION = "-1";
    public static final String REDUNDANT_VERSION = "01.00";

    public void setUp() {
        jbiTask = new JBITask();
    }

    public void tearDown() {
        jbiTask = null;
    }

    public void testVersion() {
        try {
            jbiTask.setVersion(null);
            fail("Version must not be null");
        } catch (JBIMessageException jme) {}

        try {
            jbiTask.setVersion(INVALID_DECIMAL_VERSION);
            fail("Invalid decimal version: " + INVALID_DECIMAL_VERSION);
        } catch (JBIMessageException jme) {}

        try {
            jbiTask.setVersion(ZERO_VERSION);
            fail("Version cannot be zero");
        } catch (JBIMessageException jme) {}

        try {
            jbiTask.setVersion(NEGATIVE_VERSION);
            fail("Version cannot be negative");
        } catch (JBIMessageException jme) {}

        try {
            jbiTask.setVersion(REDUNDANT_VERSION);
            fail("Version expression cannot be redundant");
        } catch (JBIMessageException jme) {}

        try {
            jbiTask.setVersion(VALID_VERSION);
        } catch (JBIMessageException jme) {
            fail("Valid version not accepted: " + VALID_VERSION);
        }
    }

    public void testTaskResult() {
        try {
            jbiTask.setTaskResult(null);
            fail("Task result must not be null");
        } catch (JBIMessageException jme) {}

        if (jbiTask.getTaskResult() == null) {
            fail("Default task result is null");
        }
    }

    public void testValidate() {
    	// This validations is now a logger warning
    	/*
        try {
            jbiTask.validate();
            fail("Validation passes with null task result");
        } catch (JBIMessageException jme) {}
		*/
    	
        jbiTask.setTaskResult(JBITaskResultTest.newInstance());
        try {
            jbiTask.validate();
        } catch (JBIMessageException jme) {
            fail("Validation fails with valid task result");
        }
    }

    public void testGetString() {
        jbiTask.setVersion("1.0");
        jbiTask.setTaskResult(JBITaskResultTest.newInstance());
        String jbiTaskString = jbiTask.getString();
        // System.err.println(jbiTaskString);
        validateXMLString(jbiTaskString, "JBITask");
    }
}
