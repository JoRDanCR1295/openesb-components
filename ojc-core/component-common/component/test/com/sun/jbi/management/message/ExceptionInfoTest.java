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
 * @(#)ExceptionInfoTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import com.sun.jbi.management.message.ExceptionInfo;
import com.sun.jbi.management.message.JBIMessageException;
import com.sun.jbi.management.message.MessageLocalizationInfo;

/**
 *
 * @author Sun Microsystems
 *
 */
public class ExceptionInfoTest extends JBIMessageTestCase {
    private ExceptionInfo exceptionInfo;

    public void setUp() {
        exceptionInfo = new ExceptionInfo();
    }

    public void tearDown() {
        exceptionInfo = null;
    }

    public void testException() {
        try {
            exceptionInfo.setException(null);
            fail("Exception cannot be null");
        } catch (JBIMessageException jme) {}
        assertEquals(exceptionInfo.getNestingLevel(), -1);
        Exception e1 = new IllegalArgumentException("Name cannot be null");

        exceptionInfo.setException(e1);
        assertEquals(exceptionInfo.getNestingLevel(), 0);

        Exception e2 = new JBIMessageException("Error populating properties", e1);
        exceptionInfo.setException(e2);
        assertEquals(exceptionInfo.getNestingLevel(), 1);
    }

    public void testMessageLocalization() {
        try {
            exceptionInfo.setMessageLocalization(null);
            fail("Message localization cannot be null");
        } catch (JBIMessageException jme) {}

        if (exceptionInfo.getMessageLocalization() == null) {
            fail("Default message localization info is null");
        }
    }

    public void testValidate() {
    	// These validations are now logger warnings
    	/*
        try {
            exceptionInfo.validate();
            fail("Exception is not set");
        } catch (JBIMessageException jme) {}
        exceptionInfo.setException(new Exception("Oh man!"));

        try {
            exceptionInfo.validate();
            fail("Message localization is not set");
        } catch (JBIMessageException jme) {}
        exceptionInfo.setMessageLocalization(new MessageLocalizationInfo());
        try {
            exceptionInfo.validate();
        } catch (Exception e) {
            fail("Should pass validation");
        }
		*/
    }

    public void testGetString() {
        String exceptionInfoString = newInstance().getString();
        validateXMLString(exceptionInfoString, "ExceptionInfo");
    }

    static ExceptionInfo newInstance() {
        ExceptionInfo exceptionInfo = new ExceptionInfo();
        exceptionInfo.setException(
                new JBIMessageException("Descriptor processing error",
                        new IllegalArgumentException("Null name!!!")));
        exceptionInfo.setMessageLocalization(MessageLocalizationInfoTest.newInstance());
        return exceptionInfo;
    }
}
