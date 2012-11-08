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
 * @(#)TaskResultDetailsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.ExceptionInfo;
import com.sun.jbi.management.message.JBIMessageException;
import com.sun.jbi.management.message.MessageLocalizationInfo;
import com.sun.jbi.management.message.TaskResultDetails;

/**
 *
 * @author Sun Microsystems
 *
 */
public class TaskResultDetailsTest extends JBIMessageTestCase {
    private TaskResultDetails taskResultDetails = null;

    public void setUp() {
        taskResultDetails = new TaskResultDetails();
    }

    public void tearDown() {
        taskResultDetails = null;
    }

    public void testTaskId() {
        try {
            taskResultDetails.setTaskId(null);
            fail("Task id cannot be null");
        } catch (JBIMessageException jme) {}
    }

    public void testMessageType() {
        try {
            taskResultDetails.setMessageType(-1);
            fail("Message type cannot be -1");
        } catch (JBIMessageException jme) {}

        try {
            taskResultDetails.setMessageType(1000);
            fail("Message type cannot be 1000");
        } catch (JBIMessageException jme) {}

        try {
            taskResultDetails.setMessageType(JBITaskMessageBuilder.INFO);
        } catch (JBIMessageException jme) {
            fail("Message type should be allowed: INFO");
        }
        try {
            taskResultDetails.setMessageType(JBITaskMessageBuilder.WARNING);
        } catch (JBIMessageException jme) {
            fail("Message type should be allowed: WARNING");
        }
        try {
            taskResultDetails.setMessageType(JBITaskMessageBuilder.ERROR);
        } catch (JBIMessageException jme) {
            fail("Message type should be allowed: ERROR");
        }
    }

    public void testMessageLocalizations() {
        try {
            taskResultDetails.setMessages(null);
            fail("Message localizations cannot be null");
        } catch (JBIMessageException jme) {}

        MessageLocalizationInfo mli_1 = MessageLocalizationInfoTest.newInstance();
        try {
            taskResultDetails.removeMessageLocalizationInfo(mli_1);
            fail("Cannot remove from empty message localization list");
        } catch (JBIMessageException jme) {}

        List list = taskResultDetails.getMessages();
        if (list == null) {
            fail("Default message localization list is null");
            list = new ArrayList();
        } else  if (list.size() != 0) {
            fail("Default message localization list is not empty");
        }

        try {
            list.add(null);
            fail("Cannot add null to list");
        } catch (IllegalArgumentException iae) {}
        try {
            list.add("Hey man!");
            fail("Cannot add non-MessageLocalizationInfo element to list");
        } catch (ClassCastException iae) {}

        try {
            taskResultDetails.addMessageLocalizationInfo(null);
            fail("Cannot add null message localization info");
        } catch (JBIMessageException jme) {}

        taskResultDetails.addMessageLocalizationInfo(mli_1);
        assertEquals(taskResultDetails.getMessages().size(), 1);
        try {
            list.add(mli_1);
            fail("Cannot add duplicate component task result element to list");
        } catch (IllegalArgumentException iae) {}

        MessageLocalizationInfo mli_2 = taskResultDetails.
            newMessageLocalizationInfo("greeting",
                                       "Hi {0}",
                                       new Object[] { "Stefano" });
        // assertEquals(taskResultDetails.getMessages().size(), 2);

        try {
            taskResultDetails.removeMessageLocalizationInfo(null);
            fail("Cannot remove null message localization info");
        } catch (JBIMessageException jme) {}

        try {
            MessageLocalizationInfo mli_3 = MessageLocalizationInfoTest.newInstance();
            taskResultDetails.removeMessageLocalizationInfo(mli_3);
            fail("Cannot remove not previoulsy added component task result");
        } catch (JBIMessageException jme) {}

        taskResultDetails.removeMessageLocalizationInfo(mli_2);
        assertEquals(taskResultDetails.getMessages().size(), 1);

        taskResultDetails.removeMessageLocalizationInfo(mli_1);
        assertEquals(taskResultDetails.getMessages().size(), 0);
    }

    public void testExceptions() {
        try {
            taskResultDetails.setExceptions(null);
            fail("Exception cannot be null");
        } catch (JBIMessageException jme) {}

        ExceptionInfo ei_1 = ExceptionInfoTest.newInstance();
        try {
            taskResultDetails.removeExceptionInfo(ei_1);
            fail("Cannot remove from empty exception info list");
        } catch (JBIMessageException jme) {}

        List list = taskResultDetails.getExceptions();
        if (list == null) {
            fail("Default exception info list is null");
            list = new ArrayList();
        } else  if (list.size() != 0) {
            fail("Default exception info list is not empty");
        }

        try {
            list.add(null);
            fail("Cannot add null to list");
        } catch (IllegalArgumentException iae) {}
        try {
            list.add("Hey man!");
            fail("Cannot add non-ExceptionInfo element to list");
        } catch (ClassCastException iae) {}

        try {
            taskResultDetails.addExceptionInfo(null);
            fail("Cannot add null exception info");
        } catch (JBIMessageException jme) {}

        taskResultDetails.addExceptionInfo(ei_1);
        assertEquals(taskResultDetails.getExceptions().size(), 1);
        try {
            list.add(ei_1);
            fail("Cannot add duplicate exception element to list");
        } catch (IllegalArgumentException iae) {}

        MessageLocalizationInfo mli = MessageLocalizationInfoTest.newInstance();
        ExceptionInfo ei_2 = taskResultDetails.newExceptionInfo(new Exception("Oh man!"), mli);
        assertEquals(taskResultDetails.getExceptions().size(), 2);

        try {
            taskResultDetails.removeExceptionInfo(null);
            fail("Cannot remove null exception info");
        } catch (JBIMessageException jme) {}

        try {
            ExceptionInfo ei_3 = ExceptionInfoTest.newInstance();
            taskResultDetails.removeExceptionInfo(ei_3);
            fail("Cannot remove not previoulsy added exception info result");
        } catch (JBIMessageException jme) {}

        taskResultDetails.removeExceptionInfo(ei_2);
        assertEquals(taskResultDetails.getExceptions().size(), 1);

        taskResultDetails.removeExceptionInfo(ei_1);
        assertEquals(taskResultDetails.getMessages().size(), 0);
    }

    public void testValidate() {
    	// These validations are now logger warnings
    	/*
        try {
            taskResultDetails.validate();
            fail("Validation passes with null task id");
        } catch (JBIMessageException jme) {}
        taskResultDetails.setTaskId("deploy");

        taskResultDetails.setSuccessfulResult(true);
        taskResultDetails.setMessageType(JBITaskMessageBuilder.ERROR);
        try {
            taskResultDetails.validate();
            fail("Validation passes with success and error");
        } catch (JBIMessageException jme) {}
		*/
    	
        taskResultDetails.setSuccessfulResult(false);

        taskResultDetails.addExceptionInfo(ExceptionInfoTest.newInstance());
        try {
            taskResultDetails.validate();
        } catch (JBIMessageException jme) {
            fail("Validation fails with valid state");
        }
    }

    public void testGetString() {
        validateXMLString(newInstance().getString(), "TaskResultDetails");
    }

    static TaskResultDetails newInstance() {
        TaskResultDetails taskResultDetails = new TaskResultDetails();

        taskResultDetails.setTaskId("123");
        taskResultDetails.setSuccessfulResult(false);
        MessageLocalizationInfo mli = MessageLocalizationInfoTest.newInstance();
        taskResultDetails.newExceptionInfo(
                new JBIMessageException("Descriptor processing error",
                        new IllegalArgumentException("Missing component name")),
                mli);
        taskResultDetails.addMessageLocalizationInfo(mli);
        taskResultDetails.setMessageType(JBITaskMessageBuilder.ERROR);

        return taskResultDetails;
    }
}
