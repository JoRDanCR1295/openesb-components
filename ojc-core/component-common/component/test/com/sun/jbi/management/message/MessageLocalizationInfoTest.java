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
 * @(#)MessageLocalizationInfoTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.Date;

import com.sun.jbi.management.message.JBIMessageException;
import com.sun.jbi.management.message.MessageLocalizationInfo;

/**
 *
 * @author Sun Microsystems
 *
 */
public class MessageLocalizationInfoTest extends JBIMessageTestCase {
    private MessageLocalizationInfo messageLocalizationInfo = null;

    public void setUp() {
        messageLocalizationInfo = new MessageLocalizationInfo();
    }

    public void tearDown() {
        messageLocalizationInfo = null;
    }

    public void testToken() {
        try {
            messageLocalizationInfo.setToken(null);
            fail("Token cannot be null");
        } catch (JBIMessageException jme) {}
    }

    public void testMessage() {
        try {
            messageLocalizationInfo.setMessage(null);
            fail("Message cannot be null");
        } catch (JBIMessageException jme) {}
    }

//     public void testValidate() {
//         try {
//             messageLocalizationInfo.validate();
//             fail("Validation passes when token is null");
//         } catch (JBIMessageException jme) {
//         }
//         messageLocalizationInfo.setToken("lostConnection");

//         try {
//             messageLocalizationInfo.validate();
//             fail("Validation passes when message is null");
//         } catch (JBIMessageException jme) {
//         }
//         messageLocalizationInfo.setMessage("Malformed {pattern");

//         try {
//             messageLocalizationInfo.validate();
//             fail("Validation passes when message pattern in invalid");
//         } catch (JBIMessageException jme) {
//         }
//         messageLocalizationInfo.setMessage("Lost connection to {0} at {1}");

//         try {
//             messageLocalizationInfo.validate();
//             fail("Validation passes when parameter count doesn't match pattern");
//         } catch (JBIMessageException jme) {
//         }
//         messageLocalizationInfo.setParameters(new Object[] { "main server", new Date() });

//         try {
//             messageLocalizationInfo.validate();
//         } catch (JBIMessageException jme) {
//             fail("Validation fails under correct conditions: " + jme.getMessage());
//         }
//     }

    public void testGetString() {
        String exceptionInfoString = newInstance().getString();
        validateXMLString(exceptionInfoString, "MessageLocalizationInfo");
    }

    static MessageLocalizationInfo newInstance() {
        MessageLocalizationInfo messageLocalizationInfo =
            new MessageLocalizationInfo("missingResource",
                                        "Risorsa mancante: {0}",
                                        new Object[] { "jbi.xml" });
        return messageLocalizationInfo;
    }
}
