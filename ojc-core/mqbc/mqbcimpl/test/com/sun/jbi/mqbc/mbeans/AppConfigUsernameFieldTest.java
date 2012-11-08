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
 */

package com.sun.jbi.mqbc.mbeans;

import junit.framework.TestCase;

public class AppConfigUsernameFieldTest extends TestCase {
    AppConfigUsernameField appConfigUsernameField;

    @Override
    protected void setUp() throws Exception {
        appConfigUsernameField = new AppConfigUsernameField();
    }

    public void testToString() throws Exception {
        appConfigUsernameField.setValue("TestnameToString1");
        assertTrue(appConfigUsernameField.toString().equals("TestnameToString1"));
        
        appConfigUsernameField.fromString("TestnameToString2");
        assertTrue(appConfigUsernameField.toString().equals("TestnameToString2"));
        
        appConfigUsernameField.setValue("");
        assertTrue(appConfigUsernameField.toString().equals(""));
        
        appConfigUsernameField.setValue(null);
        assertTrue(appConfigUsernameField.toString().equals(""));
    }

    public void testFromString() throws Exception {
        appConfigUsernameField.fromString("TestnameFromString");
        assertTrue(appConfigUsernameField.toString().equals("TestnameFromString"));
        
        appConfigUsernameField.fromString("");
        assertTrue(appConfigUsernameField.toString().equals(""));
        
        appConfigUsernameField.fromString(null);
        assertTrue(appConfigUsernameField.toString().equals(""));
    }

    public void testClone() throws Exception {
        appConfigUsernameField.setValue("TestnameClone");
        Object clone = appConfigUsernameField.clone();
        assertTrue(clone.getClass() == appConfigUsernameField.getClass());
        assertTrue(clone != appConfigUsernameField);
    }

    public void testValidate() throws Exception {
        Object result;
        Object data;
        Object correctData;
        
        // no errors
        data = "TestnameValidateNoError";
        result = appConfigUsernameField.validate(data);
        if (result != null) {
            if (!result.getClass().isArray() || !(result instanceof String[])) {
                fail("result expected to be null or a String[], got instead "
                        + result.getClass().getName());
            } else {
                String[] results = (String[]) result;
                assertTrue(results.length == 0);
            }
        }
        
        // correctable error
        data = " 1Hello~2~~World3";
        correctData = "1Hello2World3";
        result = appConfigUsernameField.validate(data);
        if (result == null) {
            fail("result expected to be not null");
        } else {
            if (!result.getClass().isArray() || !(result instanceof String[])) {
                fail("result expected to be null or String[], got instead "
                        + result.getClass().getName());
            } else {
                String[] results = (String[]) result;
                assertTrue(results.length == 1);
                String correction = results[0];
                assertTrue("Expected/got: " + correctData + "/" + correction,
                        correction.equals(correctData));
            }
        }
        
        
        // correctable error
        data = " !@##@%$^%#($*@ ";
        correctData = "";
        result = appConfigUsernameField.validate(data);
        if (result == null) {
            fail("result expected to be not null");
        } else {
            if (!result.getClass().isArray() || !(result instanceof String[])) {
                fail("result expected to be null or String[], got instead "
                        + result.getClass().getName());
            } else {
                String[] results = (String[]) result;
                assertTrue(results.length == 1);
                String correction = results[0];
                assertTrue("Expected/got: " + correctData + "/" + correction,
                        correction.equals(correctData));
            }
        }
        
        // correctable error
        data = null;
        correctData = "";
        result = appConfigUsernameField.validate(data);
        if (result == null) {
            fail("result expected to be not null");
        } else {
            if (!result.getClass().isArray() || !(result instanceof String[])) {
                fail("result expected to be null or String[], got instead "
                        + result.getClass().getName());
            } else {
                String[] results = (String[]) result;
                assertTrue(results.length == 1);
                String correction = results[0];
                assertTrue("Expected/got: " + correctData + "/" + correction,
                        correction.equals(correctData));
            }
        }
    }

    public void testSetValue() throws Exception {
        appConfigUsernameField.setValue("TestnameToString");
        assertTrue(appConfigUsernameField.toString().equals("TestnameToString"));
        appConfigUsernameField.setValue("");
        assertTrue(appConfigUsernameField.toString().equals(""));
        appConfigUsernameField.setValue(null);
        assertTrue(appConfigUsernameField.toString().equals(""));
    }

    public void testGetValue() throws Exception {
        appConfigUsernameField.setValue("TestnameGetValue1");
        assertTrue(appConfigUsernameField.getValue().equals("TestnameGetValue1"));
        appConfigUsernameField.setValue(null);
        assertTrue(appConfigUsernameField.getValue().equals(""));
        appConfigUsernameField.setValue("");
        assertTrue(appConfigUsernameField.getValue().equals(""));
        
        appConfigUsernameField.fromString("TestnameGetValue2");
        assertTrue(appConfigUsernameField.getValue().equals("TestnameGetValue2"));
        appConfigUsernameField.fromString("");
        assertTrue(appConfigUsernameField.getValue().equals(""));
        appConfigUsernameField.fromString(null);
        assertTrue(appConfigUsernameField.getValue().equals(""));
    }
}
