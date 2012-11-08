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

public class AppConfigNameFieldTest extends TestCase {
    AppConfigNameField appConfigNameField;

    @Override
    protected void setUp() throws Exception {
        appConfigNameField = new AppConfigNameField();
    }

    public void testToString() throws Exception {
        appConfigNameField.setValue("TestnameToString1");
        assertTrue(appConfigNameField.toString().equals("TestnameToString1"));
        
        appConfigNameField.fromString("TestnameToString2");
        assertTrue(appConfigNameField.toString().equals("TestnameToString2"));
        
        appConfigNameField.setValue("");
        assertTrue(appConfigNameField.toString().equals(""));
        
        appConfigNameField.setValue(null);
        assertTrue(appConfigNameField.toString().equals(""));
    }

    public void testFromString() throws Exception {
        appConfigNameField.fromString("TestnameFromString");
        assertTrue(appConfigNameField.toString().equals("TestnameFromString"));
        
        appConfigNameField.fromString("");
        assertTrue(appConfigNameField.toString().equals(""));
        
        appConfigNameField.fromString(null);
        assertTrue(appConfigNameField.toString().equals(""));
    }

    public void testClone() throws Exception {
        appConfigNameField.setValue("TestnameClone");
        Object clone = appConfigNameField.clone();
        assertTrue(clone.getClass() == appConfigNameField.getClass());
        assertTrue(clone != appConfigNameField);
    }

    public void testValidate() throws Exception {
        Object result;
        Object data;
        Object correctData;
        
        // no errors
        data = "TestnameValidateNoError";
        result = appConfigNameField.validate(data);
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
        result = appConfigNameField.validate(data);
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
        result = appConfigNameField.validate(data);
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
        result = appConfigNameField.validate(data);
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
        appConfigNameField.setValue("TestnameToString");
        assertTrue(appConfigNameField.toString().equals("TestnameToString"));
        appConfigNameField.setValue("");
        assertTrue(appConfigNameField.toString().equals(""));
        appConfigNameField.setValue(null);
        assertTrue(appConfigNameField.toString().equals(""));
    }

    public void testGetValue() throws Exception {
        appConfigNameField.setValue("TestnameGetValue1");
        assertTrue(appConfigNameField.getValue().equals("TestnameGetValue1"));
        appConfigNameField.setValue(null);
        assertTrue(appConfigNameField.getValue().equals(""));
        appConfigNameField.setValue("");
        assertTrue(appConfigNameField.getValue().equals(""));
        
        appConfigNameField.fromString("TestnameGetValue2");
        assertTrue(appConfigNameField.getValue().equals("TestnameGetValue2"));
        appConfigNameField.fromString("");
        assertTrue(appConfigNameField.getValue().equals(""));
        appConfigNameField.fromString(null);
        assertTrue(appConfigNameField.getValue().equals(""));
    }
}
