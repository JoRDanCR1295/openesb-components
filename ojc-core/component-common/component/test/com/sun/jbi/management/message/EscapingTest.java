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
 * @(#)EscapingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;


public class EscapingTest extends JBIMessageTestCase {

    public void setUp() {
    }

    public void tearDown() {
    }

    // making sure the test works without escaping
    public void testNoEscaping() {    
        JBITaskMessageBuilder builder = new DefaultJBITaskMessageBuilder();
        builder.setComponentName("myComponent");
        String message = builder.createExceptionMessage(
                "deploy", // taskId
                "unsuccessfulDeploymentInvalid provides", // locToken
                "Unsuccessful deployment of service unit: {0} because provides is invalid", // locMessage
                "myServiceUnit provides", // locParam
                new Exception("Oh man! no escaping test, provides not valid")); // exception
        validateXMLString(message, "ManagementMessage");
    }
    
    // making sure the test works with escaping    
    public void testSimpleEscaping() {
        JBITaskMessageBuilder builder = new DefaultJBITaskMessageBuilder();
        builder.setComponentName("myComponent");
        String message = builder.createExceptionMessage(
                "deploy", // taskId
                "unsuccessfulDeploymentInvalid <provides>", // locToken
                "Unsuccessful deployment of service unit: {0} because <provides> is invalid \" ' & < > \\ test end.", // locMessage
                "myServiceUnit <provides>", // locParam
                new Exception("Oh man! <escaping> test, <provides> not valid")); // exception
        
        validateXMLString(message, "ManagementMessage");
    }
    
    // confirm namespace-qualified messages don't get escaped erroneously
    public void testDontEscape() {
        JBITaskMessageBuilder builder = new DefaultJBITaskMessageBuilder();
        builder.setComponentName("myComponent");
        String message = builder.createExceptionMessage(
                "deploy", // taskId
                "unsuccessfulDeploymentInvalid <provides>", // locToken
                "Unsuccessful deployment of service unit: {0} because look, here's this: {myNS}fooOperation", // locMessage
                "myServiceUnit <provides>", // locParam
                new Exception("Oh man! <escaping> test, <provides> not valid")); // exception
        
        validateXMLString(message, "ManagementMessage");
    }
}
