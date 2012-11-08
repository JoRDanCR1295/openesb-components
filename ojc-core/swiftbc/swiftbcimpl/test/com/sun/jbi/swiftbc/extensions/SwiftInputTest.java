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
 * @(#)SwiftInputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author traghuna
 */
public class SwiftInputTest extends TestCase {
    SwiftInput instance = new SwiftInput();

    public SwiftInputTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SwiftInputTest.class);

        return suite;
    }

    /**
     * Test of setSwiftMessage and getSwiftMessage method, of class com.sun.jbi.swiftbc.extensions.SwiftInput.
     */
    public void testSetGetSwiftMessage() {
        /*
        System.out.println("Testing setSwiftMessage and getSwiftMessage");

        SwiftMessage message = (new SAGObjectFactoryFactory()).getObjectFactory().getNewMessage();
        instance.setSwiftMessage(message);
        SwiftMessage result = instance.getSwiftMessage();
        assertEquals(message, result);

        System.out.println("Successfully tested setSwiftMessage and getSwiftMessage");
        */
    }

}
