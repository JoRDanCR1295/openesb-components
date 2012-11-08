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
 * @(#)HL7InputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author traghuna
 */
public class HL7InputTest extends TestCase {
    HL7Input instance = new HL7Input();

    public HL7InputTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HL7InputTest.class);

        return suite;
    }

    /**
     * Test of setHL7Message and getHL7Message method, of class com.sun.jbi.hl7bc.extensions.HL7Input.
     */
    public void testSetGetHL7Message() {
        System.out.println("Testing setHL7Message and getHL7Message");

        HL7Message message = new HL7Message();
        instance.setHL7Message(message);
        HL7Message result = instance.getHL7Message();
        assertEquals(message, result);

        System.out.println("Successfully tested setHL7Message and getHL7Message");
    }

}
