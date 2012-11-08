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
 * @(#)WSDLUtilitiesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import junit.framework.*;
import javax.xml.namespace.QName;

/**
 *
 * @author sweng
 */
public class WSDLUtilitiesTest extends TestCase {

    public WSDLUtilitiesTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of isBuiltInType method, of class com.sun.jbi.filebc.util.WSDLUtilities.
     */
    public void testIsBuiltInType() {
        System.out.println("Testing isBuiltInType");

        QName typename = QName.valueOf("{http://www.w3.org/2001/XMLSchema}double");

        boolean expResult = true;
        boolean result = WSDLUtilities.isBuiltInType(typename);
        assertEquals(expResult, result);

        typename = QName.valueOf("{http://www.w3.org/2001/XMLSchema}string");
        result = WSDLUtilities.isBuiltInType(typename);
        assertEquals(expResult, result);

        typename = QName.valueOf("{http://www.w3.org/2001/XMLSchema}ENTITIES");
        result = WSDLUtilities.isBuiltInType(typename);
        assertEquals(expResult, result);

        System.out.println("Successfully tested isBuiltInType");
    }
}
