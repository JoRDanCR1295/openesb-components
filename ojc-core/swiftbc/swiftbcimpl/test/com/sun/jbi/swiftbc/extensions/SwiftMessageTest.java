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
 * @(#)SwiftMessageTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import junit.framework.*;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class SwiftMessageTest extends TestCase {
    /* SwiftMessage instance = (new SAGObjectFactoryFactory()).getObjectFactory().getNewMessage(); */
    
    public SwiftMessageTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SwiftMessageTest.class);
        
        return suite;
    }

    /**
     * Test of getUseType method, of class com.sun.jbi.swiftbc.extensions.SwiftMessage.
     */
    public void testSetGetUseType() {
        /*
        System.out.println("Testing getUseType");
        
        String val = "encoded";
        instance.setUseType(val);
        String result = instance.getUseType();
        assertEquals(val, result);
        
        System.out.println("Successfully getUseType");
        */
    }

       
}
