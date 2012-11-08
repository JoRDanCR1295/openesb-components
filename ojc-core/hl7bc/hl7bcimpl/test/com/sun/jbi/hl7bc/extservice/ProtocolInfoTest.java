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
 * @(#)ProtocolInfoTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice;


import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import junit.framework.*;

/**
 *
 * @author Raghunadh
 */
public class ProtocolInfoTest extends TestCase {

    ProtocolInfo instance = null;

    public ProtocolInfoTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new ProtocolInfo();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ProtocolInfoTest.class);
        return suite;
    }

 
    /**
     * Test of set and get method, of class com.sun.jbi.hl7bc.extservice.ProtocolInfo.
     */
    public void testSetGet() {
        System.out.println("Testing set and get methods");

        String expResult = "localhost";
		String key = "location";
		String val = "localhost";
        instance.put(key,val);
        val = instance.get(key);
        assertEquals(expResult, val);

        System.out.println("Successfully tested setand get");

    }

  
}
