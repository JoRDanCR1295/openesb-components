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
 * @(#)ExtensionFilterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;
import junit.framework.*;
import java.io.*;
/*
 * ExtensionFilterTest.java
 * JUnit based test
 *
 * Created on February 15, 2007, 5:10 PM
 */

/**
 *
 * @author rdwivedi
 */
public class ExtensionFilterTest extends TestCase {
    
    public ExtensionFilterTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ExtensionFilterTest.class);
        
        return suite;
    }

    /**
     * Test of accept method, of class com.sun.jbi.engine.iep.core.runtime.util.ExtensionFilter.
     */
    public void testAccept() {
        try {
        
        File file1 = File.createTempFile("a12",".txt");
        File file2 = File.createTempFile("a12",".fst");
        File file3 = File.createTempFile("a12",".jst");
        File file4 = File.createTempFile("a12",".tmt");
        String[] exts = {".txt", ".fst", ".jst"};
        com.sun.jbi.engine.iep.core.runtime.util.ExtensionFilter instance = new com.sun.jbi.engine.iep.core.runtime.util.ExtensionFilter(exts);
        
         
        boolean result = instance.accept(file1);
        assertTrue( result);
        assertTrue( instance.accept(file2));
        assertTrue( instance.accept(file3));
        assertFalse( instance.accept(file4));
        
        } catch(Exception e) {
            fail("The test case is a failed due to exception. " + e.getMessage());
        }
        // TODO review the generated test code and remove the default call to fail.
        
    }
    
}
