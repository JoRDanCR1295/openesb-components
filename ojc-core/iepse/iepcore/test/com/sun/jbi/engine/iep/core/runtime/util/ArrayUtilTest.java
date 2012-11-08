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
 * @(#)ArrayUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;
import junit.framework.*;
/*
 * ArrayUtilTest.java
 * JUnit based test
 *
 * Created on February 15, 2007, 5:10 PM
 */

/**
 *
 * @author rdwivedi
 */
public class ArrayUtilTest extends TestCase {
    
    public ArrayUtilTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ArrayUtilTest.class);
        
        return suite;
    }

    /**
     * Test of duplicate method, of class com.sun.jbi.engine.iep.core.runtime.util.ArrayUtil.
     */
    public void testDuplicate() {
        
        Object[] a = null;
        
        Object[] expResult = null;
        Object[] result = com.sun.jbi.engine.iep.core.runtime.util.ArrayUtil.duplicate(a);
        assertEquals(expResult, result);
        
        Object[] b = {"A","B","C"};
        result = com.sun.jbi.engine.iep.core.runtime.util.ArrayUtil.duplicate(b);
        assertEquals(b[0],result[0]);
         assertEquals(b[1],result[1]);
          assertEquals(b[2],result[2]);
        
        
    }
    
}
