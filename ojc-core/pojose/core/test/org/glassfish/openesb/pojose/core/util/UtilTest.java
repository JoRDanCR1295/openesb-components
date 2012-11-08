/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.core.util;

import javax.xml.transform.Source;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author gpatil
 */
public class UtilTest extends TestCase {
    
    public UtilTest(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Test of jbiMessage2String method, of class Util.
     */

    public void testJbiMessage2String() throws Exception {
        System.out.println("jbiMessage2String");
        Source src = null;
        String expResult = "";
//        String result = Util.jbiMessage2String(src);
//        assertEquals(expResult, result);
//        TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }    
    
  public static Test suite() {
        TestSuite suite = new TestSuite(UtilTest.class);
        
        return suite;
    }    
}
