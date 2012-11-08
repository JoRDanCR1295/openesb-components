package com.sun.jbi.restbc.jbiadapter.util;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;


public class JsonUtilTest extends TestCase {
    
    
    public JsonUtilTest(String testName) {
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

    public void testParseJsonPairs() throws Exception {
        
    }    
    
  public static Test suite() {
        TestSuite suite = new TestSuite(JsonUtilTest.class);
        
        return suite;
    }    
    
}