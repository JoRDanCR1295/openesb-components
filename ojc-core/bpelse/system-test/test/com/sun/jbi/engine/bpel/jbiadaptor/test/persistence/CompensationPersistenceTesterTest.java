package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class CompensationPersistenceTesterTest extends TestCase {
    /** Creates a new instance of PersistenceTesterTest */
    public CompensationPersistenceTesterTest(String testname) {
        super(testname);
    }
    
    /** @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
        CommonPersistenceTesterHelper.setUp();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(CompensationPersistenceTesterTest.class);
        return suite;
    }
    
    public void testSimpleScopes() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/compensation/simplescopes", "SimpleScopes.properties");
    }    
    public void testNestedCompensate() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/compensation/nestedcompensate", "NestedCompensate.properties");
    }    
	
    public void testFaultInCH() throws Exception {
        CommonPersistenceTesterHelper.commonCode("bpels/compensation/faultinch", "FaultInCH.properties");
    }    

    public void testCompensateInFlow() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/compensation/compensateinflow", "CompensateInFlow.properties");
    }    

    public void testCompensateScopesInFlow() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/compensation/compensatescopesinflow", "CompensateScopesInFlow.properties");
    }    

    public void testSimpleFlowCompensate() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/compensation/simpleflowcompensate", "SimpleFlowCompensate.properties");
    }    

    public void testDefaultCompensation() throws Exception {
    	CommonPersistenceTesterHelper.commonCode("bpels/compensation/defaultcompensation", "DefaultCompensation.properties");
    }    
    
}
