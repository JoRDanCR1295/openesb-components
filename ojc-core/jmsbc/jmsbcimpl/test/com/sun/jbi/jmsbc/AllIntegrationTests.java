package com.sun.jbi.jmsbc;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllIntegrationTests{

	public static Test suite() {
		TestSuite suite = new TestSuite();
		
		//Add test suits there
		addTest((TestSuite)test.jbi.jmsbc.integration.testcases.qos.AllTests.suite(), suite);
		addTest((TestSuite)test.jbi.jmsbc.integration.testcases.managmentMbean.AllTests.suite(), suite);
		addTest((TestSuite)test.jbi.jmsbc.integration.testcases.errorstrategy.AllTests.suite(), suite);
		addTest((TestSuite)test.jbi.jmsbc.integration.testcases.bugfixes.AllTests.suite(), suite);
		addTest((TestSuite)test.jbi.jmsbc.integration.testcases.syncreadandjmsreplyto.AllTests.suite(), suite);
		addTest((TestSuite) test.jbi.jmsbc.integration.testcases.nmproperties.AllTests.suite(), suite);
		addTest((TestSuite) test.jbi.jmsbc.integration.testcases.attachments.AllTests.suite(), suite);
		//End----------

		return suite;
	}
	
	private static void addTest(TestSuite source, TestSuite dest){
		for(int i=0; i<source.testCount(); ++i){
			dest.addTest(source.testAt(i));
		}
	}
}
