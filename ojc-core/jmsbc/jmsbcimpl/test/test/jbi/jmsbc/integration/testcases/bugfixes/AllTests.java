package test.jbi.jmsbc.integration.testcases.bugfixes;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.bugfixes");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestMapMessage.class);
		suite.addTestSuite(TestInboundRequestReplyBugFix.class);
		suite.addTestSuite(TestFreezeAtShutdown.class);
		suite.addTestSuite(TestAnyType.class);
		//$JUnit-END$
		return suite;
	}

}
