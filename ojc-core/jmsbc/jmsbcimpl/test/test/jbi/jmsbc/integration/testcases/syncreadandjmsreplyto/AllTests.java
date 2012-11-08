package test.jbi.jmsbc.integration.testcases.syncreadandjmsreplyto;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.syncread");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestSynchronousRead.class);
		suite.addTestSuite(TestSynchronousReadWithJMSOptions.class);
		suite.addTestSuite(TestSynchronousReadWithLookupURL.class);
		//$JUnit-END$
		return suite;
	}

}
