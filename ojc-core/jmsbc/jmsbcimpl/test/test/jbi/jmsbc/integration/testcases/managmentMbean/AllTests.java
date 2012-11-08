package test.jbi.jmsbc.integration.testcases.managmentMbean;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.managmentMbean");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestManagmentMBean.class);
		//$JUnit-END$
		return suite;
	}

}
