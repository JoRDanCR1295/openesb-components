package test.jbi.jmsbc.integration.testcases.nmproperties;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.nmproperties");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestNMPropertiesFailureTestCases.class);
		suite.addTestSuite(TestNMPropertiesOuboundInOnly.class);
		suite.addTestSuite(TestNMPropertiesOuboundInOut.class);
		suite.addTestSuite(TestNMPropertiesOuboundInOut2.class);
		suite.addTestSuite(TestNMPropertiesSyncRead.class);
		//$JUnit-END$
		return suite;
	}

}
