package test.jbi.jmsbc.integration.testcases.qos;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.qos");
		//$JUnit-BEGIN$
		suite.addTestSuite(QOSTestCaseRedliveryDeleteAction.class);
		suite.addTestSuite(QOSTestCaseSuspendedActionRequestReply.class);
		suite.addTestSuite(QOSTestCaseThrottling.class);
		suite.addTestSuite(QOSTestCaseRedliverySuspendAction.class);
		suite.addTestSuite(QOSTestCaseRedirectActionRequestReply.class);
		suite.addTestSuite(QOSTestCaseDeleteActionRequestReply.class);
		suite.addTestSuite(QOSTestCaseRegularTestForSanity.class);
		suite.addTestSuite(QOSTestCaseErrorActionRequestReply.class);
		suite.addTestSuite(QOSTestCaseRedliveryErrorAction.class);
		suite.addTestSuite(QOSTestCaseRedlivery.class);
		//$JUnit-END$
		return suite;
	}

}
