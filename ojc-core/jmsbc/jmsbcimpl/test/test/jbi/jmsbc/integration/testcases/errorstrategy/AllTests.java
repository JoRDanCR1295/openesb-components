package test.jbi.jmsbc.integration.testcases.errorstrategy;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.errorstrategy");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestCommonFaultErrorStrategyInbound.class);
		suite.addTestSuite(TestCommonFaultErrorStrategyOutbound.class);
		suite.addTestSuite(TestCommonFaultErrorStrategyInboundInOut.class);
		//$JUnit-END$
		return suite;
	}

}
