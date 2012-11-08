package test.jbi.jmsbc.integration.testcases.attachments;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for test.jbi.jmsbc.integration.testcases.attachments");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestAttachmentsForBinary.class);
		suite.addTestSuite(TestAttachmentsForTextMessage.class);
		//$JUnit-END$
		return suite;
	}

}
