/**
 * 
 */
package test.jbi.integration.testcases.recovery;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.FileBCSUAssembler;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.integration.testcases.Helper;
import test.jbi.integration.test.framework.SAAssembler.Redelivery;
import test.jbi.integration.test.framework.SAAssembler.Throttling;

import com.sun.jbi.filebc.util.EPUtil;

/**
 * @author Sujit Biswas
 * 
 */
public class TestRecovery extends IntegrationTestCase {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * service assembly name
	 */
	private static final String SA_NAME = "FILEBC-TEST_RECOVERY-SA";
	/**
	 * consuming end-point
	 */
	private static final String CONSUMER_EP = "filePort";
	/**
	 * consuming end-point service name
	 */
	private static final QName FILE_SERVICE = new QName(
			"http://j2ee.netbeans.org/wsdl/file", "fileService");

	/**
	 * provider end-point service name
	 */
	private static final QName PROVIDER_SERVICE = new QName(
			"urn:FILEBC-TEST_RECOVERY-SA", "testService");

	/**
	 * provider end-point
	 */
	private static final String PROVIDER_EP = "providerEP";

	/**
	 * test recovery in file-bc
	 * 
	 * @throws Throwable
	 */
	public void testRecovery() throws Throwable {

		String inputDir = Configuration.getWorkingDir() + File.separator + "recovery";
		
		// Create TestSU
		FileBCSUAssembler su = Helper.createTestSU(
				"FILE-BC-SU-RecoveryTest", "FILE-BC-SU-RecoveryTest",
				"file.wsdl", inputDir, this.getClass());
		String testSAPath = Helper.createTestSA(su, SA_NAME,
				FILE_SERVICE, CONSUMER_EP, PROVIDER_SERVICE, PROVIDER_EP, null,
				null);
		String saName = null;
		try {
			// activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			// number of files to recover
			String epWorkAreaBase = inputDir + File.separator + EPUtil.getWorkAreaBaseDir(FILE_SERVICE, CONSUMER_EP);
			
			String workArea = epWorkAreaBase + File.separator + "filebc-in-processing";
			int rCount = 5;
			Helper.writeInputFiles(workArea, rCount);

			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(2000); // wait some time before SU start sending
			// messages

			// Drain all the messages meant for this EP
			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			String str;
			int i = 0;
			while (!(str = (String) getConnection().execute(receviceMsgCommand))
					.equals("FAIL")) {

				System.out.println(str);
				i++;

			}

			assertEquals(rCount, i);

		} finally {
			// Un-deploy SA
			getInstaller().undeployServiceAssembly(SA_NAME);
		}
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}

	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public static class ActivateDeactivateEP implements Command {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		public boolean activate;

		public Serializable execute(ComponentContext context) throws Exception {
			if (activate) {
				JbiHelper.activateEndpoint(context, PROVIDER_SERVICE,
						PROVIDER_EP);
			} else {
				JbiHelper.deactivateEndpoint(context, PROVIDER_SERVICE,
						PROVIDER_EP);
			}
			return "SUCCESS";
		}
	}

	public static class ReceiveMsgCommand implements Command {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public Serializable execute(ComponentContext context) throws Exception {
			InOut ex = (InOut) JbiHelper.getNextMessage(PROVIDER_SERVICE,
					PROVIDER_EP);
			Object[] results = null;
			if (ex != null && ex.getStatus() != ExchangeStatus.ERROR) {
				// ex.setStatus(ExchangeStatus.DONE);
				if (ex.getStatus() != ExchangeStatus.DONE) {
					results = JbiHelper.unwrapFromJBISource(ex.getInMessage()
							.getContent());
					ex.setOutMessage(ex.getInMessage());
					context.getDeliveryChannel().send(ex);
				}
			}
			if (results != null && results.length > 0) {
				return ((Text) results[0]).getTextContent();
			}
			return "FAIL";
		}

	}

}
