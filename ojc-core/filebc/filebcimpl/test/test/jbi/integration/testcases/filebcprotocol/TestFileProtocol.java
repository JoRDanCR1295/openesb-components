/**
 * 
 */
package test.jbi.integration.testcases.filebcprotocol;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.xml.namespace.QName;
import javax.jbi.messaging.NormalizedMessage;

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

/**
 * @author Nitin Nahata
 * 
 */
public class TestFileProtocol extends IntegrationTestCase {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * service assembly name
	 */
	private static final String SA_NAME = "FILEBC-TEST_PROTOCOL-SA";
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
	 * A file with the same name as the output file already exists.
	 * WSDL has protect=true on file:message but protectDirectory is not specified
	 * Existing output file should be renamed to: <filename> + <UUID> + ".protected", in
	 * the existing location.
	 */
	public void testOutProtectNoProtectDir() throws Throwable {

		String inputDir = Configuration.getWorkingDir() + File.separator + "file-bc-protocol";

		// Create TestSU
		FileBCSUAssembler su = Helper.createTestSU(
				"FILE-BC-SU-ProtocolTest", "FILE-BC-SU-ProtocolTest",
				"outProtect-No-ProtectDir.wsdl", inputDir, this.getClass());
		String testSAPath = Helper.createTestSA(su, SA_NAME,
				FILE_SERVICE, CONSUMER_EP, PROVIDER_SERVICE, PROVIDER_EP, null,
				null);
		String saName = null;
		try {

			// activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			Helper.writeInputFiles(inputDir, 1);

			// create pre-existing output file which should be protected
			Helper.writeFile(inputDir, "output.xml", "dummy");
			
			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(2000); // wait some time before SU start sending
			// messages

			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			String str;
			int i = 0;
			while (!(str = (String) getConnection().execute(receviceMsgCommand))
					.equals("FAIL")) {
				i++;
			}

			System.out.println("No of messages received by Test BC: " + i);
			
			// wait for the output file to be written
			Thread.sleep(1000);

			// check in-the-way file has been renamed in the existing location. 
			File fInDir = new File(inputDir);
			File[] protectedfiles = fInDir.listFiles(new FilenameFilter(){
				public boolean accept(File dir, String name) {
					if(name.startsWith("output.xml") && name.endsWith(".protected")){
						return true;
					}
					return false;
				}
			});
			
			assertEquals(1, protectedfiles.length);
			
		} finally {
			// Un-deploy SA
			getInstaller().undeployServiceAssembly(SA_NAME);
		}
	}


	/**
	 * A file with the same name as the output file already exists.
	 * WSDL has protect=true on file:message and protectDirectory="protect" 
	 * Existing output file should be tagged and moved to protectDirectory ie. to 
	 * <input-dir>/protect/<filename + UUID + .protected>
	 */
	public void testOutProtectWithProtectDir() throws Throwable {
		
		String inputDir = Configuration.getWorkingDir() + File.separator + "file-bc-protocol";

		// Create TestSU
		FileBCSUAssembler su = Helper.createTestSU(
				"FILE-BC-SU-ProtocolTest", "FILE-BC-SU-ProtocolTest",
				"outProtect-With-ProtectDir.wsdl", inputDir, this.getClass());
		String testSAPath = Helper.createTestSA(su, SA_NAME,
				FILE_SERVICE, CONSUMER_EP, PROVIDER_SERVICE, PROVIDER_EP, null,
				null);
		String saName = null;
		try {

			// activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			Helper.writeInputFiles(inputDir, 1);

			// create pre-existing output file which should be protected
			Helper.writeFile(inputDir, "output.xml", "dummy");
			
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
				i++;
			}

			System.out.println("No of messages received by Test BC: " + i);
			
			// wait for the output file to be written
			Thread.sleep(5000);
			
			// this is the protectDir name in the WSDL.
			String protectDirName = "protect";
			
			// check in-the-way file has been moved to protect dir.
			File fInDir = new File(inputDir + File.separator + protectDirName);
			File[] protectedfiles = fInDir.listFiles(new FilenameFilter(){
				public boolean accept(File dir, String name) {
					if(name.startsWith("output.xml") && name.endsWith(".protected")){
						return true;
					}
					return false;
				}
			});
			
			assertEquals(1, protectedfiles.length);
			
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
		private static final long serialVersionUID = 1L;

		public Serializable execute(ComponentContext context) throws Exception {
			InOut ex = (InOut) JbiHelper.getNextMessage(PROVIDER_SERVICE,
					PROVIDER_EP);
			Object[] results = null;
			if (ex != null && ex.getStatus() != ExchangeStatus.ERROR) {
				NormalizedMessage nm = ex.createMessage();
				
				if (ex.getStatus() != ExchangeStatus.DONE) {
					
					ex.setService(FILE_SERVICE);
					results = JbiHelper.unwrapFromJBISource(ex.getInMessage()
							.getContent());
					nm.setContent(ex.getInMessage().getContent());
					ex.setOutMessage(nm);
					context.getDeliveryChannel().sendSync(ex);
				
				}
				
				if(ex.getStatus() == ExchangeStatus.DONE){
					return "PASS";
				} else {
					Object[] errorDetails = new Object[]{ex.getError(), ex.getProperty("com.sun.jbi.crl.faultcode")};
					return errorDetails;
				}

			}
			if (results != null && results.length > 0) {
				return ((Text) results[0]).getTextContent();
			}
			return "FAIL";
		}

	}

}
