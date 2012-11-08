/**
 * 
 */
package test.jbi.integration.testcases.nmprops;

import java.io.File;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.jbi.messaging.NormalizedMessage;

import org.w3c.dom.Text;

import com.sun.jbi.filebc.FileMeta;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.impl.FileBCSUAssembler;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.integration.testcases.Helper;

/**
 * @author Nitin Nahata
 * 
 */
public class TestNMProperties extends IntegrationTestCase {

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

	private static final String NMPROP_OUT_FILENAME = "nm_prop_output.xml";
	private static final String WSDL_OUT_FILENAME = "output.xml";
	
	/**
	 * Test Dynamic Normalized Message Properties
	 * Test BC provider response message has NM (Normalized Message) properties set. 
	 * Values of the properties from the NM should override values configured in WSDL
	 */
	public void testNMProperties() throws Throwable {

		String inputDir = Configuration.getWorkingDir() + File.separator + "nm-properties";

		// Create TestSU
		FileBCSUAssembler su = Helper.createTestSU(
				"FILE-BC-SU-ProtocolTest", "FILE-BC-SU-ProtocolTest",
				"in-out.wsdl", inputDir, this.getClass());
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
			
			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(2000); // wait some time before SU start sending
			// messages

 
			// Dynamic dir and filename that will be set by Test BC provider.
			String dyDir = Configuration.getWorkingDir() + File.separator + "nm-properties"
			+ File.separator + "dynamic-out";
			String dyFileName =  "nm_prop_output.xml";
			
			//Make Test BC receive and respond to the message
			ResponseWithNMPropertyCommand receviceMsgCommand = 
				new ResponseWithNMPropertyCommand( dyDir, dyFileName);
			String str;
			int i = 0;
			while (!(str = (String) getConnection().execute(receviceMsgCommand))
					.equals("FAIL")) {
				i++;
			}

			// wait for the output file to be written
			Thread.sleep(1000);

			// Check output file was written to dynamic dir and filename instead of 
			// whats configured in the WSDL. 
			File outFile = new File(dyDir + File.separator + dyFileName);
			System.out.println("Checking output file: " + outFile.getAbsolutePath());
			assertEquals(true, outFile.exists());
			
		} finally {
			// Un-deploy SA
			getInstaller().undeployServiceAssembly(SA_NAME);
		}
	}

	/**
	 * Negative Test - Dynamic Normalized Message Properties  
	 * Test BC provider response message has NO NM (Normalized Message) properties set. 
	 * Configurations from the WSDL would apply in this case.  
	 */
	public void testNoNMProperties() throws Throwable {

		String inputDir = Configuration.getWorkingDir() + File.separator + "nm-properties";

		// Create TestSU
		FileBCSUAssembler su = Helper.createTestSU(
				"FILE-BC-SU-ProtocolTest", "FILE-BC-SU-ProtocolTest",
				"in-out.wsdl", inputDir, this.getClass());
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
			
			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(2000); // wait some time before SU start sending
			// messages

			//Make Test BC receive and respond to the message 
			ResponseWithOutNMPropertyCommand receviceMsgCommand = new ResponseWithOutNMPropertyCommand();
			String str;
			int i = 0;
			while (!(str = (String) getConnection().execute(receviceMsgCommand))
					.equals("FAIL")) {
				i++;
			}

			// wait for the output file to be written
			Thread.sleep(1000);

			// Output file should be written at the filename and dir specified in the WSDL. 
			File outFile = new File(inputDir + File.separator + WSDL_OUT_FILENAME);
			System.out.println("Checking output file: " + outFile.getAbsolutePath());
			assertEquals(true, outFile.exists());
			
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

	public static class ResponseWithNMPropertyCommand implements Command {
		private static final long serialVersionUID = 1L;

		//dynamic dir and filename
		String dyDir;
		String dyFileName;
		
		ResponseWithNMPropertyCommand(String dir, String fileName){
			dyDir = dir;
			dyFileName = fileName;
		}
		
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
					
					// send response with NM Properties
				
					nm.setProperty(FileMeta.NMPROP_INBOUND_FILEDIR, dyDir);
					nm.setProperty(FileMeta.NMPROP_INBOUND_FILENAME, dyFileName);
					nm.setContent(ex.getInMessage().getContent());
					ex.setOutMessage(nm);
					context.getDeliveryChannel().sendSync(ex);
				}
				
				if(ex.getStatus() == ExchangeStatus.DONE){
					return "PASS";
				 } else{
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
		
		public static class ResponseWithOutNMPropertyCommand implements Command {
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
						
						// send response with no NM Properties.
						Source content = ex.getInMessage().getContent();
						nm.setContent(content);
						ex.setOutMessage(nm);
						context.getDeliveryChannel().sendSync(ex);
					}
					
					if(ex.getStatus() == ExchangeStatus.DONE){
						return "PASS";
					 } else{
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
