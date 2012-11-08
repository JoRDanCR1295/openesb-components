/**
 *
 */
package test.jbi.integration.testcases.throttling;

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
import javax.jbi.messaging.InOnly;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.SAAssembler.Throttling;
import test.jbi.integration.test.framework.impl.FileBCSUAssembler;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.integration.testcases.Helper;

/**
 * @author Sun Microsystems
 *
 */
public class TestThrottling extends IntegrationTestCase {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * service assembly name
	 */
	private static final String SA_NAME = "FILEBC-TEST_THROTTLING-SA";
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
			"urn:FILEBC-TEST_THROTTLING-SA", "testService");

	/**
	 * provider end-point
	 */
	private static final String PROVIDER_EP = "providerEP";

	private File mInputDir;
	
	


	/**
	 * Test Serial Processing i.e Concurrency = 1.
	 *
	 * @throws Throwable
	 */
	public void testSerialProcessing() throws Throwable {

		// drain all messages that might have remained from previous tests.
		drainTestSEDeliveryChannel();
		
		String workArea = Configuration.getWorkingDir() + File.separator + "testSeq";
		mInputDir = new File(workArea);
		
		// Create TestSU
		// Create TestSU
	    FileBCSUAssembler su = Helper.createTestSU("FILE-BC-SU-SerialProcessingTest", 
	    		"FILE-BC-SU-SerialProcessingTest", "file1.wsdl", workArea, this.getClass());
		String testSAPath = Helper.createTestSA(su, SA_NAME, FILE_SERVICE, CONSUMER_EP, 
				PROVIDER_SERVICE, PROVIDER_EP, null,  new Throttling(1));
//		String zipTestSU = createTestSU("file1.wsdl", new Throttling(1));
		String saName = null;
		try {

			// activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			// write 2 input files
			Helper.writeInputFiles(workArea, 2);
			
			File[] inputfiles = mInputDir.listFiles(new FilenameFilter(){
				public boolean accept(File dir, String name) {
					if(name.startsWith("test.xml")){
						return true;
					}
					return false;
				}
			});
			
			assertEquals(2, inputfiles.length);
			
			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(3000); // wait some time before SU start sending
			// messages

			// Get the messages waiting in TEST SE delivery channel so far 
			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			int i = 0;
			String str = (String) getConnection().execute(receviceMsgCommand);
			while ( !"FAIL".equals(str) ) {
				System.out.println(str);
				str = (String) getConnection().execute(receviceMsgCommand);
				i++;
			}
			
			// we should not have looped more than once, since messages should have been
			// sent serially i.e. next message is sent only after getting reply for the
			// first one.
			assertEquals(1, i);
			
			// After getting first reply, file bc should have sent another message
			// So TEST SE should have one more message waiting it its delivery channel
			i = 0;
			str = (String) getConnection().execute(receviceMsgCommand);
			if( "FAIL".equals(str) ){
				// if there are no more messages yet in Test SE,
				// let the file bc thread run to send the next message
				try{
					Thread.sleep(3000);
				} catch(InterruptedException ie){
					
				}
				// now check for message again
				str = (String) getConnection().execute(receviceMsgCommand);
			}
			
			while ( !"FAIL".equals(str) ) {
				System.out.println(str);
				str = (String) getConnection().execute(receviceMsgCommand);
				i++;
			}
			assertEquals(1, i);
			
		} finally {
			// Un-deploy SA
			getInstaller().undeployServiceAssembly(SA_NAME);
		}
	}

	/**
	 * Test throttling in file-bc
	 *
	 * @throws Throwable
	 */
	public void testConcurrencyGreaterThanOne() throws Throwable {

		// drain all messages that might have remained from previous tests.
		drainTestSEDeliveryChannel();
		
		String workArea = Configuration.getWorkingDir() + File.separator + "testConc";
		mInputDir = new File(workArea);
		
		// Create TestSU
	    FileBCSUAssembler su = Helper.createTestSU("FILE-BC-SU-ThrottlingTest", 
	    		"FILE-BC-SU-ThrottlingTest", "file2.wsdl", workArea, this.getClass());
		String testSAPath = Helper.createTestSA(su, SA_NAME, FILE_SERVICE, CONSUMER_EP, 
				PROVIDER_SERVICE, PROVIDER_EP, null,  new Throttling(6));
//		String zipTestSU = createTestSU("file2.wsdl", new Throttling(6));
		String saName = null;
		try {

			// activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			// write input files
			Helper.writeInputFiles(workArea, 4);
			
			File[] inputfiles = mInputDir.listFiles(new FilenameFilter(){
				public boolean accept(File dir, String name) {
					if(name.startsWith("test.xml")){
						return true;
					}
					return false;
				}
			});
			
			assertEquals(4, inputfiles.length);
			
			// Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(testSAPath);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(3000); // wait some time before SU start sending
			// messages

			// Get the messages waiting in TEST SE delivery channel so far 
			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			int i = 0;
			String str = (String) getConnection().execute(receviceMsgCommand);
			while ( !"FAIL".equals(str) ) {
				System.out.println(str);
				str = (String) getConnection().execute(receviceMsgCommand);
				i++;
			}
			
			// We should have more than one message since concurrency is > 1
			boolean success = false;
			if( i > 1 ){
				success = true;
			}
			
			assertTrue(" i= " + i, success);
				
		} catch(Exception e){
			e.printStackTrace();
		}
			finally {
		
			// Un-deploy SA
			getInstaller().undeployServiceAssembly(SA_NAME);
		}
	}

	private void drainTestSEDeliveryChannel() throws Throwable{
		// Get the messages waiting in TEST SE delivery channel so far 
		ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
		String str = (String) getConnection().execute(receviceMsgCommand);
		while ( !"FAIL".equals(str) ) {
			str = (String) getConnection().execute(receviceMsgCommand);
		}
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Make sure that SA is undeployed
		try {
			System.out.println("setup: undeploy service assembly");
			getInstaller().undeployServiceAssembly(SA_NAME);
			System.out.println("setup: undeploy service assembly - DONE");
		} catch (Throwable t) {
		}

		
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * @param fileResource
	 * @return
	 */
	private StringBuilder getResourceContents(String fileResource) {

		StringBuilder sb = new StringBuilder();

		try {

			InputStream ins = new FileInputStream(fileResource);

			BufferedReader br = new BufferedReader(new InputStreamReader(ins));

			while (true) {
				String line = br.readLine();
				if (line == null)
					break;
				sb.append(line + "\n");

			}

			br.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return sb;
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
			InOnly ex = (InOnly) JbiHelper.getNextMessage(PROVIDER_SERVICE,
					PROVIDER_EP);
			Object[] results = null;
			if (ex != null && ex.getStatus() != ExchangeStatus.ERROR) {
				// ex.setStatus(ExchangeStatus.DONE);
				if (ex.getStatus() != ExchangeStatus.DONE) {
					results = JbiHelper.unwrapFromJBISource(ex.getInMessage()
							.getContent());
//					ex.setOutMessage(ex.getInMessage());
					ex.setStatus(ExchangeStatus.DONE);
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
