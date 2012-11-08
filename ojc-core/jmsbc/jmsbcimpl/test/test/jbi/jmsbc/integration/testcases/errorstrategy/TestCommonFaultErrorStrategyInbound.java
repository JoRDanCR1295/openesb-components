package test.jbi.jmsbc.integration.testcases.errorstrategy;

import java.io.IOException;

import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.test.util.ActivateDeactivateEndpoint;
import test.jbi.integration.test.util.Constants;
import test.jbi.integration.test.util.GetAndAckInOnlyMessageExchange;
import test.jbi.integration.test.util.PeekNextMessageExchangeInput;
import test.jbi.integration.test.util.SendErrorToNextMessageExchange;
import test.jbi.integration.test.util.SendInOnlyMessageExchange;

public class TestCommonFaultErrorStrategyInbound extends IntegrationTestCase {

	private static final String SA_NAME_PROVIDER = "TEST-ERROR-STRATEGY-SA-INBOUND-PROVIDER";
	private static final String SA_NAME_CONSUMER = "TEST-ERROR-STRATEGY-SA-INBOUND-CONSUMER";

	//Set service end points
	private static final String TARGET_NAMESPACE = "http://j2ee.netbeans.org/wsdl/JMSOut";
	private static final QName JMS_SERVICE = new QName(TARGET_NAMESPACE,"JMSOutService");
	private static final String CONSUMER_EP = "JMSOutPort";

	//TestBC EP
	private static final QName TEST_SERVICE = new QName("urn:test-service","Service");
	private static final String TEST_CONSUMER_EP = "recv";

	//set operation name
	private static final QName OPERATOR = new QName("JMSOutOperation");
	private static final QName MESSAGE = new QName(
			TARGET_NAMESPACE, "JMSOutOperationRequest");
	
	private String zipConsumerSA;
	private String zipProviderSA;
	private ActivateDeactivateEndpoint mActivateCmd = new ActivateDeactivateEndpoint(
			TEST_SERVICE, TEST_CONSUMER_EP); 
	
	public void testSendClientError() throws Throwable {
		Object obj;
		//Deploy and start consumer
		getInstaller().deployServiceAssembly(createSAConsumer());
		getInstaller().startServiceAssembly(SA_NAME_CONSUMER);
		Thread.sleep(5000);

		//drain all the messages
		GetAndAckInOnlyMessageExchange ackCmd = new GetAndAckInOnlyMessageExchange(TEST_SERVICE, TEST_CONSUMER_EP);
		for(;;){
			obj = getConnection().execute(ackCmd);
			if(obj instanceof Constants.Result){
				break;
			}
		}
		getInstaller().undeployServiceAssembly(SA_NAME_CONSUMER);

		//Deploy and start provider
		getInstaller().deployServiceAssembly(createSAProvider());
		getInstaller().startServiceAssembly(SA_NAME_PROVIDER);
		Thread.sleep(5000);
		//Send a message
		String[] messageParts = new String[]{"<param1>param1</param1><param2>param2</param2>"};
		SendInOnlyMessageExchange sendCmd = new SendInOnlyMessageExchange(
				JMS_SERVICE, CONSUMER_EP, OPERATOR, messageParts, MESSAGE);
		obj = getConnection().execute(sendCmd);
		assertTrue(obj instanceof Constants.Result);
		getInstaller().undeployServiceAssembly(SA_NAME_PROVIDER);

		//Deploy consumer again and send client error
		getInstaller().deployServiceAssembly(createSAConsumer());
		getInstaller().startServiceAssembly(SA_NAME_CONSUMER);
		Thread.sleep(5000);

		//Peek the next message
		PeekNextMessageExchangeInput peekMsgCmd = new PeekNextMessageExchangeInput(TEST_SERVICE, TEST_CONSUMER_EP);
		obj = getConnection().execute(peekMsgCmd);
		assertFalse(obj instanceof Constants.Result);
		SendErrorToNextMessageExchange sendErrCmd = new SendErrorToNextMessageExchange(TEST_SERVICE, TEST_CONSUMER_EP);
		sendErrCmd.setError(new Exception("Client error"));
		sendErrCmd.setFaultCode("Client");
		sendErrCmd.setFaultDetails("Client error");
		sendErrCmd.setFaultFactor("testbc");
		obj = getConnection().execute(sendErrCmd);

		//This test is not verifying anything except that the flow runs without any error
		//For inbound in case of JMSBC if there is a client error it would still redeliver the
		//message but would send an alert to let the user know that message has bad data in it.
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		unDeployTestSA();
		createSAProvider();
		createSAConsumer();
		try{
			mActivateCmd.setActivate(true);
			getConnection().execute(mActivateCmd);
		}catch(Throwable t){
			throw new Exception(t);
		}
	}


	@Override
	protected void tearDown() throws Exception {
		try{
			mActivateCmd.setActivate(false);
			getConnection().execute(mActivateCmd);
		}catch(Throwable t){
			throw new Exception(t);
		}
		unDeployTestSA();
		super.tearDown();
	}

	private String createSAProvider() throws IOException {
		if(zipProviderSA != null)
			return zipProviderSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU-provider", "JMS-BC-Test-SU-provider");
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME_PROVIDER, SA_NAME_PROVIDER);
		sa.addSUAssembler(su);
		zipProviderSA = sa.assemble(Configuration.getWorkingDir());
		return zipProviderSA;
	}
	
	private String createSAConsumer() throws IOException {
		if(zipConsumerSA != null)
			return zipConsumerSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU-Consumer", "JMS-BC-Test-SU-Consumer");
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbiConsumer.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME_CONSUMER, SA_NAME_CONSUMER);
		sa.addSUAssembler(su);
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, TEST_SERVICE, TEST_CONSUMER_EP);
		zipConsumerSA = sa.assemble(Configuration.getWorkingDir());
		return zipConsumerSA;
	}

	private void unDeployTestSA() {
		try{
			getInstaller().undeployServiceAssembly(SA_NAME_PROVIDER);
		}catch(Throwable t){}
		try{
			getInstaller().undeployServiceAssembly(SA_NAME_CONSUMER);
		}catch(Throwable t){}
	}

}
