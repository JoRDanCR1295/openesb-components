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
import test.jbi.integration.test.util.SendReplyToInOutMessageExchange;
import test.jbi.integration.test.util.SendRequestReplyMessage;

public class TestCommonFaultErrorStrategyInboundInOut extends IntegrationTestCase {

	private static final String SA_NAME_CONSUMER = "TEST-ERROR-STRATEGY-SA-INBOUND-CONSUMER";

	//Set service end points
	private static final String TARGET_NAMESPACE = "http://j2ee.netbeans.org/wsdl/JMSInboundInOut";
	private static final QName JMS_SERVICE = new QName(TARGET_NAMESPACE,"JMSInboundInOutService");
	private static final String CONSUMER_EP = "JMSInboundInOutPort";

	//set operation name
	private static final QName OPERATOR = new QName("JMSInboundInOutOperation");
	private static final QName OUT_MESSAGE = new QName(
			TARGET_NAMESPACE, "JMSInboundInOutOperationResponse");
	
	private String zipConsumerSA;
	private ActivateDeactivateEndpoint mActivateCmd = new ActivateDeactivateEndpoint(
			JMS_SERVICE, CONSUMER_EP); 
	
	transient private SendRequestReplyMessage mSendRequestReplyMessage = new SendRequestReplyMessage(
			"RequestReplyInOut", "RequestReplyReplyTo"); 
	public void testSendClientError() throws Throwable {
		Object obj;
		//Deploy and start consumer
		getInstaller().deployServiceAssembly(createSAConsumer());
		getInstaller().startServiceAssembly(SA_NAME_CONSUMER);
		Thread.sleep(5000);

		//drain all the messages
		String[] parts =  new String[]{"reply"};
		PeekNextMessageExchangeInput peekCmd = new PeekNextMessageExchangeInput(JMS_SERVICE, CONSUMER_EP);
		SendReplyToInOutMessageExchange sendReply = new SendReplyToInOutMessageExchange(
				JMS_SERVICE, CONSUMER_EP, OPERATOR, parts, OUT_MESSAGE);
		for(;;){
			obj = getConnection().execute(peekCmd);
			if(obj instanceof Constants.Result){
				break;
			}
			obj = getConnection().execute(sendReply);
		}

		//send a message
		mSendRequestReplyMessage.sendMessageToQueue("testMessage");
		Thread.sleep(5000);
		
		obj = getConnection().execute(peekCmd);
		QName bad_message = new QName(
				TARGET_NAMESPACE, "JMSInboundInOutOperationResponse123");
		sendReply.setMessage(bad_message);
		obj = getConnection().execute(sendReply);
		assertFalse(obj instanceof Constants.Result);
		Object[] arr = (Object[])obj;
		assertTrue(arr[1] != null);
		assertTrue(((String)arr[1]).equalsIgnoreCase("Client"));
	}
	
	public void testSendError() throws Throwable {
		Object obj;
		//Deploy and start consumer
		getInstaller().deployServiceAssembly(createSAConsumer());
		getInstaller().startServiceAssembly(SA_NAME_CONSUMER);
		Thread.sleep(5000);

		//drain all the messages
		String[] parts =  new String[]{"reply"};
		PeekNextMessageExchangeInput peekCmd = new PeekNextMessageExchangeInput(JMS_SERVICE, CONSUMER_EP);
		SendReplyToInOutMessageExchange sendReply = new SendReplyToInOutMessageExchange(
				JMS_SERVICE, CONSUMER_EP, OPERATOR, parts, OUT_MESSAGE);
		for(;;){
			obj = getConnection().execute(peekCmd);
			if(obj instanceof Constants.Result){
				break;
			}
			obj = getConnection().execute(sendReply);
		}

		//send a message
		mSendRequestReplyMessage.sendMessageToQueue("testMessage");
		Thread.sleep(5000);
		
		obj = getConnection().execute(peekCmd);
		SendErrorToNextMessageExchange sendErrCmd = new SendErrorToNextMessageExchange(JMS_SERVICE, CONSUMER_EP);
		sendErrCmd.setError(new Exception("Client error"));
		sendErrCmd.setFaultCode("Client");
		sendErrCmd.setFaultDetails("Client error");
		sendErrCmd.setFaultFactor("testbc");
		obj = getConnection().execute(sendErrCmd);
		assertTrue(obj instanceof Constants.Result);
		assertEquals(Constants.Result.PASS, (Constants.Result)obj);
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		unDeployTestSA();
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

	private String createSAConsumer() throws IOException {
		if(zipConsumerSA != null)
			return zipConsumerSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU-Consumer", "JMS-BC-Test-SU-Consumer");
		su.addWsdl(Configuration.getPath(getClass(), "JMSInBoundInOut.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbiConsumerInOut.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME_CONSUMER, SA_NAME_CONSUMER);
		sa.addSUAssembler(su);
		zipConsumerSA = sa.assemble(Configuration.getWorkingDir());
		return zipConsumerSA;
	}

	private void unDeployTestSA() {
		try{
			getInstaller().undeployServiceAssembly(SA_NAME_CONSUMER);
		}catch(Throwable t){}
	}

}
