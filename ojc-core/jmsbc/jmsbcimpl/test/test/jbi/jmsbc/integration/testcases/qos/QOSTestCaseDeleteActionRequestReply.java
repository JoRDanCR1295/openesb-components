package test.jbi.jmsbc.integration.testcases.qos;

import java.io.IOException;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueSender;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.xml.namespace.QName;

import junit.framework.Assert;
import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.test.util.SendRequestReplyMessage;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.util.GUIDUtil;

public class QOSTestCaseDeleteActionRequestReply extends IntegrationTestCase {
	
	private static final String SA_NAME = "JMSBC-TEST_REDELIVERY_INOUT_SA";
	private static final int WAIT_TIME = 500;
	private static final int MAX_ATTEMPTS = 5;
	private static final String CONSUMER_EP = "JMSPort";
	private static final QName JMS_SERVICE = new QName("http://j2ee.netbeans.org/wsdl/JMS","JMSService");
	private static final String OP_NAME = "op-name";
	private static final QName DELIVERY_SERVICE_NAME = new QName("urn:JMSBC-TEST_REDELIVERY-SA","Service");
	private static final String DELIVERY_EP = "deliveryEP";
	transient private SendRequestReplyMessage mSendRequestReplyMessage = new SendRequestReplyMessage("RequestReply", "ReplyTo");

	public void testRedlivery() throws Throwable{
		
		//Create TestSU
		String zipTestSU = createTestSU();
		
		//Activate delivery end points
		getConnection().execute(new Command() {
			public Serializable execute(ComponentContext context) throws Exception {
				JbiHelper.activateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
				return "SUCCESS";
			}
		});
		
		
		final String msg = "Test Message";
		try{
			//Now deploy and start SA
			getInstaller().deployServiceAssembly(zipTestSU);
			getInstaller().startServiceAssembly(SA_NAME);
			
			//Drain all the messages from the queue before testing
			getConnection().execute(new Command() {
				public Serializable execute(ComponentContext context) throws Exception {
					for(;;){
						Thread.sleep(WAIT_TIME * 10);
						InOut ex = (InOut)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
						if(ex == null)
							break;
						ex.setOutMessage(ex.getInMessage());
						context.getDeliveryChannel().sendSync(ex);
					}
					return "SUCCESS";
				}
			});

			//Send a message
			//TODO: this would be implemented when JMSReplyTo is working
			mSendRequestReplyMessage.sendMessageToQueue(GUIDUtil.generateGUID());

			//Get the message
			Object obj = getConnection().execute(new Command() {
				public Serializable execute(ComponentContext context) throws Exception {
					int i = 0;
					for(; i<=MAX_ATTEMPTS; ++ i){
						Thread.sleep(WAIT_TIME * 2);
						InOut ex = (InOut)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
						if(ex == null)
							break;
						ex.setStatus(ExchangeStatus.ERROR);
						context.getDeliveryChannel().send(ex);
					}
					if(i > MAX_ATTEMPTS)
						return "SUCCESS";
					return "FAIL";
				}
			});

			//Compare test result
			Assert.assertEquals("SUCCESS", obj);
			

			//Message  is not deleted. That is OK for InOut message.
			obj = getConnection().execute(new Command() {
				public Serializable execute(ComponentContext context) throws Exception {
						Thread.sleep(WAIT_TIME * 10);
						InOut ex = (InOut)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
						if(ex != null && ex.getStatus() != ExchangeStatus.ERROR){
							ex.setOutMessage(ex.getInMessage());
							context.getDeliveryChannel().sendSync(ex);
							return "SUCCESS";
						}
						return "FAIL";
				}
			});

			//Compare test result. Message should not be delivered
			Assert.assertEquals("SUCCESS", obj);
			
		}finally{
			try{
				getConnection().execute(new Command() {
					public Serializable execute(ComponentContext context) throws Exception {
						JbiHelper.deactivateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
						return "SUCCESS";
					}
				});
			}catch(Throwable t){}
		}		
	}
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
	}


	@Override
	protected void tearDown() throws Exception {
		//Make sure that SA is undeployed
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
		// TODO Auto-generated method stub
		super.tearDown();
	}

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-SU-RedeliveryTest", "JMS-BC-SU-RedeliveryTest");
		su.addWsdl(Configuration.getPath(getClass(), "JMSInboundRequestReply.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbiRequestReplyInbound.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		//Now add connection
		SAAssembler.Redelivery redelivery = new SAAssembler.Redelivery(MAX_ATTEMPTS, WAIT_TIME, SAAssembler.Redelivery.OnFailure.delete);
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, DELIVERY_SERVICE_NAME, DELIVERY_EP, redelivery);
		
		return sa.assemble(Configuration.getWorkingDir());
	}
	

}
