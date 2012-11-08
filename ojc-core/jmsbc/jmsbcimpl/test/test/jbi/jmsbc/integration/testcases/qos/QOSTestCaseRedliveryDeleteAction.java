package test.jbi.jmsbc.integration.testcases.qos;

import java.io.IOException;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import junit.framework.Assert;
import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;


public class QOSTestCaseRedliveryDeleteAction extends IntegrationTestCase {
	
	private static final String SA_NAME = "JMSBC-TEST_REDELIVERY-SA";
	private static final int WAIT_TIME = 500;
	private static final int MAX_ATTEMPTS = 5;
	private static final String CONSUMER_EP = "JMSPortRecv";
	private static final QName JMS_SERVICE = new QName("http://j2ee.netbeans.org/wsdl/JMS","JMSService");
	private static final QName DELIVERY_SERVICE_NAME = new QName("urn:JMSBC-TEST_REDELIVERY-SA","Service");
	private static final String DELIVERY_EP = "deliveryEP";

	public void test_IntegrationTestRedlivery() throws Throwable{
		ComponentContext context = JbiHelper.getComponentContext();
		//Activate delivery end points
		JbiHelper.activateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
		getInstaller().startServiceAssembly(SA_NAME);
		final String msg = "Test Message";
		try{
			//Drain all the messages from the queue before testing
			for(;;){
				Thread.sleep(WAIT_TIME * 10);
				InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
				if(ex == null)
					break;
				ex.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(ex);
			}

			//Send a message
			ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, "JMSPortSend");
			InOnly inOnly = context.getDeliveryChannel()
					.createExchangeFactory().createInOnlyExchange();
			inOnly.setEndpoint(ep);
			inOnly.setOperation(new QName("JMSOperationSend"));
			NormalizedMessage nm = inOnly.createMessage();
			
			//Create a message 
			String xml = JbiHelper.wrapIntoJBIMessage(new QName(
					"http://j2ee.netbeans.org/wsdl/JMS",
					"JMSOperationSendRequest"),
					new String[] { msg });
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
			
			inOnly.setInMessage(nm);
			context.getDeliveryChannel().sendSync(inOnly);
			if(inOnly.getStatus() != ExchangeStatus.DONE){
				throw inOnly.getError();
			}

			Object obj = "FAIL";
			//Get the message
			int i = 0;
			for(; i<=MAX_ATTEMPTS; ++ i){
				Thread.sleep(WAIT_TIME * 10);
				InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
				if(ex == null)
					break;
				ex.setStatus(ExchangeStatus.ERROR);
				context.getDeliveryChannel().send(ex);
			}
			if(i > MAX_ATTEMPTS)
				obj = "SUCCESS";

			//Compare test result
			Assert.assertEquals("SUCCESS", obj);
			//Message should not be redelivered since it has been deleted
			obj = "FAIL";
			Thread.sleep(WAIT_TIME * 10);
			InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
			if(ex != null && ex.getStatus() != ExchangeStatus.ERROR){
				ex.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(ex);
				obj = "SUCCESS";
			}
			//Compare test result. Message should not be delivered
			Assert.assertEquals("FAIL", obj);
			
		}finally{
			try{
				JbiHelper.deactivateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
			}catch(Throwable t){}
		}		
	}

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-SU-RedeliveryTest", "JMS-BC-SU-RedeliveryTest");
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		//Now add connection
		SAAssembler.Redelivery redelivery = new SAAssembler.Redelivery(MAX_ATTEMPTS, WAIT_TIME, SAAssembler.Redelivery.OnFailure.delete);
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, DELIVERY_SERVICE_NAME, DELIVERY_EP, redelivery);
		
		return sa.assemble(Configuration.getWorkingDir());
	}
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
		String zipTestSU = createTestSU();
		getInstaller().deployServiceAssembly(zipTestSU);
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

}
