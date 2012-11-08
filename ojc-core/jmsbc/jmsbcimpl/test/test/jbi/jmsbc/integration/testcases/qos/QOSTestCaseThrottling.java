package test.jbi.jmsbc.integration.testcases.qos;

import java.io.IOException;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
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


public class QOSTestCaseThrottling extends IntegrationTestCase {
	
	private static final String SA_NAME = "JMSBC-TEST_Throttling-SA";
	private static final String CONSUMER_EP = "JMSPortRecv";
	private static final QName JMS_SERVICE = new QName("http://j2ee.netbeans.org/wsdl/JMS","JMSService");
	private static final String OP_NAME = "op-name";
	private static final QName DELIVERY_SERVICE_NAME = new QName("urn:JMSBC-TEST_REDELIVERY-SA","Service");
	private static final String DELIVERY_EP = "deliveryEP";

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
						Thread.sleep(5000);
						InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
						if(ex == null)
							break;
						ex.setStatus(ExchangeStatus.DONE);
						context.getDeliveryChannel().send(ex);
					}
					return "SUCCESS";
				}
			});

			//Send a message
			getConnection().execute(new Command() {
				public Serializable execute(ComponentContext context) throws Exception {
					ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, "JMSPortSend");
					MessageExchangeFactory factory = context.getDeliveryChannel().createExchangeFactory();
					for(int i=0; i<5; ++i){
						InOnly inOnly = factory.createInOnlyExchange();
						inOnly.setEndpoint(ep);
						inOnly.setOperation(new QName("JMSOperationSend"));
						NormalizedMessage nm = inOnly.createMessage();
						
						//Create a message 
						String xml = JbiHelper.wrapIntoJBIMessage(new QName(
								"http://j2ee.netbeans.org/wsdl/JMS",
								"JMSOperationSendRequest"),
								new String[] { msg + i });
						nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
						
						inOnly.setInMessage(nm);
						context.getDeliveryChannel().sendSync(inOnly);
					}
					return "SUCCESS";
				}
			});
			
			//Get the message
			Object obj = getConnection().execute(new Command() {
				public Serializable execute(ComponentContext context) throws Exception {
					int i = 0;
					InOnly[] exs = new InOnly[5];
					for(; i<5; ++ i){
						Thread.sleep(500);
						exs[i] = null;
						InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
						if(ex == null)
							break;
						exs[i] = ex; 
					}
					for(i=0; i<5; ++ i){
						if(exs[i] == null)
							break;
						exs[i].setStatus(ExchangeStatus.DONE);
						context.getDeliveryChannel().send(exs[i]);
					}
					if(i > 1)
						return "SUCCESS";
					return "FAIL";
				}
			});

			//Compare test result
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

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-SU-Throttling-Test", "JMS-BC-SU-Throttling-Test");
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		//Now add connection
		SAAssembler.Throttling throttling = new SAAssembler.Throttling(5);
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, DELIVERY_SERVICE_NAME, DELIVERY_EP, null, throttling);
		
		return sa.assemble(Configuration.getWorkingDir());
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

}
