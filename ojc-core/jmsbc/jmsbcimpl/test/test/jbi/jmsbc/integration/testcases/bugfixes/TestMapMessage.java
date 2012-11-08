package test.jbi.jmsbc.integration.testcases.bugfixes;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

public class TestMapMessage extends IntegrationTestCase {

	private static final int WAIT_TIME = 500;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMSMap";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestMapMessage";
	private static final String SA_NAME = "JMS-BC-SA-TestMapMessage";
	private static final QName JMS_SERVICE = new QName(JMS_TNS,"JMSMapService");
	private static final String CONSUMER_EP = "JMSMapPort";

	private static final QName DELIVERY_SERVICE_NAME = new QName("urn:test-bc-tns","TestRecvService");
	private static final String DELIVERY_EP = "TestSecvPort";

	private static final QName PRODUCER_SERVICE_NAME = new QName("urn:test-bc-tns","TestSendService");
	private static final String PRODUCER_EP = "TestSendPort";
	//set operation name
	private static final QName OPERATOR = new QName("JMSMapOperation");
	private static final QName MESSAGE = new QName(
			JMS_TNS, "JMSMapOperationRequest");

	public void test_IntegrationTestRedlivery() throws Throwable{
		final String NAME = "name";
		final String VALUE = "value";
		final double DECIMAL = 2.23;
		final 
		ComponentContext context = JbiHelper.getComponentContext();
		try{
			//Activate delivery end points
			JbiHelper.activateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
			//Drain all the messages from the queue before testing
			for(;;){
				Thread.sleep(WAIT_TIME * 10);
				InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
				if(ex == null)
					break;
				ex.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(ex);
			}
			
			//send a message to test that map message is working fine.
			boolean pass = true;
			ServiceEndpoint ep = context.getEndpoint(PRODUCER_SERVICE_NAME, PRODUCER_EP);
			InOnly inOnly = context.getDeliveryChannel()
					.createExchangeFactory().createInOnlyExchange();
			inOnly.setEndpoint(ep);
			inOnly.setOperation(OPERATOR);
			NormalizedMessage nm = inOnly.createMessage();
			String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE,
					new String[] { NAME, VALUE, DECIMAL + "" });
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
			inOnly.setInMessage(nm);
			context.getDeliveryChannel().sendSync(inOnly);
			if(inOnly.getStatus() != ExchangeStatus.DONE){
				pass = false;
			}
			assertTrue(pass);

			pass = false;
			//Now get the message and check if the message received is correct
			Thread.sleep(WAIT_TIME * 10);
			InOnly ex = (InOnly)JbiHelper.getNextMessage(DELIVERY_SERVICE_NAME, DELIVERY_EP);
			if(ex != null){
				ex.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(ex);
				Object[] results = JbiHelper.unwrapFromJBISource(ex.getInMessage()
						.getContent());
				if(results != null){
					assertEquals(NAME, ((Text)results[0]).getTextContent());
					assertEquals(VALUE, ((Text)results[1]).getTextContent());
					String decimalStr = ((Text)results[2]).getTextContent();
					assertEquals(DECIMAL, Double.valueOf(decimalStr));
					pass = true;
				}
			}
			assertTrue(pass);
			
			
		}finally{
			try{
				JbiHelper.deactivateEndpoint(context, DELIVERY_SERVICE_NAME, DELIVERY_EP);
			}catch(Throwable t){}
			
		}
		
	}

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-mapmessage.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		//Now add connection
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, DELIVERY_SERVICE_NAME, DELIVERY_EP);
		sa.addConnection(PRODUCER_SERVICE_NAME, PRODUCER_EP, JMS_SERVICE, CONSUMER_EP);
		
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
		getInstaller().startServiceAssembly(SA_NAME);
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
