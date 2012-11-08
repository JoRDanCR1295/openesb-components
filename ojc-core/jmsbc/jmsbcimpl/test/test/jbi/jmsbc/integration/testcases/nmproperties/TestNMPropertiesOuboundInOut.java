package test.jbi.jmsbc.integration.testcases.nmproperties;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.NMPropertiesUtil;
import com.sun.jbi.jmsbc.extensions.JMSConstants;

public class TestNMPropertiesOuboundInOut extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 5000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestNMPropertiesOuboundInOut";
	private static final String SA_NAME = "JMS-BC-SA-TestNMPropertiesOuboundInOut";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort1";

	// set operation name
	private static final QName OPERATOR_SEND = new QName("JMSSend");
	private static final QName OPERATOR_READ = new QName("JMSRead");
	private static final QName OPERATOR_REQUESTREPLY = new QName("JMSRequestReply");
	private static final QName MESSAGE_REQUEST = new QName(JMS_TNS,
			"JMSOperationRequest");
	
	public void test_Integration_TestInboundInOutSetDetination() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOutSetDetination";
		final String MSG_REPLY = "test_Integration_TestInboundInOutSetDetination_Reply";
		final String DESTINATION_QUEUE = "TestOutboundInOutSetDestination";
		final String REPLYTO_QUEUE = "TestOutboundInOutSetDestination_ReplyTo";
		final String CORRID_REQUEST = "CorrId_Request";
		final String CORRID_REPLY = "CorrId_Reply";
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		drainMessages(REPLYTO_QUEUE, JMSConstants.QUEUE, context);
		
		try{
			JbiHelper.registerEndPoint(JMS_SERVICE, PROVIDER_EP);
			boolean pass = true;
			{
				//Wait for all the messages to be drained first
				Thread.sleep(WAIT_TIME);
				InOut inOut = createInOutMessageExchange(MSG, context);
				inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
				inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, JMSConstants.QUEUE);
				inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLYTO_QUEUE);
				inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, JMSConstants.QUEUE);
				inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REQUEST);
				inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
				inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2");
				context.getDeliveryChannel().send(inOut);
			}
			
			{
				Thread.sleep(WAIT_TIME);
				//Now read the message
				InOut readInOut = getMessage(DESTINATION_QUEUE, context, null);
				if (readInOut.getStatus() == ExchangeStatus.ERROR) {
					pass = false;
				} else {
					Object[] results = JbiHelper.unwrapFromJBISource(readInOut
							.getOutMessage().getContent());
					if (results != null) {
						assertEquals(MSG, ((Text) results[0]).getTextContent());
					}
					readInOut.setStatus(ExchangeStatus.DONE);
					context.getDeliveryChannel().send(readInOut);
					//Get the properties from exchange
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE);
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_REPLYTODESTINATION), REPLYTO_QUEUE);
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE), QUEUE);
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REQUEST);
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1");
					assertEquals(readInOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2");
					
				}
				assertTrue(pass);
			}
			
			{
				//Send a message 
				InOnly sendEx = createInOnlyMessageForSend(MSG_REPLY, context);
				sendEx.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, REPLYTO_QUEUE);
				sendEx.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
				sendEx.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REPLY);
				sendEx.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1-reply");
				sendEx.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2-reply");
				context.getDeliveryChannel().sendSync(sendEx);
				if (sendEx.getStatus() != ExchangeStatus.DONE) {
					pass = false;
				}
				assertTrue(pass);
			}

			{
				//Wait for reply
				Thread.sleep(WAIT_TIME);
				InOut inOut = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, PROVIDER_EP);
				if (inOut.getStatus() == ExchangeStatus.ERROR) {
					pass = false;
				} else {
					Object[] results = JbiHelper.unwrapFromJBISource(inOut
							.getOutMessage().getContent());
					if (results != null) {
						assertEquals(MSG_REPLY, ((Text) results[0]).getTextContent());
					}
					inOut.setStatus(ExchangeStatus.DONE);
					context.getDeliveryChannel().send(inOut);
					//Get the properties from exchange
					assertEquals(inOut.getOutMessage().getProperty(
							NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE);
					assertEquals(inOut.getOutMessage().getProperty(
							NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
					assertEquals(inOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REPLY);
					assertEquals(inOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1-reply");
					assertEquals(inOut.getOutMessage().getProperty(
							NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2-reply");
				}
				assertTrue(pass);
			}
			
		}finally{
		}
	}
	
	private InOnly createInOnlyMessageForSend(final String MSG,
			ComponentContext context) throws MessagingException, SAXException,
			IOException {
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOnly inOnly = context.getDeliveryChannel().createExchangeFactory()
				.createInOnlyExchange();
		inOnly.setEndpoint(ep);
		inOnly.setOperation(OPERATOR_SEND);
		NormalizedMessage nm = inOnly.createMessage();
		String xml = JbiHelper
				.wrapIntoJBIMessage(MESSAGE_REQUEST, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOnly.setInMessage(nm);
		return inOnly;
	}
	
	private void drainMessages(String dest, String destType, ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), dest, destType, context);
		drain.drainDestiantion();
	}

	private InOut getMessage(String dest,
			ComponentContext context, Transaction tx) throws MessagingException, SAXException,
			IOException {
		InOut inOut = createInOutMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, dest);
		//Set transaction
		if(tx != null)
			inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
		
		context.getDeliveryChannel().sendSync(inOut);
		return inOut;
	}

	private InOut createInOutMessageExchange(final String MSG, ComponentContext context)
			throws MessagingException, SAXException, IOException {
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory()
				.createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_REQUESTREPLY);
		NormalizedMessage nm = inOut.createMessage();
		String xml = JbiHelper
				.wrapIntoJBIMessage(MESSAGE_REQUEST, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOut.setInMessage(nm);
		return inOut;
	}
	
	private InOut createInOutMessageExchangeForRead(ComponentContext context)
			throws MessagingException, SAXException, IOException {
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory()
				.createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		NormalizedMessage nm = inOut.createMessage();
		inOut.setInMessage(nm);
		return inOut;
	}
	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-OutboundInOut.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-OutboundInOut.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		deploySA();
	}

	@Override
	protected void tearDown() throws Exception {
		unDeploy();
		// Make sure that SA is undeployed
		super.tearDown();
	}
	
	private void deploySA() throws Exception{
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		String zipTestSU = createTestSU();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}
	
	private void unDeploy(){
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
	}

}
