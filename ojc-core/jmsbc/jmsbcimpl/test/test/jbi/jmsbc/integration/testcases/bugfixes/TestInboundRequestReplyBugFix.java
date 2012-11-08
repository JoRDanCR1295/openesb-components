package test.jbi.jmsbc.integration.testcases.bugfixes;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.NMPropertiesUtil;

public class TestInboundRequestReplyBugFix extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 5000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestInboundRequestReplyBugFix";
	private static final String SA_NAME = "JMS-BC-SA-TestInboundRequestReplyBugFix";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort";
	private static final String CONSUMMER_EP = "JMSPort2";

	// set operation name
	private static final QName OPERATOR_SEND = new QName("JMSOperation");
	private static final QName OPERATOR_READ = new QName("JMSRead");
	private static final QName MESSAGE_1 = new QName(JMS_TNS,
			"JMSOperationRequest");
	private static final QName MESSAGE_2 = new QName(JMS_TNS,
			"JMSOperationReply");

	public void test_Integration_TestSuccessSenarioWhileSendingReply() throws Throwable {
		ComponentContext context = JbiHelper.getComponentContext();
		try{
			final String MSG = "test_Integration_TransactionNotEnabledWhileSendingReply";
			final String REPLY_TO_QUEUE = "ReplyToQueue";
			boolean pass = true;
			//Activate consumer Endpoint
			JbiHelper.activateEndpoint(context, JMS_SERVICE, CONSUMMER_EP);
			//drain all message from the destination
			for(;;){
				Thread.sleep(WAIT_TIME);
				InOut inOut = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, CONSUMMER_EP);
				if(inOut != null){
					NormalizedMessage nm = inOut.createMessage();
					String xml = JbiHelper
							.wrapIntoJBIMessage(MESSAGE_2, new String[] {"dummy"});
					nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
					nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLY_TO_QUEUE);
					nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, QUEUE);
					inOut.setOutMessage(nm);
					context.getDeliveryChannel().sendSync(inOut);
					if(inOut.getStatus() != ExchangeStatus.DONE){
						pass = false;
						break;
					}
				}else{
					break;
				}
			}
			assertTrue(pass);
			drainMessages(REPLY_TO_QUEUE, QUEUE, context);

			//Now send a message
			pass = true;
			InOnly inOnly = createInOnlyMessageExchange(MSG, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLY_TO_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, QUEUE);
			context.getDeliveryChannel().sendSync(inOnly);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);

			//Wait and then receive message
			Thread.sleep(WAIT_TIME);
			pass = true;
			InOut inOut = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, CONSUMMER_EP);
			if(inOut != null){
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getInMessage().getContent());
				if (results != null) {
					assertEquals(MSG, ((Text) results[0]).getTextContent());
				}
				NormalizedMessage nm = inOut.createMessage();
				String xml = JbiHelper
						.wrapIntoJBIMessage(MESSAGE_2, new String[] { MSG });
				nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
				inOut.setOutMessage(nm);
				context.getDeliveryChannel().sendSync(inOut);
				if(inOut.getStatus() != ExchangeStatus.DONE){
					pass = false;
				}
			}else{
				pass = false;
			}
			assertTrue(pass);
			
			//Wait and then receive message
			Thread.sleep(WAIT_TIME);
			pass = true;
			inOut = getMessage(REPLY_TO_QUEUE, QUEUE, context);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if (results != null) {
					assertEquals(MSG, ((Text) results[0]).getTextContent());
				}
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
			}
			assertTrue(pass);
		}finally{
			JbiHelper.deactivateEndpoint(context, JMS_SERVICE, CONSUMMER_EP);
		}
	}

	public void test_Integration_IfBatchProcessFailsMessageIsStillSendtoReplyQueue() throws Throwable {
		final String MSG_1 = "test_Integration_IfBatchProcessFailsMessageIsStillSendtoReplyQueue_1";
		final String MSG_2 = "test_Integration_IfBatchProcessFailsMessageIsStillSendtoReplyQueue_2";
		final String REPLY_TO_QUEUE = "ReplyToQueue2";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		try{
			//Activate consumer Endpoint
			JbiHelper.activateEndpoint(context, JMS_SERVICE, CONSUMMER_EP);
			//drain all message from the destination
			for(;;){
				Thread.sleep(WAIT_TIME);
				InOut inOut = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, CONSUMMER_EP);
				if(inOut != null){
					NormalizedMessage nm = inOut.createMessage();
					String xml = JbiHelper
							.wrapIntoJBIMessage(MESSAGE_2, new String[] {"dummy"});
					nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
					nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLY_TO_QUEUE);
					nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, QUEUE);
					inOut.setOutMessage(nm);
					context.getDeliveryChannel().sendSync(inOut);
					if(inOut.getStatus() != ExchangeStatus.DONE){
						pass = false;
						break;
					}
				}else{
					break;
				}
			}
			assertTrue(pass);
			drainMessages(REPLY_TO_QUEUE, QUEUE, context);

			//Now send 2 messages
			pass = true;
			InOnly inOnly = createInOnlyMessageExchange(MSG_1, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLY_TO_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, QUEUE);
			context.getDeliveryChannel().sendSync(inOnly);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
			inOnly = createInOnlyMessageExchange(MSG_2, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLY_TO_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, QUEUE);
			context.getDeliveryChannel().sendSync(inOnly);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);

			//Wait and then receive message
			Thread.sleep(WAIT_TIME);
			String msgRead = null;
			pass = true;
			InOut inOut1 = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, CONSUMMER_EP);
			InOut inOut2 = (InOut)JbiHelper.getNextMessage(JMS_SERVICE, CONSUMMER_EP);
			//Process first message
			if(inOut1 != null){
				Object[] results = JbiHelper.unwrapFromJBISource(inOut1
						.getInMessage().getContent());
				
				msgRead = ((Text) results[0]).getTextContent();
				NormalizedMessage nm = inOut1.createMessage();
				String xml = JbiHelper
						.wrapIntoJBIMessage(MESSAGE_2, new String[] { msgRead });
				nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
				inOut1.setOutMessage(nm);
				context.getDeliveryChannel().sendSync(inOut1);
				if(inOut1.getStatus() != ExchangeStatus.DONE){
					pass = false;
				}
			}else{
				pass = false;
			}
			assertTrue(pass);
			//Process second message
			pass = true;
			if(inOut2 != null){
				Object[] results = JbiHelper.unwrapFromJBISource(inOut2
						.getInMessage().getContent());
				//Send errot now this should rollback the transaction
				inOut2.setStatus(ExchangeStatus.ERROR);
				context.getDeliveryChannel().send(inOut2);
			}else{
				pass = false;
			}
			assertTrue(pass);
			
			//Wait and then receive message
			Thread.sleep(WAIT_TIME);
			pass = true;
			InOut inOut3 = getMessage(REPLY_TO_QUEUE, QUEUE, context);
			Object[] results = JbiHelper.unwrapFromJBISource(inOut3
					.getOutMessage().getContent());
			if (results.length == 0) {
				pass = false;
			} else {
				assertEquals(msgRead, ((Text) results[0]).getTextContent());
				inOut3.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut3);
			}
			assertFalse(pass);
		}finally{
			JbiHelper.deactivateEndpoint(context, JMS_SERVICE, CONSUMMER_EP);
		}
	}
	
	private void drainMessages(String dest, String destType, ComponentContext context) throws Exception{
		for(;;){
			Thread.sleep(WAIT_TIME);
			InOut inOut = getMessage(dest, destType, context);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				break;
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if(results == null || results.length == 0)
				break;

		}
	}

	private InOut getMessage(String dest, String destType,
			ComponentContext context) throws MessagingException, SAXException,
			IOException {
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, dest);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, destType);
		context.getDeliveryChannel().sendSync(inOut);
		return inOut;
	}

	private InOnly createInOnlyMessageExchange(final String MSG, ComponentContext context)
			throws MessagingException, SAXException, IOException {
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOnly inOnly = context.getDeliveryChannel().createExchangeFactory()
				.createInOnlyExchange();
		inOnly.setEndpoint(ep);
		inOnly.setOperation(OPERATOR_SEND);
		NormalizedMessage nm = inOnly.createMessage();
		String xml = JbiHelper
				.wrapIntoJBIMessage(MESSAGE_1, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOnly.setInMessage(nm);
		return inOnly;
	}
	
	private InOut createInOnlyMessageExchangeForRead(ComponentContext context) throws MessagingException, SAXException,
			IOException {
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
		su.addWsdl(Configuration.getPath(getClass(), "JMS-2.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

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
		String zipTestSU = createTestSU();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}

	@Override
	protected void tearDown() throws Exception {
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		super.tearDown();
	}

}
