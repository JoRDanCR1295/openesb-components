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

import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.NMPropertiesUtil;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import com.sun.jbi.ui.common.JBIRemoteException;

public class TestNMPropertiesSyncRead extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 10000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestNMPropertiesFailureTestCases";
	private static final String SA_NAME = "JMS-BC-SA-TestNMPropertiesFailureTestCases";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort";

	// set operation name
	private static final QName OPERATOR_SEND = new QName("JMSOperation");
	private static final QName OPERATOR_READ = new QName("JMSRead");
	private static final QName MESSAGE = new QName(JMS_TNS,
			"JMSOperationRequest");

	public void test_Integration_TestSyncReadSetForwardAsAttachment() throws Throwable {
		final String MSG_1 = "test_Integration_TestSyncReadSetForwardAsAttachment_1";
		final String MSG_2 = "test_Integration_TestSyncReadSetForwardAsAttachment_2";
		final String DESTINATION_QUEUE = "TestNMPropertiesSyncReadQueueForwardAsAttachment";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, QUEUE, context);

		{
			InOnly inOnly = createInOnlyMessageExchange(MSG_1, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}

		pass = true;
		Thread.sleep(WAIT_TIME);
		{
			//Read first message with JMSType = type1
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					assertEquals(MSG_1, ((Text) results[0]).getTextContent());
				}
			}
			assertTrue(pass);
		}
		
		{
			//Send second message
			InOnly inOnly = createInOnlyMessageExchange(MSG_2, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}

		pass = true;
		Thread.sleep(WAIT_TIME);
		{
			//Read first message with JMSType = type1
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_FORWARDASATTACHMENT, "true");

			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				pass = WrapperUtil.isNodeXopInclude((Node)results[0]);
				if(pass){
					String cid = WrapperUtil.getXopContentId((Node)results[0]);
					pass = cid != null && cid.trim().length()!=0;
					if(pass){
						byte[] bytes = JbiHelper.getAttachment(inOut.getOutMessage(), cid);
						pass = bytes != null & bytes.length != 0;
						if(pass){
							assertEquals(MSG_2,  new String(bytes));
						}
					}
				}
			}
			assertTrue(pass);
		}
	}

	public void test_Integration_TestSyncReadSetMessageSelector() throws Throwable {
		final String MSG_1 = "test_Integration_TestSyncRead_1";
		final String MSG_2 = "test_Integration_TestSyncRead_2";
		final String DESTINATION_QUEUE = "TestNMPropertiesSyncReadQueue";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, QUEUE, context);

		{
			//send first message
			InOnly inOnly = createInOnlyMessageExchange(MSG_1, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_MESSAGETYPE, "type1");
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}

		{
			//send second message
			InOnly inOnly = createInOnlyMessageExchange(MSG_2, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_MESSAGETYPE, "type2");
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}
		
		pass = true;
		Thread.sleep(WAIT_TIME);
		{
			//Read first message with JMSType = type1
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_MESSAGESELECTOR, "JMSType = 'type1'");
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					assertEquals(MSG_1, ((Text) results[0]).getTextContent());
				}
			}
			assertTrue(pass);
		}
		
		pass = true;
		Thread.sleep(WAIT_TIME);
		{
			//Read second message with JMSType = type1. This should fail
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_MESSAGESELECTOR, "JMSType = 'type1'");
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				assertTrue(false);
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results.length == 0) {
					pass = false;
				}
			}
			assertFalse(pass);
		}
		
		pass = true;
		Thread.sleep(WAIT_TIME * 5);
		{
			//Try reading message with JMSType = type2. This should pass
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_MESSAGESELECTOR, "JMSType = 'type2'");
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					assertEquals(MSG_2, ((Text) results[0]).getTextContent());
				}
			}
			assertTrue(pass);
		}
	}

	public void test_Integration_TestSyncReadSetDurableSubscriber() throws Throwable {
		final String MSG_1 = "test_Integration_TestSyncReadSetDurableSubscriber_1";
		final String MSG_2 = "test_Integration_TestSyncReadSetDurableSubscriber_2";
		final String DESTINATION_TOPIC = "TestNMPropertiesSyncReadTopic";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		{
			//Drain all the messages and register durable subscriber for this topic
			for(;;){
				Thread.sleep(WAIT_TIME);
				InOut inOut = createInOnlyMessageExchangeForRead(context);
				setTopicSubscriptionProperty(DESTINATION_TOPIC, inOut);
				context.getDeliveryChannel().sendSync(inOut);
				if (inOut.getStatus() == ExchangeStatus.ERROR)
					break;
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if(results.length == 0)
					break;
			}
		}

		{
			//send first message
			InOnly inOnly = createInOnlyMessageExchange(MSG_1, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_TOPIC);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, JMSConstants.TOPIC);
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}

		pass = true;
		Thread.sleep(WAIT_TIME);
		{
			//read first message
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			setTopicSubscriptionProperty(DESTINATION_TOPIC, inOut);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					assertEquals(MSG_1, ((Text) results[0]).getTextContent());
				}
			}
			assertTrue(pass);
		}
		
		unDeploy();
		deploy();

		{
			//send second message
			InOnly inOnly = createInOnlyMessageExchange(MSG_2, context);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_TOPIC);
			inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, JMSConstants.TOPIC);
			sendMessage(context, inOnly, false);
			if (inOnly.getStatus() != ExchangeStatus.DONE) {
				pass = false;
			}
			assertTrue(pass);
		}

		Thread.sleep(WAIT_TIME);
		pass = true;
		{
			//read second message, This should pass
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			setTopicSubscriptionProperty(DESTINATION_TOPIC, inOut);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					assertEquals(MSG_2, ((Text) results[0]).getTextContent());
				}
			}
			assertTrue(pass);
		}
		
	}

	private void setTopicSubscriptionProperty(final String DESTINATION_TOPIC,
			InOut inOut) {
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_TOPIC);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, JMSConstants.TOPIC);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_SUBSCRIPTIONDURABILITY, JMSConstants.DURABLE);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_CLIENTID, "TestClient1");
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_SUBSCRIPTIONNAME, "TestSub");
	}
	
	private void drainMessages(String dest, String destType, ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), dest, destType, context);
		drain.drainDestiantion();
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
				.wrapIntoJBIMessage(MESSAGE, new String[] { MSG });
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
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}

	private Transaction sendMessage(ComponentContext context, InOnly inOnly, boolean inTransaction)
			throws Exception, MessagingException {
		Transaction tx = null;
		if(inTransaction){
			tx = JbiHelper.startTransaction(context);
			inOnly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
			JbiHelper.suspendThreadTx(context);
		}
		context.getDeliveryChannel().sendSync(inOnly);
		if(tx != null)
			JbiHelper.resumeTransaction(context, tx);
		return tx;
	}
	
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		unDeploy();
		deploy();
	}

	private void deploy() throws IOException, Exception, JBIRemoteException {
		String zipTestSU = createTestSU();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}

	@Override
	protected void tearDown() throws Exception {
		// Make sure that SA is undeployed
		unDeploy();
		// TODO Auto-generated method stub
		super.tearDown();
	}

	private void unDeploy() {
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
	}

}
