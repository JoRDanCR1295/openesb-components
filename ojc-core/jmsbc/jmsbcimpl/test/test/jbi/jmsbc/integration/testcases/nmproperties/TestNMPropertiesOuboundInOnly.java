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

public class TestNMPropertiesOuboundInOnly extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 5000;
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

	public void test_Integration_TestInboundInOnlySetDifferentConnectionURL() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySetDifferentConnectionURL";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueetDifferentConnectionURL";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		//drain all message from the destination
		for(;;){
			Thread.sleep(WAIT_TIME);
			InOut inOut = createInOnlyMessageExchangeForRead(context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_CONNECTIONURL, "stcms://localhost:18007");
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_USERNAME, "admin");
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_PASSWORD, "adminadmin");
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				break;
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_CONNECTIONURL, "stcms://localhost:18007");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_USERNAME, "admin");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_PASSWORD, "adminadmin");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);

		//Wait for some time
		pass = true;
		Thread.sleep(WAIT_TIME);
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_CONNECTIONURL, "stcms://localhost:18007");
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_USERNAME, "admin");
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_PASSWORD, "adminadmin");
		context.getDeliveryChannel().sendSync(inOut);
		
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
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_CONNECTIONURL), "stcms://localhost:18007");
		}
		assertTrue(pass);
		
		//Try to read message from the configured queue to test that original values are not 
		//overwritten permanently
		pass = true;
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);

		//Wait for some time and the message should be expired by that time.
		Thread.sleep(WAIT_TIME);
		//Now try to read message from the redirected queue
		inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
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
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_CONNECTIONURL), "mq://localhost:7676");
		}
		assertTrue(pass);
	}


	public void test_Integration_TestInboundInOnlySetXATransaction() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySetXATransaction";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueSetXATransaction";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_XATRANSACTION, JMSConstants.TRANSACTION_NONE);
		Transaction tx = sendMessage(context, inOnly);
		tx.commit();
		tx = null;
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);

		//Wait for some time
		pass = true;
		Thread.sleep(WAIT_TIME);
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, tx);
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
		
		
		//Now send an message and rollback the transaction it should still receive the message
		inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_XATRANSACTION, JMSConstants.TRANSACTION_NONE);
		tx = sendMessage(context, inOnly);
		tx.rollback();
		tx = null;
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		
		//Wait for some time
		pass = true;
		Thread.sleep(WAIT_TIME);
		inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, tx);
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
		
	}

	public void test_Integration_TestInboundInOnlySetReplyToDestination() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySetReplyToDestination";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueSetReplyToDest";
		final String REPLYTOTOPIC = "ReplyToTopic";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, REPLYTOTOPIC);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, JMSConstants.TOPIC);

		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);

		//Wait for some time and the message should be expired by that time.
		Thread.sleep(WAIT_TIME * 3);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
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
			//Get the properties from exchange
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_REPLYTODESTINATION), REPLYTOTOPIC);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE), JMSConstants.TOPIC);
		}
		assertTrue(pass);
	}

	
	public void test_Integration_TestInboundInOnly() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnly";
		final String REDIRECT_DESTINATION_QUEUE = "RedirectDestinationQueue";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(REDIRECT_DESTINATION_QUEUE, QUEUE, context);
		
		//Now send a message
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, REDIRECT_DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		
		Thread.sleep(WAIT_TIME);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(REDIRECT_DESTINATION_QUEUE, QUEUE, context, null);
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
			//Get the properties from exchange
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATION), REDIRECT_DESTINATION_QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
		}
		assertTrue(pass);
	}
	
	public void test_Integration_TestInboundInOnlySettingUserProeprtiesOnMessage() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySettingUserProeprtiesOnMessage";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueWithUserProperty";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		//Set user properties
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name3", "value3");
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		
		Thread.sleep(WAIT_TIME);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
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
			//Get the properties from exchange
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "name3"), "value3");
			
		}
		assertTrue(pass);
	}

	public void test_Integration_TestInboundInOnlySettingUserProeprtiesWithTypesOnMessage() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySettingUserProeprtiesOnMessage";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueWithUserProperty";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		//Set user properties
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "BooleanValue", "true");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "BooleanValue" + ".type", "boolean");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "ShortValue", "2");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "ShortValue" + ".type", "Short");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "IntValue", "1234");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "IntValue" + ".type", "int");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "LongValue", "1234567");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "LongValue" + ".type", "long");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "FloatValue", "2.34");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "FloatValue" + ".type", "float");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "DoubleValue", "23.1");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "DoubleValue" + ".type", "double");
		
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "StringValue", "TestString");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "StringValue" + ".type", "String");
		
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		
		Thread.sleep(WAIT_TIME);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
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
			//Get the properties from exchange
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
			
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "BooleanValue"), "true");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "BooleanValue" + ".type"), "boolean");
			
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "ShortValue"), "2");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "ShortValue" + ".type"), "short");

			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "IntValue"), "1234");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "IntValue" + ".type"), "int");

			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "LongValue"), "1234567");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "LongValue" + ".type"), "long");

			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "FloatValue"), "2.34");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "FloatValue" + ".type"), "float");

			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "DoubleValue"), "23.1");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "DoubleValue" + ".type"), "double");

			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "StringValue"), "TestString");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_USERPROPERTIES + "StringValue" + ".type"), "string");
			
		}
		assertTrue(pass);
	}
	
	public void test_Integration_TestInboundInOnlySettingMessageTypeAndCorrelationId() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySettingMessageTypeAndCorrelationId";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueSetMessageTypeAndCorrelationId";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_MESSAGETYPE, "TestMessage");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, "CorrelationId");
		//Set user properties
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		
		Thread.sleep(WAIT_TIME);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
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
			//Get the properties from exchange
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE);
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_MESSAGETYPE), "TestMessage");
			assertEquals(inOut.getOutMessage().getProperty(
					NMPropertiesUtil.INOUT_CORRELATIONID), "CorrelationId");
			
		}
		assertTrue(pass);
	}
	public void test_Integration_TestInboundInOnlySettingTimetolive() throws Throwable {
		final String MSG = "test_Integration_TestInboundInOnlySettingTimetolive";
		final String DESTINATION_QUEUE = "RedirectDestinationQueueSetTimetolive";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		
		//drain all message from the destination
		drainMessages(DESTINATION_QUEUE, QUEUE, context);
		
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		//Set Destination
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, DESTINATION_QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, QUEUE);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_TIMETOLIVE, "" + WAIT_TIME);
		//Set user properties
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);

		//Wait for some time and the message should be expired by that time.
		Thread.sleep(5* WAIT_TIME);
		//Now try to read message from the redirected queue
		InOut inOut = getMessage(DESTINATION_QUEUE, QUEUE, context, null);
		Object[] results = JbiHelper.unwrapFromJBISource(inOut
				.getOutMessage().getContent());
		inOut.setStatus(ExchangeStatus.DONE);
		context.getDeliveryChannel().send(inOut);
		if (results.length == 0) {
			pass = false;
		}
		assertFalse(pass);
	}
	
	private void drainMessages(String dest, String destType, ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), dest, destType, context);
		drain.drainDestiantion();
	}

	private InOut getMessage(String dest, String destType,
			ComponentContext context, Transaction tx) throws MessagingException, SAXException,
			IOException {
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, dest);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, destType);
		//Set transaction
		if(tx != null)
			inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
		
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

	private Transaction sendMessage(ComponentContext context, InOnly inOnly)
			throws Exception, MessagingException {
		Transaction tx = JbiHelper.startTransaction(context);
		inOnly.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
		JbiHelper.suspendThreadTx(context);
		context.getDeliveryChannel().sendSync(inOnly);
		JbiHelper.resumeTransaction(context, tx);
		return tx;
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
		// TODO Auto-generated method stub
		super.tearDown();
	}

}
