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
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.sun.jbi.jmsbc.NMPropertiesUtil;
import com.sun.jbi.jmsbc.NMPropertiesUtil.NMPropertiesParsingException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

public class TestNMPropertiesFailureTestCases extends IntegrationTestCase {

	private static final int WAIT_TIME = 500;
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

	public void test_Integration_TestSetUserPropertiesFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetDestenationFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1" + ".type", "XYZ");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0775") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetDestinationFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetDestenationFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, "DestinationQueue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
//			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
//			Exception e = inOnly.getError();
//			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
//			assertTrue(e.getMessage().indexOf("JMSBC-E0766") != -1);
			pass = false;
		}
		assertTrue(pass);
	}

	public void test_Integration_TestSetDestinationTypeFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetDestenationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, "Queue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
//			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
//			Exception e = inOnly.getError();
//			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
//			assertTrue(e.getMessage().indexOf("JMSBC-E0766") != -1);
			pass = false;
		}
		assertTrue(pass);
	}
	
	public void test_Integration_TestSetReplyToDestinationFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetReplyToDestinationFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, "ReplyToDestinationQueue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0767") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetReplyToDestinationTypeFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, "Queue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0767") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetInValidReplyToDestinationTypeFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetInValidReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, "InvalidQueue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}
	
	public void test_Integration_TestSetInValidDestinationTypeFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetInValidReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, "InvalidQueue");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}
	
	public void test_Integration_TestSetInValidTimeToLiveFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetInValidReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_TIMETOLIVE, "InvalidTimetolive");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetInValidPriorityFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetInValidReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.INOUT_PRIORITY, "InvalidPriority");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetInValidTransactionFailure() throws Throwable {
		final String MSG = "test_Integration_TestSetInValidReplyToDestinationTypeFailure";
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOnly inOnly = createInOnlyMessageExchange(MSG, context);
		inOnly.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_XATRANSACTION, "InvalidTransaction");
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOnly.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOnly.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}
	
	public void test_Integration_TestSetSubscriptionDurabilityFailure() throws Throwable {
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_SUBSCRIPTIONDURABILITY, "Durable");
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOut.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOut.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0768") != -1);
			pass = false;
		}
		assertFalse(pass);
	}
	
	public void test_Integration_TestSetInValidSubscriptionDurabilityFailure() throws Throwable {
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_SUBSCRIPTIONDURABILITY, "InvalidDurable");
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOut.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOut.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
	}

	public void test_Integration_TestSetSubscriptionNameFailure() throws Throwable {
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_SUBSCRIPTIONNAME, "DurableSub");
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOut.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOut.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0768") != -1);
			pass = false;
		}
		assertFalse(pass);
	}
	
//	public void test_Integration_TestSetClientIdFailure() throws Throwable {
//		boolean pass = true;
//		ComponentContext context = JbiHelper.getComponentContext();
//		InOut inOut = createInOnlyMessageExchangeForRead(context);
//		inOut.getInMessage().setProperty(NMPropertiesUtil.CLIENTID, "XYZ");
//		context.getDeliveryChannel().sendSync(inOut);
//		if (inOut.getStatus() != ExchangeStatus.DONE) {
//			assertEquals("Client", (String)inOut.getProperty("com.sun.jbi.crl.faultcode"));
//			Exception e = inOut.getError();
//			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
//			assertTrue(e.getMessage().indexOf("JMSBC-E0768") != -1);
//			pass = false;
//		}
//		assertFalse(pass);
//	}
	
	public void test_Integration_TestSetInvalidTimeout() throws Throwable {
		boolean pass = true;
		ComponentContext context = JbiHelper.getComponentContext();
		InOut inOut = createInOnlyMessageExchangeForRead(context);
		inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_TIMEOUT, "InvalidTimeout");
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() != ExchangeStatus.DONE) {
			assertEquals("Client", (String)inOut.getProperty("com.sun.jbi.crl.faultcode"));
			Exception e = inOut.getError();
			assertEquals(e.getCause().getClass().getName(), NMPropertiesParsingException.class.getName());
			assertTrue(e.getMessage().indexOf("JMSBC-E0765") != -1);
			pass = false;
		}
		assertFalse(pass);
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
