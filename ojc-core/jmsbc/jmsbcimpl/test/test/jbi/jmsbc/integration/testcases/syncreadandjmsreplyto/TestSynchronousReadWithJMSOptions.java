package test.jbi.jmsbc.integration.testcases.syncreadandjmsreplyto;

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

public class TestSynchronousReadWithJMSOptions extends IntegrationTestCase {

	private static final int WAIT_TIME = 500;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestSynchronousRead";
	private static final String SA_NAME = "JMS-BC-SA-TestSynchronousReadWithJMSOptions";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort";

	// set operation name
	private static final QName OPERATOR_SEND = new QName("JMSOperationOut");
	private static final QName OPERATOR_READ = new QName("JMSOperationRead");
	private static final QName MESSAGE_REQUEST = new QName(JMS_TNS,
			"JMSOperationOutRequest");
	private static final QName MESSAGE_RESPONSE = new QName(JMS_TNS,
			"JMSOperationResponse");
	private static final QName MESSAGE_READ_REQUEST = new QName(JMS_TNS,
			"JMSOperationRequest");
	
	public void test_IntegrationTestSynchReadWithInputMessageNotSet() throws Throwable {
		final String MSG = "test_IntegrationTestSynchReadWithInputMessageNotSet";
		ComponentContext context = JbiHelper.getComponentContext();
		// send a message
		NormalizedMessage nm;
		ServiceEndpoint ep = sendMessage(MSG, context);

		boolean pass = false;
		// Now do synchronous read
		Thread.sleep(WAIT_TIME * 10);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory()
				.createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		nm = inOut.createMessage();
		inOut.setInMessage(nm);
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() == ExchangeStatus.ERROR) {
			pass = false;
		} else {
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if (results != null && results.length > 0) {
				assertEquals(MSG, ((Text) results[0]).getTextContent());
				pass = true;
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		assertTrue(pass);
	}

	public void test_IntegrationTestSynchReadWithBlankInputMessage() throws Throwable {
		final String MSG = "test_IntegrationTestSynchReadWithBlankInputMessage";
		ComponentContext context = JbiHelper.getComponentContext();
		// send a message
		NormalizedMessage nm;
		ServiceEndpoint ep = sendMessage(MSG, context);

		boolean pass = false;
		// Now do synchronous read
		Thread.sleep(WAIT_TIME * 10);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory()
				.createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		nm = inOut.createMessage();
		String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE_READ_REQUEST, new String[] {});
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOut.setInMessage(nm);
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() == ExchangeStatus.ERROR) {
			pass = false;
		} else {
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if (results != null && results.length > 0) {
				assertEquals(MSG, ((Text) results[0]).getTextContent());
				pass = true;
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		assertTrue(pass);
	}
	
	private ServiceEndpoint sendMessage(final String MSG,
			ComponentContext context) throws MessagingException, SAXException,
			IOException {
		boolean pass = true;
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
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		return ep;
	}

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-sync-read-2.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-sync-read-2.xml"));

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
