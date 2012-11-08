package test.jbi.jmsbc.integration.testcases.bugfixes;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.jmsbc.integration.testcases.nmproperties.DrainDestination;

import com.sun.jbi.ui.common.JBIRemoteException;

public class TestAnyType extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 5000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/BpelModuleFileInOnly/JMSSyncRead";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestAnyType";
	private static final String SA_NAME = "JMS-BC-SA-TestAnyType";

	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSSolicitedReadService");
	private static final String PROVIDER_EP = "JMSSyncRead_SolicitedReadPort";
	
	// set operation name
	private static final QName OPERATOR_READ = new QName("read");
	private static final QName MESSAGE = new QName(JMS_TNS,
			"SolicitedReadOutputMessage");

	private static final String DESTINATION_QUEUE = "ReadAnyData";

	public void test_Integration_TestAnyType_TextData() throws Throwable {
		ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(context);
		
		final String MSG = "test_Integration_TestAnyType_TextData";
		sendMessage(MSG, context);


		Thread.sleep(WAIT_TIME);
		boolean pass = false;
		Object result = readMessage(context);

		if(result != null && (result instanceof Text)){
			pass = MSG.equals(((Text)result).getTextContent());
		}
		assertTrue(pass);
	}

	public void test_Integration_TestAnyType_XMLData() throws Throwable {
		ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(context);
		
		final String MSG = "<xmlContent>TestData</xmlContent>";
		sendMessage(MSG, context);


		Thread.sleep(WAIT_TIME);
		boolean pass = false;
		Object result = readMessage(context);

		if(result != null && result instanceof Element){
			Element e = (Element)result;
			if(e.getLocalName().equals("xmlContent")){
				Node c = e.getFirstChild();
				if(c instanceof Text){
					pass = "TestData".equals(((Text)c).getTextContent());
				}
			}
		}
		assertTrue(pass);
	}

	private Object readMessage(ComponentContext context)
			throws MessagingException, SAXException, IOException, Exception {
		Object result = null;
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		NormalizedMessage nm = inOut.createMessage();
		String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE, new String[] { "" });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOut.setInMessage(nm);
		context.getDeliveryChannel().sendSync(inOut);
		
		if (inOut.getStatus() != ExchangeStatus.ERROR) {
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if (results != null && results.length > 0) {
				result = results[0];
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		return result;
	}

	
	private void drainMessages(ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), DESTINATION_QUEUE, QUEUE, context);
		drain.drainDestiantion();
	}

	private void sendMessage(final String MSG, final ComponentContext context)
			throws JBIRemoteException, Exception {
		SendMessage sendMessage = new SendMessage(getInstaller(),
				DESTINATION_QUEUE, QUEUE, context);
		sendMessage.sendMessage(MSG);
	}

	
	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-Test-AnyType.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-Test-AnyType.xml"));

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
