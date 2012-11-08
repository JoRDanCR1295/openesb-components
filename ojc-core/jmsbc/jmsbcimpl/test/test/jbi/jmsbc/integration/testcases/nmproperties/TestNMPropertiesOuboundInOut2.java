package test.jbi.jmsbc.integration.testcases.nmproperties;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.integration.testbc.impl.MessageConsumer;

import com.sun.jbi.jmsbc.NMPropertiesUtil;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

public class TestNMPropertiesOuboundInOut2 extends IntegrationTestCase {

	private static final String QUEUE = "Queue";
	private static final int WAIT_TIME = 5000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestNMPropertiesOuboundInOut-2";
	private static final String SA_NAME = "JMS-BC-SA-TestNMPropertiesOuboundInOut-2";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final QName JMS_SERVICE_CONSUMER = new QName(JMS_TNS, "JMSService2");
	private static final String PROVIDER_EP = "JMSPort1";
	private static final String CONSUMER_EP = "JMSPort2";

	// set operation name
	private static final QName OPERATOR_REQUESTREPLY = new QName("JMSRequestReply");
	private static final QName MESSAGE_REQUEST = new QName(JMS_TNS,
			"JMSOperationRequest");
	private static final QName MESSAGE_REPLY = new QName(JMS_TNS,
	"JMSOperationReply");

	
	private static final String MSG = "test_Integration_TestInboundInOutSetDetination";
	private static final String MSG_REPLY = "test_Integration_TestInboundInOutTestNMPropertiesPropogation_Reply";
	private static final String MSG_FAILED = "test_Integration_TestInboundInOutTestNMPropertiesPropogation_Failed";
	private static final String DESTINATION_QUEUE = "TestNMPropertiesQueue3";
	private static final String DESTINATION_QUEUE_REPLYTO = "TestNMPropertiesQueue3Replyto";
	private static final String CORRID_REQUEST = "CorrId_Request";
	private static final String CORRID_REPLY = "CorrId_Reply";
	
	
	public void test_Integration_TestInboundInOutTestNMPropertiesForwardAsAttachment() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REQUEST);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2");
			inOut.getInMessage().setProperty(NMPropertiesUtil.OUTBOUND_FORWARDASATTACHMENT, "true");
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
				if (results != null) {
					pass = WrapperUtil.isNodeXopInclude((Node)results[0]);
					if(pass){
						String cid = WrapperUtil.getXopContentId((Node)results[0]);
						pass = cid != null && cid.trim().length()!=0;
						if(pass){
							byte[] bytes = JbiHelper.getAttachment(inOut.getOutMessage(), cid);
							pass = bytes != null & bytes.length != 0;
							if(pass){
								assertEquals(MSG_REPLY,  new String(bytes));
							}
						}
					}
				}
				//Get the properties from exchange
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REPLY);
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1-reply");
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2-reply");
			}
			assertTrue(pass);
				
		}finally{
			unDeploy();
		}
	}
	
	public void test_Integration_TestInboundInOutTestNMPropertiesPropogation() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REQUEST);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2");
			context.getDeliveryChannel().sendSync(inOut);
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
						NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REPLY);
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1-reply");
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2-reply");
			}
			assertTrue(pass);
				
		}finally{
			unDeploy();
		}
	}
	
	public void test_Integration_TestInboundInOutTestNMPropertiesPropogationWithReplyToQueue() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		drainMessages(DESTINATION_QUEUE_REPLYTO, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REQUEST);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, DESTINATION_QUEUE_REPLYTO);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, JMSConstants.QUEUE);
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1");
			inOut.getInMessage().setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2");
			context.getDeliveryChannel().sendSync(inOut);
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
						NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REPLY);
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1-reply");
				assertEquals(inOut.getOutMessage().getProperty(
						NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2-reply");
			}
			assertTrue(pass);
				
		}finally{
			unDeploy();
		}
	}

	private static class MessageConsumerImpl implements MessageConsumer{
		private ComponentContext context;
		private boolean sendToReplToQueue = false;
		public MessageConsumerImpl(ComponentContext context, boolean sentToReplToQueue){
			this.context = context;
			this.sendToReplToQueue = sentToReplToQueue;
		}
		public void onMessage(MessageExchange ex) throws Exception {
			if(!(ex instanceof InOut)){
				System.out.println("Test would fail. Invalid message exchange");
			}
			InOut inOut = (InOut)ex;
			if (inOut.getStatus() == ExchangeStatus.ERROR || inOut.getStatus() == ExchangeStatus.DONE) {
				return;
			} 
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getInMessage().getContent());
			boolean pass = true;
			if (results != null && results.length > 0) {
				pass =  compareStr(MSG, ((Text) results[0]).getTextContent()) &&
						compareStr(inOut.getInMessage().getProperty(
								NMPropertiesUtil.INBOUND_DESTINATION), DESTINATION_QUEUE) &&
						compareStr(inOut.getInMessage().getProperty(
								NMPropertiesUtil.INBOUND_DESTINATIONTYPE), QUEUE) &&
						compareStr(inOut.getInMessage().getProperty(
								NMPropertiesUtil.INOUT_CORRELATIONID), CORRID_REQUEST) &&
						compareStr(inOut.getInMessage().getProperty(
								NMPropertiesUtil.INOUT_USERPROPERTIES + "name1"), "value1") &&
						compareStr(inOut.getInMessage().getProperty(
								NMPropertiesUtil.INOUT_USERPROPERTIES + "name2"), "value2");
			}else{
				pass = false;
			}
			
			NormalizedMessage nm = inOut.createMessage();
			inOut.setOutMessage(nm);
			if(pass){
				String xml = JbiHelper
					.wrapIntoJBIMessage(MESSAGE_REPLY, new String[] { MSG_REPLY });
				nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
				nm.setProperty(NMPropertiesUtil.INOUT_CORRELATIONID, CORRID_REPLY);
				nm.setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name1", "value1-reply");
				nm.setProperty(NMPropertiesUtil.INOUT_USERPROPERTIES + "name2", "value2-reply");
			}else{
				String xml = JbiHelper
					.wrapIntoJBIMessage(MESSAGE_REPLY, new String[] { MSG_FAILED });
				nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
			}
			if(sendToReplToQueue){
				nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATION, DESTINATION_QUEUE_REPLYTO);
				nm.setProperty(NMPropertiesUtil.INOUT_REPLYTODESTINATIONTYPE, JMSConstants.QUEUE);
			}
			context.getDeliveryChannel().send(inOut);
		}
	}
	
	private static boolean compareStr(Object str1, String str2){
		if(str1 == null || str2 == null)
			return false;
		str2 = str2.trim();
		
		return str1.equals(str2);
	}
	
	private void drainMessages(String dest, String destType, ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), dest, destType, context);
		drain.drainDestiantion();
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
	
	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-OutboundInOut-2.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-OutboundInOut-2.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
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
