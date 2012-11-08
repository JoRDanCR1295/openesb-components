package test.jbi.jmsbc.integration.testcases.attachments;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.activation.DataHandler;
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
import test.jbi.jmsbc.integration.testcases.nmproperties.DrainDestination;

import com.sun.jbi.common.util.Base64Utils;
import com.sun.jbi.jmsbc.JMSBCDataSource;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

public class TestAttachmentsForBinary extends IntegrationTestCase {

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

	
	private static final String MSG_REPLY = "test_Integration_TestAttachmentsForBinary_Reply";
	private static final String MSG_FAILED = "test_Integration_TestAttachmentsForBinary_Failed";
	private static final String DESTINATION_QUEUE = "TestAttachmentsQueue";
	private static final String MSG = "test_Integration_TestAttachmentsForBinary";
	
	public void test_Integration_TestBinaryNotAsAttachment_Success_TestCase() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA3();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if (results != null && results.length != 0) {
					pass = !WrapperUtil.isNodeXopInclude((Node)results[0]);
					if(pass){
						String base64Binary = ((Text) results[0]).getTextContent();
						assertEquals(MSG_REPLY,  Base64Utils.base64Decode(base64Binary));
					}
				}
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
			}
			assertTrue(pass);
				
		}finally{
			cleanup(context);
		}
	}
	
	public void test_Integration_TestBinaryAttachment_SendBinaryInline_Testcase() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, true));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA2();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if (results != null && results.length != 0) {
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
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
			}
			assertTrue(pass);
				
		}finally{
			cleanup(context);
		}
	}
	
	public void test_Integration_TestBinaryAttachment_Success_TestCase() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA2();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			context.getDeliveryChannel().sendSync(inOut);
			if (inOut.getStatus() == ExchangeStatus.ERROR) {
				pass = false;
			} else {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if (results != null && results.length != 0) {
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
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);
			}
			assertTrue(pass);
				
		}finally{
			cleanup(context);
		}
	}
	
	
	public void test_Integration_TestBinaryAttachment_Failure_TestCase() throws Throwable {
		//Activate end point and message processor
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(DESTINATION_QUEUE, JMSConstants.QUEUE, context);
		
		JbiHelper.registerEndPoint(JMS_SERVICE_CONSUMER, CONSUMER_EP, new MessageConsumerImpl(context, false));
		context.activateEndpoint(JMS_SERVICE_CONSUMER, CONSUMER_EP);
		
		//Deploy
		deploySA1();
		
		try{
			
			boolean pass = true;
			InOut inOut = createInOutMessageExchange(MSG, context);
			context.getDeliveryChannel().sendSync(inOut);
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
			if (results.length == 0) {
				pass = false;
			}
			assertFalse(pass);
				
		}finally{
			cleanup(context);
		}
	}

	private void cleanup(final ComponentContext context) {
		try{
			JbiHelper.deactivateEndpoint(context, JMS_SERVICE_CONSUMER, CONSUMER_EP);
		}catch(Throwable t){}
		try{
			unDeploy();
		}catch(Throwable t){}
	}
	
	private static class MessageConsumerImpl implements MessageConsumer{
		private ComponentContext context;
		private boolean sendInline = false;
		public MessageConsumerImpl(ComponentContext context, boolean sendInline){
			this.context = context;
			this.sendInline = sendInline;
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
				pass = WrapperUtil.isNodeXopInclude((Node)results[0]);
				if(pass){
					String cid = WrapperUtil.getXopContentId((Node)results[0]);
					pass = cid != null && cid.trim().length()!=0;
					if(pass){
						byte[] bytes = JbiHelper.getAttachment(inOut.getInMessage(), cid);
						pass = bytes != null & bytes.length != 0;
						if(pass){
							pass =  compareStr(MSG, new String(bytes));
						}
					}
				}
			}else{
				pass = false;
			}
			
			NormalizedMessage nm = inOut.createMessage();
			inOut.setOutMessage(nm);
			byte[] reply;
			String xml;
			if(pass){
				reply = MSG_REPLY.getBytes();
			}else{
				reply = MSG_FAILED.getBytes();
			}
			if(sendInline){
				reply = Base64Utils.string2Base64(reply);
				xml = JbiHelper.wrapIntoJBIMessage(MESSAGE_REPLY, new String[] {new String(reply)});
			}else{
				String cid = WrapperUtil.createXopCid();
				xml = JbiHelper.wrapIntoJBIMessageAsAttachment(MESSAGE_REPLY, cid);
		    	nm.addAttachment(cid, new DataHandler(new JMSBCDataSource(
						new ByteArrayInputStream(reply), "part1")));
			}
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
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
		String cid = WrapperUtil.createXopCid();
		String xml = JbiHelper.wrapIntoJBIMessageAsAttachment(MESSAGE_REQUEST, cid);
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
    	nm.addAttachment(cid, new DataHandler(new JMSBCDataSource(
				new ByteArrayInputStream(MSG.getBytes()), "part1")));
		inOut.setInMessage(nm);
		return inOut;
	}
	
	private String createTestSU1() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-1.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}

	private String createTestSU2() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-2.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}

	private String createTestSU3() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-3.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}
	
	private void deploySA1() throws Exception{
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		String zipTestSU = createTestSU1();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}
	
	private void deploySA2() throws Exception{
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		String zipTestSU = createTestSU2();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}
	
	private void deploySA3() throws Exception{
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		String zipTestSU = createTestSU3();
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
