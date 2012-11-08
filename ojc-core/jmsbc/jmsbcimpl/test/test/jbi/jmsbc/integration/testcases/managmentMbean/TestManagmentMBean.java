package test.jbi.jmsbc.integration.testcases.managmentMbean;

import java.io.IOException;
import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanException;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerInvocationHandler;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.xml.namespace.QName;

import junit.framework.Assert;

import org.w3c.dom.Text;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.jmsbc.integration.testcases.qos.QOSTestCaseRedlivery;

import com.sun.jbi.jmsbc.mbeans.JMSBCManagementMBean;
import com.sun.jbi.jmsbc.util.GUIDUtil;

public class TestManagmentMBean extends IntegrationTestCase {

	private static final String SA_NAME = "JMSBC-TEST_REDELIVERY-SA";
	private static final int WAIT_TIME = 500;
	private static final int MAX_ATTEMPTS = 5;
	private static final String CONSUMER_EP = "JMSPortRecv";
	private static final QName JMS_SERVICE = new QName(
			"http://j2ee.netbeans.org/wsdl/JMS", "JMSService");
	private static final String OP_NAME = "op-name";
	private static final QName DELIVERY_SERVICE_NAME = new QName(
			"urn:JMSBC-TEST_REDELIVERY-SA", "Service");
	private static final String DELIVERY_EP = "deliveryEP";

	private JMSBCManagementMBean mMBean;
	private JMXConnector jmxc;

	public void testSuspendResume1() throws Throwable {

		//Create TestSU
		String zipTestSU = createTestSU();
		String saName = null;
		try {
			
			//activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			//Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(zipTestSU);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(2000); //wait some time before SU start sending messages

			//Drain all the messages meant for this EP
			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			String str;
			while(!(str = (String)getConnection().execute(receviceMsgCommand)).equals("FAIL"));

			//send a message
			SendMsgCommand sendMsgCommand = new SendMsgCommand();
			sendMsgCommand.msg = GUIDUtil.generateGUID();
			getConnection().execute(sendMsgCommand);
			Thread.sleep(5000);

			//Receive a message to check the end point is working
			String msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals(sendMsgCommand.msg, msg);

			//Now suspend the end point
			getMBean().suspend("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");

			//send a message
			sendMsgCommand.msg = GUIDUtil.generateGUID();
			getConnection().execute(sendMsgCommand);
			Thread.sleep(5000);
			
			//receive message
			msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals("FAIL", msg);

			//activate the end point and then receive
			getMBean().resume("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			Thread.sleep(5000);
			msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals(sendMsgCommand.msg, msg);

		} finally {
			try{
				//Undeploy SA
				getInstaller().undeployServiceAssembly(SA_NAME);
			}catch(Throwable t){}
		}
	}

	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
		getMBean();
	}


	@Override
	protected void tearDown() throws Exception {
		if (jmxc != null) {
			try{jmxc.close();}catch(Throwable t){}
		}
		//Make sure that SA is undeployed
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
		// TODO Auto-generated method stub
		super.tearDown();
	}


	public void testSuspendResume2() throws Throwable {

		//Create TestSU
		String zipTestSU = createTestSU();
		String saName = null;
		try {
			//activate endpoint
			ActivateDeactivateEP activateDeactivateEPCmd = new ActivateDeactivateEP();
			activateDeactivateEPCmd.activate = true;
			getConnection().execute(activateDeactivateEPCmd);

			//Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(zipTestSU);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(20000); //wait some time before SU start sending messages

			//Drain all the messages meant for this EP
			ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
			String str;
			while(!(str = (String)getConnection().execute(receviceMsgCommand)).equals("FAIL"));

			//send a message
			SendMsgCommand sendMsgCommand = new SendMsgCommand();
			sendMsgCommand.msg = GUIDUtil.generateGUID();
			getConnection().execute(sendMsgCommand);
			Thread.sleep(5000);

			//Receive a message to check the end point is working
			String msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals(sendMsgCommand.msg, msg);

			//send a message first and then suspend the end point
			sendMsgCommand.msg = GUIDUtil.generateGUID();
			getConnection().execute(sendMsgCommand);
			Thread.sleep(1000);
			
			//Now suspend the end point
			getMBean().suspend("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			
			//receive message
			//Here you would still have received even after suspending.
			//But this exchange would be rolledback on JMSBC side so after resume you
			//would still receive the same message. This is the expected behavior.
			msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals(sendMsgCommand.msg, msg);

			//activate the end point and then receive
			getMBean().resume("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			Thread.sleep(5000);
			msg = (String) getConnection().execute(receviceMsgCommand);
			Assert.assertEquals(sendMsgCommand.msg, msg);

		} finally {
			try{
				//Undeploy SA
				getInstaller().undeployServiceAssembly(SA_NAME);
			}catch(Throwable t){}
		}
	}
	
	public void testOtherMethods() throws Throwable {

		//Create TestSU
		String zipTestSU = createTestSU();
		String saName = null;
		try {

			//Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(zipTestSU);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(20000); //wait some time before SU start sending messages

			assertTrue(getMBean()
					.isEndpointActive(
							"http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv"));
			
			
			assertTrue(getMBean()
					.isEndpointActive(
							"http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortSend,provider"));
			

			HashSet<String> set = new HashSet<String>();
			String[] eps = getMBean().listActiveEndpoints();
			for(int i=0; i<eps.length; ++i){
				set.add(eps[i]);
			}
			assertTrue(set.contains("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortSend,provider"));
			assertTrue(set.contains("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer"));
			
			
			//Test suspend
			getMBean().suspend("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			eps = getMBean().listActiveEndpoints();
			set.clear();
			for(int i=0; i<eps.length; ++i){
				set.add(eps[i]);
			}
			assertFalse(set.contains("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer"));
			
			
			//Now resume
			getMBean().resume("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			eps = getMBean().listActiveEndpoints();
			set.clear();
			for(int i=0; i<eps.length; ++i){
				set.add(eps[i]);
			}
			assertTrue(set.contains("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer"));
			
		} finally {
			try{
				//Undeploy SA
				getInstaller().undeployServiceAssembly(SA_NAME);
			}catch(Throwable t){}
		}
	}
	
	public void testOtherMethodsNegativeTests() throws Throwable {

		//Create TestSU
		String zipTestSU = createTestSU();
		String saName = null;
		try {

			//Now deploy and start SA
			saName = getInstaller().deployServiceAssembly(zipTestSU);
			getInstaller().startServiceAssembly(saName);
			Thread.sleep(20000); //wait some time before SU start sending messages

			boolean fail = false;
			try{
				getMBean()
						.isEndpointActive(
								"http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv2,consumer");
			}catch(MBeanException e){
				fail = true;
			}
			assertTrue(fail);
			

			getMBean().suspend("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			fail = false;
			try{
				fail = !getMBean()
						.isEndpointActive(
								"http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");
			}catch(MBeanException e){
			}
			assertTrue(fail);
			
			
			getMBean().resume("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortRecv,consumer");

			fail = false;
			try{
				getMBean().suspend("http://j2ee.netbeans.org/wsdl/JMS,JMSService,JMSPortSend,provider");
			}catch(MBeanException e){
				fail = true;
			}
			assertTrue(fail);
			
		} finally {
			try{
				//Undeploy SA
				getInstaller().undeployServiceAssembly(SA_NAME);
			}catch(Throwable t){}
		}
	}

	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-SU-RedeliveryTest",
				"JMS-BC-SU-RedeliveryTest");
		su.addWsdl(Configuration.getPath(QOSTestCaseRedlivery.class, "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(QOSTestCaseRedlivery.class, "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME,
				SA_NAME);
		sa.addSUAssembler(su);
		//Now add connection
		sa.addConnection(JMS_SERVICE, CONSUMER_EP, DELIVERY_SERVICE_NAME,
				DELIVERY_EP, null);

		return sa.assemble(Configuration.getWorkingDir());
	}

	private JMSBCManagementMBean getMBean() throws IOException,
			MalformedObjectNameException, NullPointerException {
		
		if(mMBean != null)
			return mMBean;
		
		String[] credentials = new String[] { Configuration.getJbiUsername(),
				Configuration.getJbiPassword() };

		HashMap props = new HashMap();
		props.put("jmx.remote.credentials", credentials);
		JMXServiceURL url = new JMXServiceURL(
				"service:jmx:rmi:///jndi/rmi://:8686/jmxrmi");
		jmxc = JMXConnectorFactory.connect(url, props);
		MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();
		ObjectName objName = new ObjectName(
				"*:ComponentName=sun-jms-binding,CustomControlName=Administration,*");
		Set mbeans = mbsc.queryNames(objName, null);
		if (mbeans == null || mbeans.isEmpty()) {
		}
		objName = (ObjectName) mbeans.toArray()[0];
		mMBean = (JMSBCManagementMBean) MBeanServerInvocationHandler
				.newProxyInstance(mbsc, objName, JMSBCManagementMBean.class,
						false);

		return mMBean;
	}

	public static class SendMsgCommand implements Command {
		public String msg;

		public Serializable execute(ComponentContext context) throws Exception {
			ServiceEndpoint ep = context
					.getEndpoint(JMS_SERVICE, "JMSPortSend");
			InOnly inOnly = context.getDeliveryChannel()
					.createExchangeFactory().createInOnlyExchange();
			inOnly.setEndpoint(ep);
			inOnly.setOperation(new QName("JMSOperationSend"));
			NormalizedMessage nm = inOnly.createMessage();

			//Create a message 
			String xml = JbiHelper.wrapIntoJBIMessage(new QName(
					"http://j2ee.netbeans.org/wsdl/JMS",
					"JMSOperationSendRequest"), new String[] { msg });
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));

			inOnly.setInMessage(nm);
			context.getDeliveryChannel().sendSync(inOnly);
			if (inOnly.getStatus() == ExchangeStatus.DONE)
				return "SUCCESS";
			else {
				throw inOnly.getError();
			}
		}
	}

	public static class ActivateDeactivateEP implements Command {
		public boolean activate;

		public Serializable execute(ComponentContext context) throws Exception {
			if (activate) {
				JbiHelper.activateEndpoint(context, DELIVERY_SERVICE_NAME,
						DELIVERY_EP);
			} else {
				JbiHelper.deactivateEndpoint(context, DELIVERY_SERVICE_NAME,
						DELIVERY_EP);
			}
			return "SUCCESS";
		}
	}

	public static class ReceiveMsgCommand implements Command {
		public Serializable execute(ComponentContext context) throws Exception {
			InOnly ex = (InOnly) JbiHelper.getNextMessage(
					DELIVERY_SERVICE_NAME, DELIVERY_EP);
			Object[] results = null;
			if (ex != null && ex.getStatus() != ExchangeStatus.ERROR) {
				results = JbiHelper.unwrapFromJBISource(ex.getInMessage()
						.getContent());
				ex.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(ex);
			}
			if (results != null && results.length > 0) {
				return ((Text) results[0]).getTextContent();
			}
			return "FAIL";
		}

	}

}
