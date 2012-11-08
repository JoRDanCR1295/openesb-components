package test.jbi.jmsbc.integration.testcases.bugfixes;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.jmsbc.integration.testcases.nmproperties.DrainDestination;

import com.sun.jbi.ui.common.JBIRemoteException;

public class TestFreezeAtShutdown extends IntegrationTestCase {

	private static final String SA_NAME = "TEST-TestFreezeAtShutdown-SA";
	private static final long WAIT = 10000;

	//Set consumer service end points
	private static final String TARGET_NAMESPACE = "http://j2ee.netbeans.org/wsdl/JMSOutShutdownTest";
	private static final QName JMS_SERVICE = new QName(TARGET_NAMESPACE,"JMSOutService");
	private static final String PROVIDER_EP = "JMSOutPort";

	//set operation name
	private static final QName OPERATOR = new QName("JMSOutOperation");
	private static final QName MESSAGE = new QName(
			TARGET_NAMESPACE, "JMSOutOperationRequest");
	
	private String zipSA;
	
	public void test_Integration_Freeze_At_Shutdown() throws Throwable {
		final String MSG = "test_Integration_Freeze_At_Shutdown";
		final ComponentContext context = JbiHelper.getComponentContext();
		drainMessages(context);
		sendMessage(MSG, context);
		JbiHelper.activateEndpoint(context, JMS_SERVICE, PROVIDER_EP);
		deploy();
		Thread.sleep(WAIT);
		try{
			InOnly inOnly = (InOnly)JbiHelper.getNextMessage(JMS_SERVICE, PROVIDER_EP);
			if(inOnly == null){
				assertTrue(false);
			}
			Object[] results = JbiHelper.unwrapFromJBISource(inOnly.getInMessage().getContent());
			if (results != null) {
				assertEquals(MSG, ((Text) results[0]).getTextContent());
			}
			
			getInstaller().shutdownServiceAssembly(SA_NAME);
			
			inOnly.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOnly);
			
		}finally{
			try{
				unDeploy();
			}catch(Throwable t){}
			try{
				JbiHelper.deactivateEndpoint(context, JMS_SERVICE, PROVIDER_EP);
			}catch(Throwable t){}
		}
		
	}

	private void sendMessage(final String MSG, final ComponentContext context)
			throws JBIRemoteException, Exception {
		SendMessage sendMessage = new SendMessage(getInstaller(),
				"TestShutdownQueue", "Queue", context);
		sendMessage.sendMessage(MSG);
	}

	private void drainMessages(ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), "TestShutdownQueue", "Queue", context);
		drain.drainDestiantion();
	}
	
	private String createSA() throws IOException {
		if(zipSA != null)
			return zipSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU", "JMS-BC-Test-SU");
		su.addWsdl(Configuration.getPath(getClass(), "JMS-Shutdown.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-shutdown.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		zipSA = sa.assemble(Configuration.getWorkingDir());
		return zipSA;
	}
	
	private void unDeploy() {
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
	}
	
	private void deploy() throws Exception{
		getInstaller().deployServiceAssembly(createSA());
		getInstaller().startServiceAssembly(SA_NAME);
	}

}
