package test.jbi.jmsbc.integration.testcases.errorstrategy;

import java.io.IOException;

import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.test.util.Constants;
import test.jbi.integration.test.util.SendInOnlyMessageExchange;

public class TestCommonFaultErrorStrategyOutbound extends IntegrationTestCase {

	private static final String SA_NAME_ERROR = "TEST-ERROR-STRATEGY-SA-ERROR";
	private static final String SA_NAME = "TEST-ERROR-STRATEGY-SA";

	//Set consumer service end points
	private static final String TARGET_NAMESPACE = "http://j2ee.netbeans.org/wsdl/JMSOut";
	private static final QName JMS_SERVICE = new QName(TARGET_NAMESPACE,"JMSOutService");
	private static final String PROVIDER_EP = "JMSOutPort";

	//set operation name
	private static final QName OPERATOR = new QName("JMSOutOperation");
	private static final QName MESSAGE = new QName(
			TARGET_NAMESPACE, "JMSOutOperationRequest");
	
	private String zipErrorSA;
	private String zipSA;
	
	public void testClientError() throws Throwable {
		
		//Deploy and start SA
		getInstaller().deployServiceAssembly(createSA());
		getInstaller().startServiceAssembly(SA_NAME);
		
		//Send a message
		String[] messageParts = new String[]{"<param1>param1</param1><param2>param2</param2>"};
		final QName INVALID_MESSAGE = new QName(
				TARGET_NAMESPACE, "JMSOutOperationRequest123");
		SendInOnlyMessageExchange sendCmd = new SendInOnlyMessageExchange(
				JMS_SERVICE, PROVIDER_EP, OPERATOR, messageParts, INVALID_MESSAGE);
		Object obj = getConnection().execute(sendCmd);
		assertFalse(obj instanceof Constants.Result);
		Object[] arr = (Object[])obj;
		assertTrue(arr[1] != null);
		assertTrue(((String)arr[1]).equalsIgnoreCase("Client"));
	}
	
	public void testServerError() throws Throwable {
		
		//Deploy and start SA
		getInstaller().deployServiceAssembly(createErrorSA());
		getInstaller().startServiceAssembly(SA_NAME_ERROR);
		
		//Send a message
		String[] messageParts = new String[]{"<param1>param1</param1><param2>param2</param2>"};
		SendInOnlyMessageExchange sendCmd = new SendInOnlyMessageExchange(
				JMS_SERVICE, PROVIDER_EP, OPERATOR, messageParts, MESSAGE);
		Object obj = getConnection().execute(sendCmd);
		assertFalse(obj instanceof Constants.Result);
		Object[] arr = (Object[])obj;
		assertTrue(arr[1] != null);
		assertTrue(((String)arr[1]).equalsIgnoreCase("Server"));
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		//Make sure that SA is undeployed
		unDeployTestSA();
		createErrorSA();
		createSA();
	}


	@Override
	protected void tearDown() throws Exception {
		unDeployTestSA();
		super.tearDown();
	}

	private String createErrorSA() throws IOException {
		if(zipErrorSA != null)
			return zipErrorSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU", "JMS-BC-Test-SU");
		su.addWsdl(Configuration.getPath(getClass(), "JMSError.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME_ERROR, SA_NAME_ERROR);
		sa.addSUAssembler(su);
		zipErrorSA = sa.assemble(Configuration.getWorkingDir());
		return zipErrorSA;
	}
	private String createSA() throws IOException {
		if(zipSA != null)
			return zipSA;
		
		JMSBCSUAssembler su = new JMSBCSUAssembler("JMS-BC-Test-SU", "JMS-BC-Test-SU");
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		zipSA = sa.assemble(Configuration.getWorkingDir());
		return zipSA;
	}
	
	private void unDeployTestSA() {
		try{
			getInstaller().undeployServiceAssembly(SA_NAME_ERROR);
		}catch(Throwable t){}
		try{
			getInstaller().undeployServiceAssembly(SA_NAME);
		}catch(Throwable t){}
	}

}
