package test.jbi.jmsbc.integration.testcases.bugfixes;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.InstallerService;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.NMPropertiesUtil;

public class SendMessage {
	private static final String JMS_BC_SU = "Test-SU-Send-Message";
	private static final String SA_NAME = "Test-SA-Send-Message";
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMSOutSend";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSOutService");
	private static final String PROVIDER_EP = "JMSOutPort";
	private static final QName OPERATION = new QName("JMSOutOperation");
	private static final QName MESSAGE = new QName(JMS_TNS,
	"JMSOutOperationRequest");
	
	private InstallerService installer;
	private String destination;
	private String destinationType;
	private ComponentContext context;
	
	
	public SendMessage(InstallerService installer, String dest, String destinationType, ComponentContext context){
		this.installer = installer;
		this.destination = dest;
		this.destinationType = destinationType;
		this.context = context;
	}
	
	public void sendMessage(String msg) throws Exception{
		String zipTestSA = createTestSA();
		installer.deployServiceAssembly(zipTestSA);
		installer.startServiceAssembly(SA_NAME);
		try{
		
				ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
				InOnly inOnly = context.getDeliveryChannel().createExchangeFactory()
						.createInOnlyExchange();
				inOnly.setEndpoint(ep);
				inOnly.setOperation(OPERATION);
				NormalizedMessage nm = inOnly.createMessage();
				inOnly.setInMessage(nm);
				nm.setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, destination);
				nm.setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, destinationType);
				String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE, new String[] { msg });
				nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
				context.getDeliveryChannel().sendSync(inOnly);
				if (inOnly.getStatus() == ExchangeStatus.ERROR) {
					throw inOnly.getError();
				}
			
		}finally{
			try {
				installer.undeployServiceAssembly(SA_NAME);
			} catch (Throwable t) {}
		}
		
	}

	private String createTestSA() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-send-message.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-send-message.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}
}
