package test.jbi.jmsbc.integration.testcases.nmproperties;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.InstallerService;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;

import com.sun.jbi.jmsbc.NMPropertiesUtil;

public class DrainDestination {
	private static final String JMS_BC_SU = "Test-SU-Drain-Destination";
	private static final String SA_NAME = "Test-SA-Drain-Destination";
	private static final long WAIT_TIME = 5000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS-Drain-Dest";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort";
	private static final QName OPERATOR_READ = new QName("JMSRead");
	
	private InstallerService installer;
	private String destination;
	private String destinationType;
	private ComponentContext context;
	
	
	public DrainDestination(InstallerService installer, String dest, String destinationType, ComponentContext context){
		this.installer = installer;
		this.destination = dest;
		this.destinationType = destinationType;
		this.context = context;
	}
	
	public void drainDestiantion() throws Exception{
		String zipTestSA = createTestSA();
		installer.deployServiceAssembly(zipTestSA);
		installer.startServiceAssembly(SA_NAME);
		try{
		
			for(;;){
				Thread.sleep(WAIT_TIME);
				ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
				InOut inOut = context.getDeliveryChannel().createExchangeFactory()
						.createInOutExchange();
				inOut.setEndpoint(ep);
				inOut.setOperation(OPERATOR_READ);
				NormalizedMessage nm = inOut.createMessage();
				inOut.setInMessage(nm);
				nm.setProperty(NMPropertiesUtil.OUTBOUND_DESTINATION, destination);
				nm.setProperty(NMPropertiesUtil.OUTBOUND_DESTINATIONTYPE, destinationType);
				context.getDeliveryChannel().sendSync(inOut);
				if (inOut.getStatus() == ExchangeStatus.ERROR) {
					break;
				}
				inOut.setStatus(ExchangeStatus.DONE);
				context.getDeliveryChannel().send(inOut);

				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if(results == null || results.length == 0) //No data read from the queue
					break;
			}
			
		}finally{
			try {
				installer.undeployServiceAssembly(SA_NAME);
			} catch (Throwable t) {}
		}
		
	}

	private String createTestSA() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS-Drain-Destination.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi-Drain-Destination.xml"));
		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}
}
