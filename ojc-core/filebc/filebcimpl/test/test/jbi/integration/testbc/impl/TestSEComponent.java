package test.jbi.integration.testbc.impl;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

public class TestSEComponent implements Component {

	public ComponentLifeCycle getLifeCycle() {
		return new TestSEComponentLifeCycle();
	}

	public Document getServiceDescription(ServiceEndpoint arg0) {
		// TODO Auto-generated method stub
		return null;
	}

	public ServiceUnitManager getServiceUnitManager() {
		return new MyServiceUnitManager();
	}

	public boolean isExchangeWithConsumerOkay(ServiceEndpoint arg0,
			MessageExchange arg1) {
		return true;
	}

	public boolean isExchangeWithProviderOkay(ServiceEndpoint arg0,
			MessageExchange arg1) {
		return true;
	}

	public ServiceEndpoint resolveEndpointReference(DocumentFragment arg0) {
		return null;
	}

	private class MyServiceUnitManager implements ServiceUnitManager{

		public String deploy(String arg0, String arg1)
				throws DeploymentException {
			// TODO Auto-generated method stub
			return null;
		}

		public void init(String arg0, String arg1) throws DeploymentException {
			// TODO Auto-generated method stub
			
		}

		public void shutDown(String arg0) throws DeploymentException {
			// TODO Auto-generated method stub
			
		}

		public void start(String arg0) throws DeploymentException {
			// TODO Auto-generated method stub
			
		}

		public void stop(String arg0) throws DeploymentException {
			// TODO Auto-generated method stub
			
		}

		public String undeploy(String arg0, String arg1)
				throws DeploymentException {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
}
