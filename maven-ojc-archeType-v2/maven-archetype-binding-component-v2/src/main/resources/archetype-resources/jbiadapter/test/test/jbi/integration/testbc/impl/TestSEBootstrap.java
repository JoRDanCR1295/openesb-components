#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.impl;

import javax.jbi.JBIException;
import javax.jbi.component.Bootstrap;
import javax.jbi.component.InstallationContext;
import javax.management.ObjectName;

public class TestSEBootstrap implements Bootstrap {

	public void cleanUp() throws JBIException {
		//do nothing
	}

	public ObjectName getExtensionMBeanName() {
		return null;
	}

	public void init(InstallationContext arg0) throws JBIException {
		//do nothing
	}

	public void onInstall() throws JBIException {
		//do nothing
	}

	public void onUninstall() throws JBIException {
		//do nothing
	}

}
