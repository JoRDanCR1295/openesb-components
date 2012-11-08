package test.jbi.integration.testse.impl;

import java.io.IOException;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.management.ObjectName;

import test.jbi.integration.testse.core.ConnectionListener;

public class TestSEComponentLifeCycle implements ComponentLifeCycle {

	private ComponentContext mComponentContext;
	private ConnectionListener mConnectionListener;
	public ObjectName getExtensionMBeanName() {
		return null;
	}

	public void init(ComponentContext arg0) throws JBIException {
		mComponentContext = arg0;
	}

	public void shutDown() throws JBIException {
		if(mConnectionListener != null){
			mConnectionListener.close();
			mConnectionListener = null;
		}
		
		JbiHelper.stop();
	}

	public void start() throws JBIException {
		shutDown(); //Stop the existing processor first
		JbiHelper.start(mComponentContext);

		try{
			mConnectionListener = new ConnectionListener(9888, mComponentContext);
		}catch(IOException e){
			throw new JBIException("Could not start TestBC.", e);
		}
		mConnectionListener.start(); 
	}

	public void stop() throws JBIException {
		shutDown();
	}

}
