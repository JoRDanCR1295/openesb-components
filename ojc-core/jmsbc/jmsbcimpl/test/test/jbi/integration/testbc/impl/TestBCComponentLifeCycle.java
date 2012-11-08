package test.jbi.integration.testbc.impl;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.management.ObjectName;

import test.jbi.integration.testbc.core.ConnectionListener;

public class TestBCComponentLifeCycle implements ComponentLifeCycle {

	private ComponentContext mComponentContext;
	private ConnectionListener mConnectionListener;
	private int port;
	public ObjectName getExtensionMBeanName() {
		return null;
	}

	public void init(final ComponentContext arg0) throws JBIException {
		mComponentContext = arg0;
		InputStream in = getClass().getResourceAsStream("config.properties");
		Properties prop = new Properties();
		if(in!=null){
			try {
				prop.load(in);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		port = 9888;
		try{
			port = Integer.parseInt(prop.getProperty("TestBCPort", "9888"));
		}catch(Throwable t){
		}
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
			mConnectionListener = new ConnectionListener(port, mComponentContext, mComponentContext.getWorkspaceRoot());
		}catch(IOException e){
			throw new JBIException("Could not start TestBC.", e);
		}
		mConnectionListener.start(); 
	}

	public void stop() throws JBIException {
		shutDown();
	}

}
