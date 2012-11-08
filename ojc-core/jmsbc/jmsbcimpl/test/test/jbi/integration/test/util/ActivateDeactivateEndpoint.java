package test.jbi.integration.test.util;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;

import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

public class ActivateDeactivateEndpoint implements Command {

	private QName mService;
	private String mEndPoint;
	private boolean mActivate;
	
	public boolean isActivate() {
		return mActivate;
	}

	public void setActivate(boolean activate) {
		mActivate = activate;
	}

	public ActivateDeactivateEndpoint(QName service, String endpoint){
		setService(service);
		setEndPoint(endpoint);
		mActivate = true;
	}
	
	public QName getService() {
		return mService;
	}

	public void setService(QName service) {
		mService = service;
	}

	public String getEndPoint() {
		return mEndPoint;
	}

	public void setEndPoint(String endpoint) {
		mEndPoint = endpoint;
	}

	public Serializable execute(ComponentContext context) throws Exception {
		if(mActivate)
			JbiHelper.activateEndpoint(context, mService, mEndPoint);
		else
			JbiHelper.deactivateEndpoint(context, mService, mEndPoint);
			
		return Constants.Result.PASS;
	}

}
