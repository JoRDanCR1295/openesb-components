package test.jbi.integration.test.util;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.namespace.QName;

import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

public class SendErrorToNextMessageExchange implements Command {

	private QName mService;
	private String mEndPoint;
	private String faultCode;
	private String faultDetails;
	private String faultFactor;
	private Exception error;
	
	public String getFaultCode() {
		return faultCode;
	}

	public void setFaultCode(String faultCode) {
		this.faultCode = faultCode;
	}

	public String getFaultDetails() {
		return faultDetails;
	}

	public void setFaultDetails(String faultDetails) {
		this.faultDetails = faultDetails;
	}

	public String getFaultFactor() {
		return faultFactor;
	}

	public void setFaultFactor(String faultFactor) {
		this.faultFactor = faultFactor;
	}

	public Exception getError() {
		return error;
	}

	public void setError(Exception error) {
		this.error = error;
	}

	public SendErrorToNextMessageExchange(QName service, String endpoint){
		setService(service);
		setEndPoint(endpoint);
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
		MessageExchange ex = JbiHelper.getNextMessage(mService, mEndPoint);
		if(ex == null)
			return Constants.Result.FAIL;
		ex.setStatus(ExchangeStatus.ERROR);
		if(error != null){
			ex.setError(error);
			ex.setProperty("com.sun.jbi.crl.faultcode", faultCode);
			ex.setProperty("com.sun.jbi.crl.faultstring", error.getMessage());
			ex.setProperty("com.sun.jbi.crl.faultactor", "testbc");
			ex.setProperty("com.sun.jbi.crl.faultdetail", faultDetails);
		}
		context.getDeliveryChannel().send(ex);
		return Constants.Result.PASS;
	}

}
