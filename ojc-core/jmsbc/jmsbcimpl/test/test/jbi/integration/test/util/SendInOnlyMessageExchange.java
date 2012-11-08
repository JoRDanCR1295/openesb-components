package test.jbi.integration.test.util;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

public class SendInOnlyMessageExchange implements Command {

	private QName mService;
	private String mEndPoint;
	private QName mOperation;
	private String[] mParts;
	private QName mMessage;
	
	public SendInOnlyMessageExchange(QName service, String endpoint,
			QName operation, String[] parts, QName message) {
		setService(service);
		setEndPoint(endpoint);
		setOperation(operation);
		setParts(parts);
		setMessage(message);
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

	public QName getOperation() {
		return mOperation;
	}

	public void setOperation(QName operation) {
		mOperation = operation;
	}

	public String[] getParts() {
		return mParts;
	}

	public void setParts(String[] parts) {
		mParts = parts;
	}

	public QName getMessage() {
		return mMessage;
	}

	public void setMessage(QName message) {
		mMessage = message;
	}

	public void setEndPoint(String endPoint) {
		mEndPoint = endPoint;
	}

	public Serializable execute(ComponentContext context) throws Exception {
		ServiceEndpoint ep = context.getEndpoint(mService, mEndPoint);
		InOnly inOnly = context.getDeliveryChannel()
				.createExchangeFactory().createInOnlyExchange();
		inOnly.setEndpoint(ep);
		inOnly.setOperation(mOperation);
		NormalizedMessage nm = inOnly.createMessage();
		
		//Create a message 
		String xml = JbiHelper.wrapIntoJBIMessage(mMessage, mParts);
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		
		inOnly.setInMessage(nm);
		context.getDeliveryChannel().sendSync(inOnly);
		if(inOnly.getStatus() == ExchangeStatus.DONE)
			return Constants.Result.PASS;
		else{
			Object[] errorDetails = new Object[]{inOnly.getError(), inOnly.getProperty("com.sun.jbi.crl.faultcode")};
			return errorDetails;
		}
	}

}
