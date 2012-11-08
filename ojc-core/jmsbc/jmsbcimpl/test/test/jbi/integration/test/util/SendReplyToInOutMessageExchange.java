package test.jbi.integration.test.util;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

public class SendReplyToInOutMessageExchange implements Command {

	private QName mService;
	private String mEndPoint;
	private QName mOperation;
	private String[] mParts;
	private QName mMessage;
	
	public SendReplyToInOutMessageExchange(QName service, String endpoint,
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
		InOut ex = (InOut)JbiHelper.getNextMessage(mService, mEndPoint);
		if(ex != null && ex.getStatus() != ExchangeStatus.ERROR){
			NormalizedMessage nm = ex.createMessage();
			//Create a message 
			String xml = JbiHelper.wrapIntoJBIMessage(mMessage, mParts);
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
			
			ex.setOutMessage(nm);
			context.getDeliveryChannel().sendSync(ex);

			if(ex.getStatus() == ExchangeStatus.DONE)
				return Constants.Result.PASS;
			else{
				Object[] errorDetails = new Object[]{ex.getError(), ex.getProperty("com.sun.jbi.crl.faultcode")};
				return errorDetails;
			}
		}
		return Constants.Result.FAIL;
	}

}
