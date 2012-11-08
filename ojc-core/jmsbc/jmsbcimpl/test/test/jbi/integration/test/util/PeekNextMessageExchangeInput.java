package test.jbi.integration.test.util;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;

import test.jbi.integration.testbc.core.Command;
import test.jbi.integration.testbc.impl.JbiHelper;

public class PeekNextMessageExchangeInput implements Command {

	private QName mService;
	private String mEndPoint;
	
	public PeekNextMessageExchangeInput(QName service, String endpoint){
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
		MessageExchange ex = (MessageExchange)JbiHelper.peekNextMessage(mService, mEndPoint);
		Object[] results = null;
		if(ex != null && ex.getStatus() != ExchangeStatus.ERROR){
			NormalizedMessage src;
			if(ex instanceof InOnly){
				src = ((InOnly)ex).getInMessage();
			}else{
				src = ((InOut)ex).getInMessage();
			}
			results = JbiHelper.unwrapFromJBISource(src.getContent());
		}
		if(results != null && results.length > 0){
			return results;
		}
		return Constants.Result.FAIL;
	}

}
