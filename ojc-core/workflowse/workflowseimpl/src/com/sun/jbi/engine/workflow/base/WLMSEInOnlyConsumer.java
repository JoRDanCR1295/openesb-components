package com.sun.jbi.engine.workflow.base;

import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;
import com.sun.jbi.engine.workflow.process.ConsumerCallBack;

public class WLMSEInOnlyConsumer implements ConsumerCallBack {

	private static final Logger LOGGER = Logger.getLogger(WLMSEInOnlyConsumer.class.getName());
	private WorkflowEngine mEngine;
	
//	private AcceptManager mAcceptManager;
    private WLMSEManagerContext mManagerContext;

	private Hashtable<String, InOnlyWrapper> mRequests = new Hashtable<String, InOnlyWrapper>();

	public WLMSEInOnlyConsumer(WorkflowEngine engine, WLMSEManagerContext managerContext) {
		this.mEngine = engine;
		this.mEngine.setConsumerCallBack(this);
		this.mManagerContext = managerContext;
	}
	

	public void processStatus(InOnly msg) throws JBIException {
		InOnlyWrapper inWrapper = mRequests.remove(msg.getExchangeId());
		if(inWrapper != null) {
			if(msg.getStatus() == ExchangeStatus.DONE) {
//				inWrapper.getEntry().getEndpointStatus().incrementReceivedDones();
			} else if(msg.getStatus() == ExchangeStatus.ERROR) {
//				inWrapper.getEntry().getEndpointStatus().incrementReceivedErrors();
               ( (WLMSEExchangeHandler)mManagerContext.getExchangeHandler()).logError(LOGGER, mManagerContext.getComponentContext(), msg);
			}
		}
		
	}
	
	public void onNotify(WorkflowMapEntry entry, DOMSource reply) {
		try {

            MessageExchangeFactory factory = mManagerContext.getMessagingChannel().getExchangeFactory(); 
			InOnly inOnly =   factory.createInOnlyExchange();
			NormalizedMessage nm = inOnly.createMessage();
			nm.setContent(reply);
			
			QName interfaceName = entry.getInterface();
            QName serviceName = entry.getService();
			String endpointName = entry.getEndpoint();
			String operation = entry.getOperation();
			if(operation != null) {
				inOnly.setOperation(new QName (interfaceName.getNamespaceURI(), operation ));
			}
			ServiceEndpoint se = mManagerContext.getComponentContext().getEndpoint(serviceName, endpointName);
			if(se != null) {
				inOnly.setInMessage(nm);
				inOnly.setEndpoint(se);
				mRequests.put(inOnly.getExchangeId(), new InOnlyWrapper(inOnly, entry));
//				inOnly.send();
                mManagerContext.getMessagingChannel().send(inOnly);
			}
			
			
		} catch(JBIException ex) {
			LOGGER.log(Level.SEVERE, "error sending notification on deliver channel", ex);
		}
	}
	
	

	   
	    private static class InOnlyWrapper {
	        private InOnly mInOnly;

	        private WorkflowMapEntry mEntry;

	        public InOnlyWrapper(InOnly inOnly, WorkflowMapEntry entry) {
	        	mInOnly = inOnly;
	            mEntry = entry;
	        }

	        public WorkflowMapEntry getEntry() {
	            return mEntry;
	        }

	        public InOnly getInOut() {
	            return mInOnly;
	        }

	    }
	
}