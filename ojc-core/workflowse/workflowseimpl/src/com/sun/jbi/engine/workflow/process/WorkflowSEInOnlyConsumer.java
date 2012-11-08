package com.sun.jbi.engine.workflow.process;

import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer;
import com.sun.jbi.engine.workflow.WorkflowEngine;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;

public class WorkflowSEInOnlyConsumer extends AbstractInOnlyConsumer implements ConsumerCallBack {

	private static final Logger LOGGER = Logger.getLogger(WorkflowSEInOnlyConsumer.class.getName());
	private WorkflowEngine mEngine;
	
	private AcceptManager mAcceptManager;

	private Hashtable<String, InOnlyWrapper> mRequests = new Hashtable<String, InOnlyWrapper>();

	public WorkflowSEInOnlyConsumer(WorkflowEngine engine, AcceptManager acceptManager) {
		this.mEngine = engine;
		this.mEngine.setConsumerCallBack(this);
		this.mAcceptManager = acceptManager;
	}
	
	@Override
	public void processStatus(CRLInOnly msg, ExchangeContext ctx) throws JBIException {
		InOnlyWrapper inWrapper = mRequests.remove(msg.getExchangeId());
		if(inWrapper != null) {
			if(msg.getStatus() == ExchangeStatus.DONE) {
				inWrapper.getEntry().getEndpointStatus().incrementReceivedDones();
			} else if(msg.getStatus() == ExchangeStatus.ERROR) {
				inWrapper.getEntry().getEndpointStatus().incrementReceivedErrors();
			}
		}
		
	}
	
	public void onNotify(WorkflowMapEntry entry, DOMSource reply) {
		try {
			ComponentManager cm = this.mAcceptManager.getComponentManager();
			ComponentContext cc = cm.getComponentContext();
			CRLMessageExchangeFactory factory = this.mAcceptManager.getComponentManager().getExchangeFactory();
			CRLInOnly inOnly =  (CRLInOnly) factory.createExchange(ExchangePattern.IN_ONLY);
			NormalizedMessage nm = inOnly.createMessage();
			nm.setContent(reply);
			
			QName interfaceName = entry.getInterface();
            QName serviceName = entry.getService();
			String endpointName = entry.getEndpoint();
			String operation = entry.getOperation();
			if(operation != null) {
				inOnly.setOperation(new QName (interfaceName.getNamespaceURI(), operation ));
			}
			ServiceEndpoint se = cc.getEndpoint(serviceName, endpointName);
			if(se != null) {
				inOnly.setInMessage(nm);
				inOnly.setEndpoint(se);
				mRequests.put(inOnly.getExchangeId(), new InOnlyWrapper(inOnly, entry));
				inOnly.send();
			}
			
			
		} catch(JBIException ex) {
			LOGGER.log(Level.SEVERE, "error sending notification on deliver channel", ex);
		}
	}
	
	

	   
	    private static class InOnlyWrapper {
	        private CRLInOnly mInOnly;

	        private WorkflowMapEntry mEntry;

	        public InOnlyWrapper(CRLInOnly inOnly, WorkflowMapEntry entry) {
	        	mInOnly = inOnly;
	            mEntry = entry;
	        }

	        public WorkflowMapEntry getEntry() {
	            return mEntry;
	        }

	        public CRLInOnly getInOut() {
	            return mInOnly;
	        }

	    }
	
}