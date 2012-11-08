package com.sun.mock.participant2;

import java.security.cert.CRL;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider;

public class ParticipantSE2InOnlyConsumer extends AbstractInOnlyConsumer  {

	private static final Logger LOGGER = Logger.getLogger(ParticipantSE2InOnlyConsumer.class.getName());
	private Participant2Engine mEngine;
	
	private AcceptManager mAcceptManager;


	public ParticipantSE2InOnlyConsumer(Participant2Engine engine, AcceptManager acceptManager) {
		this.mEngine = engine;
		this.mAcceptManager = acceptManager;
	}
	
	@Override
	public void processStatus(CRLInOnly msg, ExchangeContext ctx) throws JBIException {
		/*InOnlyWrapper inWrapper = mRequests.get(msg.getExchangeId());
		if(inWrapper != null) {
			if(msg.getStatus() == ExchangeStatus.DONE) {
				inWrapper.getEntry().getEndpointStatus().incrementReceivedDones();
			} else if(msg.getStatus() == ExchangeStatus.ERROR) {
				inWrapper.getEntry().getEndpointStatus().incrementReceivedErrors();
			}
		}*/
		
	}
	
	
}