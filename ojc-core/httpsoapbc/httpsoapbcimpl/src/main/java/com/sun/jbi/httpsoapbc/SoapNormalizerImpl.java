/**
 * 
 */
package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.ws.handler.MessageContext;

/**
 * @author Sujit Biswas
 * 
 */
public class SoapNormalizerImpl implements SoapNormalizer {

    private SoapNormalizer soap11Normalizer = new Soap11Normalizer();
    private SoapNormalizer soap12Normalizer = new Soap12Normalizer();
    protected boolean mPropagateSoapHeader = true;
    
    public void setPropagateSoapHeader(boolean propagateSoapHeader) {
        mPropagateSoapHeader = propagateSoapHeader;
    }

    public NormalizedMessage normalize(Object message, MessageExchange exchange, OperationMetaData meta, boolean inMsg, MessageContext context) throws MessagingException,
	    SOAPException {
	if (meta.isSoap12()) {
	    soap12Normalizer.setPropagateSoapHeader(mPropagateSoapHeader);
	    return soap12Normalizer.normalize(message, exchange, meta, inMsg, context);
	} else {
	    soap11Normalizer.setPropagateSoapHeader(mPropagateSoapHeader);
	    return soap11Normalizer.normalize(message, exchange, meta, inMsg, context);
	}
    }

    public Fault normalizeFault(SOAPFault soapFault, MessageExchange exchange, OperationMetaData meta, boolean failFast) throws MessagingException, SOAPException {
	if (meta.isSoap12()) {
	    return soap12Normalizer.normalizeFault(soapFault, exchange, meta, failFast);
	} else {
	    return soap11Normalizer.normalizeFault(soapFault, exchange, meta, failFast);
	}
    }

}
