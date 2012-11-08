/**
 * 
 */
package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;

/**
 * @author Sujit Biswas
 *
 */
public class SoapDenormalizerImpl implements SoapDenormalizer {

   
    
    private SoapDenormalizer soap11Denormalizer = new Soap11Denormalizer();
    private SoapDenormalizer soap12Denormalizer = new Soap12Denormalizer();
    
    public SOAPMessage denormalize(NormalizedMessage normalizedMessage, MessageExchange exchange, OperationMetaData meta, MessageFactory messageFactory, boolean inMsg)
	    throws MessagingException {
	if(meta.isSoap12()){
	    return soap12Denormalizer.denormalize(normalizedMessage, exchange, meta, messageFactory, inMsg);
	}else {
	    return soap11Denormalizer.denormalize(normalizedMessage, exchange, meta, messageFactory, inMsg);
	}
    }

}
