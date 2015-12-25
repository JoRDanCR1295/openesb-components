/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)JAXWSDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.http.HTTPException;

import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.FaultException;
import com.sun.jbi.httpsoapbc.HttpSoap12Endpoint;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.SoapDenormalizer;
import com.sun.jbi.httpsoapbc.SoapDenormalizerImpl;
import com.sun.jbi.httpsoapbc.util.StringUtil;
import com.sun.jbi.internationalization.Messages;

/**
 * Takes JBI Normalized Message and populates SOAPMessage for use with JAX-WS
 * This normalizer primarily delegates to SOAPNormalizer
 */
public class JAXWSDenormalizer implements Denormalizer {
    
    private static final Messages mMessages =
        Messages.getMessages(JAXWSDenormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(JAXWSDenormalizer.class);
    private static final QName FAULT_DETAIL_ENTRY_QNAME =
            new QName("detailText");
    private static final String COMPONENT_NAME = "sun-http-bc";
    
    static MessageFactory factory11 = null;
    static MessageFactory factory12 = null;
    
    SoapDenormalizer denormalizer;
    
    static {
        try {
            factory11 = MessageFactory.newInstance();
            factory12 = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "HTTPBC-E00631.Failed_to_initialize_SOAP_message_factory", ex);
        }
    }
    
    /** Creates a new instance  */
    public JAXWSDenormalizer() {
        denormalizer = new SoapDenormalizerImpl();
    }
    
    public Object denormalize(NormalizedMessage messageToDenormalize, MessageExchange exchange, Object targetToNormalizeTo, OperationMetaData operationMetaData) throws MessagingException {
        
        SOAPMessage denormalizedMsg = null; //(SOAPMessage) targetToNormalizeTo;
        try {
            denormalizedMsg = denormalizer.denormalize(messageToDenormalize, exchange, operationMetaData, factory11, false);
            
            /** no need to do saveChanges since we don't 
              * have MIME support right now.
              * This is an expensive call - it call transform()
            if (denormalizedMsg.saveRequired()) {
                denormalizedMsg.saveChanges();
            }
            **/
        } catch (Exception ex) {
            throw new MessagingException(ex);
        }
        
        return denormalizedMsg;
    }
    
    public Object denormalizeException(Throwable exceptionToHandle, Object targetToNormalizeTo) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Populating SOAP reply with a fault for the following exception.", exceptionToHandle);
        }
       
        SOAPMessage denormalizedMsg = null;
        
        try {
            // TODO: A better way to determine the default for faultCode
            // TODO: Should this be SOAP 1.1 ("Server") or 1.2 ("Receiver") ?
            String faultCode = FaultException.FAULT_CODE_SERVER;
            String faultActor = "";
            String faultString = "";
            String faultDetail = "";
            if (exceptionToHandle instanceof FaultException) {
                FaultException faultException = (FaultException) exceptionToHandle;
                faultCode = sanitize(faultException.getFaultCode());
                faultActor = sanitize(faultException.getFaultActor());
                faultString = sanitize(faultException.getFaultString());
                faultDetail = sanitize(faultException.getDetail());
            } else {
                faultString = sanitize(exceptionToHandle.getMessage());
            }
            if ("".equals(faultString)) {
                faultString = sanitize(exceptionToHandle.getClass().getName());
            }            
            
            denormalizedMsg = factory11.createMessage();
            // Ensure we use QName to set the fault code in the envelope name space
            if (faultCode.indexOf(':') == -1) {
                String prefix = denormalizedMsg.getSOAPPart().getEnvelope().getPrefix();
                if (prefix == null) {
                    prefix = "";
                }
                faultCode = prefix + ":" + faultCode;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Populating fault with code: "
                        + faultCode + ", string: " + faultString);
            }

            SOAPBody body = denormalizedMsg.getSOAPBody();
            SOAPFault fault = body.addFault();
            fault.setFaultCode(faultCode);
            fault.setFaultString(faultString);
            if (faultActor != null) {
                fault.setFaultActor(faultActor);
            }
            if (faultDetail != null) {
                addFaultDetail(fault, faultDetail);
            }
            
            if (denormalizedMsg.saveRequired()) {
                denormalizedMsg.saveChanges();
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "HTTPBC-W00639.Failed_to_populate_SOAP_fault_with_exception", ex);
            // report internal server error
            throw new HTTPException(500);
        } 
            
        return denormalizedMsg;
    }   
    
    public Object denormalizeError(MessageExchange exchange, Object outlet) {
        return denormalizeError(exchange, outlet, null);
    }
    
    
    public Object denormalizeError(MessageExchange exchange, String faultDetail) {
	return denormalizeError(exchange, faultDetail, null);
    }
    
    private void addFaultDetail(SOAPFault fault, String faultDetail) throws SOAPException {
        Detail detail = fault.getDetail();
        if (detail == null) {
            detail = fault.addDetail();
        }

        DetailEntry entry = null;
        for (Iterator detailEntries = detail.getDetailEntries(); detailEntries.hasNext(); ) {
            DetailEntry e = (DetailEntry) detailEntries.next();
            QName entryName = e.getElementQName();
            if (FAULT_DETAIL_ENTRY_QNAME.equals(entryName)) {
                entry = e;
                break;
            }
        }
        if (entry == null) {
            entry = detail.addDetailEntry(FAULT_DETAIL_ENTRY_QNAME);
        }

        entry.addTextNode(faultDetail);
    }
    
    private String sanitize(Object val) {
        String result = "";
        if (val != null) {
            // Escape XML entities in the data because it will be made a part
            // of a SOAP message, which is itself in XML.
            result = StringUtil.escapeXML(val.toString());
        }
        return result;
    }

    public Object denormalizeError(MessageExchange exchange, Object targetToNormalizeTo, Endpoint endpoint) {
	
	boolean isSoap12 = isSoap12(endpoint);
	
	MessageFactory factory = factory11;
	if(isSoap12){
	    factory = factory12;
	}
	
	
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Populating SOAP reply with a fault for exchange " + exchange.getExchangeId() + " with error status.");
        }
       
        SOAPMessage denormalizedMsg = null;
        
        try {
            // Fault Code mandatory; default to "Server" if the exchange does not specify it.
            // In other words, the assumption is it's our fault, not the consumer.
            //String faultCode = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE));
            // the other endpoint may be soap1.1 or soap1.2, the fault needs be decided based on this endpoint
            String faultCode="";
            if ("".equals(faultCode)) {
                faultCode = isSoap12?"Receiver":"Server";
            }
            
            // Fault Actor mandatory; default to ourselves if the exchange does not specify it.
            // Default to our component name
            String faultActor = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR));
            if ("".equals(faultActor)) {
                faultActor = COMPONENT_NAME;
            }
            
            // Fault String mandatory; default to exception name if the exchange does not specify it.
            String faultString = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING));
            if ("".equals(faultString)) {
                String name = exchange.getError().getClass().getName();
                String message = exchange.getError().getLocalizedMessage();
                if (message == null || "".equals(message)) {
                    faultString = name;
                } else {
                    faultString = name + ": " + message;
                }
            }
            
            // Fault Detail is optional
            String faultDetail = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTDETAIL));
            
            // Ensure we use QName to set the fault code in the envelope name space
            denormalizedMsg = factory.createMessage();
            if (faultCode.indexOf(':') == -1) {
                String prefix = denormalizedMsg.getSOAPPart().getEnvelope().getPrefix();
                if (prefix == null) {
                    prefix = "";
                }
                faultCode = prefix + ":" + faultCode;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Populating fault with code: "
                        + faultCode + ", string: " + faultString);
            }

            SOAPBody body = denormalizedMsg.getSOAPBody();
            SOAPFault fault = body.addFault();
            fault.setFaultCode(faultCode);
            fault.setFaultString(faultString);
            fault.setFaultActor(faultActor);
            
            // Fault Detail is optional; if there is no detail,
            // do NOT populate an empty fault detail.
            if (!"".equals(faultDetail)) {
                addFaultDetail(fault, faultDetail);
            }
            
            if (denormalizedMsg.saveRequired()) {
                denormalizedMsg.saveChanges();
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "HTTPBC-W00639.Failed_to_populate_SOAP_fault_with_exception", ex);
            // report internal server error
            throw new HTTPException(500);
        } 
            
        return denormalizedMsg;
    }

    private boolean isSoap12(Endpoint endpoint) {
	if(endpoint == null){
	    return false;
	}
	
	if(endpoint instanceof HttpSoap12Endpoint){
	    return true;
	}
	return false;
    }

    public Object denormalizeError(MessageExchange exchange, String faultDetail, Endpoint endpoint) {
        
	boolean isSoap12 = isSoap12(endpoint);
	
	MessageFactory factory = factory11;
	if(isSoap12){
	    factory = factory12;
	}
	
	
	if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Populating SOAP reply with a fault for exchange " + exchange.getExchangeId() + " with error status.");
        }
       
        SOAPMessage denormalizedMsg = null;
        
        try {
            // Fault Code mandatory; default to "Server" if the exchange does not specify it.
            // In other words, the assumption is it's our fault, not the consumer.
            //String faultCode = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE));
            // the other endpoint may be soap1.1 or soap1.2, the fault needs be decided based on this endpoint
            String faultCode="";
            if ("".equals(faultCode)) {
                faultCode = isSoap12?"Receiver":"Server";
            }
            
            // Fault Actor mandatory; default to ourselves if the exchange does not specify it.
            // Default to our component name
            String faultActor = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR));
            if ("".equals(faultActor)) {
                faultActor = COMPONENT_NAME;
            }
            
            // Fault String mandatory; default to exception name if the exchange does not specify it.
            String faultString = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTSTRING));
            if ("".equals(faultString)) {
                String name = exchange.getError().getClass().getName();
                String message = exchange.getError().getLocalizedMessage();
                if (message == null || "".equals(message)) {
                    faultString = name;
                } else {
                    faultString = name + ": " + message;
                }
            }
            
            // Ensure we use QName to set the fault code in the envelope name space
            denormalizedMsg = factory.createMessage();
            if (faultCode.indexOf(':') == -1) {
                String prefix = denormalizedMsg.getSOAPPart().getEnvelope().getPrefix();
                if (prefix == null) {
                    prefix = "";
                }
                faultCode = prefix + ":" + faultCode;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Populating fault with code: "
                        + faultCode + ", string: " + faultString);
            }

            SOAPBody body = denormalizedMsg.getSOAPBody();
            SOAPFault fault = body.addFault();
            fault.setFaultCode(faultCode);
            fault.setFaultString(faultString);
            fault.setFaultActor(faultActor);
            
            // Fault Detail is optional; if there is no detail,
            // do NOT populate an empty fault detail.
            if (faultDetail != null && !"".equals(faultDetail)) {
                addFaultDetail(fault, faultDetail);
            } 
            
            if (denormalizedMsg.saveRequired()) {
                denormalizedMsg.saveChanges();
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, "HTTPBC-W00639.Failed_to_populate_SOAP_fault_with_exception", ex);
            // report internal server error
            throw new HTTPException(500);
        } 
            
        return denormalizedMsg;
    }
}
