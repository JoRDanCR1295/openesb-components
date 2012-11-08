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
 * @(#)HttpServletDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.servletsupport;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.servlet.http.HttpServletResponse;
import javax.xml.namespace.QName;
import javax.xml.soap.Detail;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeader;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;

import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.FaultException;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.SoapDenormalizer;
import com.sun.jbi.httpsoapbc.SoapDenormalizerImpl;
import com.sun.jbi.httpsoapbc.util.StringUtil;
import com.sun.jbi.internationalization.Messages;

/**
 * Takes JBI Normalized Message and populates a servlet response with a corresponding SOAP message.
 */
public class HttpServletDenormalizer implements Denormalizer {
    
    private static final Messages mMessages =
        Messages.getMessages(HttpServletDenormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(HttpServletDenormalizer.class);
    private static final QName FAULT_DETAIL_ENTRY_QNAME =
            new QName("detailText");
    private static final String COMPONENT_NAME = "sun-http-bc";

    static MessageFactory factory = null;
    
    SoapDenormalizer denormalizer;
    
    static {
        try {
            factory = MessageFactory.newInstance();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Failed to initialize SOAP message factory. Cannot convert incoming requests.", ex);
        }
    }
    
    /** Creates a new instance  */
    public HttpServletDenormalizer() {
        denormalizer = new SoapDenormalizerImpl();
    }
    
    public Object denormalize(NormalizedMessage messageToDenormalize, MessageExchange exchange, Object targetToNormalizeTo, OperationMetaData operationMetaData) throws MessagingException {
        
        try {
            HttpServletResponse resp = (HttpServletResponse) targetToNormalizeTo;
            
            // TODO: is there a condition where we want to send an empty response?
            boolean sendReply = true;
            if (sendReply) {
                SOAPMessage denormalizedMsg = denormalizer.denormalize(messageToDenormalize, exchange, operationMetaData, factory, false);
                
                if (denormalizedMsg.saveRequired()) {
                    denormalizedMsg.saveChanges();
                }
                
                putHeaders(denormalizedMsg.getMimeHeaders(), resp);

                // WS-I BP 1.0 R1126, set status to 500 for Fault
                if (denormalizedMsg.getSOAPBody().hasFault()) {
                    resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR); 
                } else {
                    resp.setStatus(HttpServletResponse.SC_OK);                
                }
                
                OutputStream os = resp.getOutputStream();
                denormalizedMsg.writeTo(os);
                os.flush();
                
            } else {
                OutputStream os = resp.getOutputStream();
                os.flush();                
                resp.setStatus(HttpServletResponse.SC_NO_CONTENT);
            }
            
        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00600.Exception_denormalize"),
                                         ex);
        }
        
        return targetToNormalizeTo;
    }
    
    public Object denormalizeException(Throwable exceptionToHandle, Object targetToNormalizeTo) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Populating SOAP reply with a fault for the following exception.", exceptionToHandle);
        }
        HttpServletResponse resp = (HttpServletResponse) targetToNormalizeTo;
        
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
            
            denormalizedMsg = factory.createMessage();
            // Ensure we use QName to set the fault code in the envelope name space
            if (faultCode.indexOf(':') == -1) {
                String prefix = denormalizedMsg.getSOAPPart().getEnvelope().getPrefix();
                if (prefix == null) {
                    prefix = "";
                }
                faultCode = prefix + ":" + faultCode;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Populating fault with code: " + faultCode + ", string: " + faultString);
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
            putHeaders(denormalizedMsg.getMimeHeaders(), resp);
            // WS-I BP 1.0, set status to 500 for Fault
            resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);                

            OutputStream os = resp.getOutputStream();
            denormalizedMsg.writeTo(os);
            os.flush();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "HTTPBC-E00601.Exception_denormalize_exception", ex);
            resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            try {
                OutputStream os = resp.getOutputStream();
                os.flush();            
            } catch (IOException ioEx) {
                // We'll continue and try to report the internal error
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "I/O Exception occured trying to flush HTTP response object", ioEx);
                }
            }
        } 
            
        return resp;
    }    
    
    public Object denormalizeError(MessageExchange exchange, Object outlet) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Populating SOAP reply with a fault for exchange " + exchange.getExchangeId() + " with error status.");
        }
        HttpServletResponse resp = (HttpServletResponse) outlet;
        
        SOAPMessage denormalizedMsg = null;
        
        try {
            // Fault Code mandatory; default to "Server" if the exchange does not specify it.
            // In other words, the assumption is it's our fault, not the consumer.
            String faultCode = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTCODE));
            if ("".equals(faultCode)) {
                faultCode = "Server";
            }
            
            // Fault Actor mandatory; default to ourselves if the exchange does not specify it.
            // Default to our component name
            String faultActor = sanitize(exchange.getProperty(Denormalizer.PROPERTY_ERROR_FAULTACTOR));
            if ("".equals(faultActor)) {
                faultActor = COMPONENT_NAME;
            }
            
            // Fault String mandatory; default to exception name and message if the exchange does not specify it.
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
            
            denormalizedMsg = factory.createMessage();
            // Ensure we use QName to set the fault code in the envelope name space
            if (faultCode.indexOf(':') == -1) {
                String prefix = denormalizedMsg.getSOAPPart().getEnvelope().getPrefix();
                if (prefix == null) {
                    prefix = "";
                }
                faultCode = prefix + ":" + faultCode;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Populating fault with code: " + faultCode + ", string: " + faultString);
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
            putHeaders(denormalizedMsg.getMimeHeaders(), resp);
            // WS-I BP 1.0, set status to 500 for Fault
            resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);                

            OutputStream os = resp.getOutputStream();
            denormalizedMsg.writeTo(os);
            os.flush();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "HTTPBC-E00601.Exception_denormalize_exception", ex);
            resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            try {
                OutputStream os = resp.getOutputStream();
                os.flush();            
            } catch (IOException ioEx) {
                // We'll continue and try to report the internal error
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "I/O Exception occured trying to flush HTTP response object", ioEx);
                }
            }
        } 
            
        return resp;
    }
    

    public Object denormalizeError(MessageExchange exchange, String faultDetail) {
        // no-op for servlet denormalizer
        return null;
    }
    
    /**
     * Convert the mime headers and add them to the HttpServletResponse.
     */    
    static void putHeaders(MimeHeaders headers, HttpServletResponse res) {
        Iterator it = headers.getAllHeaders();
        
        while (it.hasNext()) {
            MimeHeader header = (MimeHeader) it.next();
            
            String[] values = headers.getHeader(header.getName());
            
            if (values.length == 1) {
                res.setHeader(header.getName(), header.getValue());
            } else {
                StringBuffer concat = new StringBuffer();
                int i = 0;
                
                while (i < values.length) {
                    if (i != 0) {
                        concat.append(',');
                    }
                    
                    concat.append(values[i++]);
                }
                
                res.setHeader(header.getName(), concat.toString());
            }
        }
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
	// TODO Auto-generated method stub
	return null;
    }

    public Object denormalizeError(MessageExchange exchange, String faultDetail, Endpoint endpoint) {
	// TODO Auto-generated method stub
	return null;
    }
}
