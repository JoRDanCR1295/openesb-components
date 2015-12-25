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
 * @(#)HttpServletNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.servletsupport;

import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.servlet.http.HttpServletRequest;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;

import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.SoapNormalizer;
import com.sun.jbi.httpsoapbc.SoapNormalizerImpl;
import com.sun.jbi.internationalization.Messages;

/**
 * Takes a SOAP request from a http servlet request and populates a JBI Normalized Message with it.
 * Delegates the soap to normalized message conversion to <code>SoapNormalizer</code>
 */
public class HttpServletNormalizer implements Normalizer {

    private static final Messages mMessages =
        Messages.getMessages(HttpServletNormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(HttpServletNormalizer.class);
    
    static MessageFactory factory = null;
    
    SoapNormalizer soapNormalizer;
    
    static {
        try {
            factory = MessageFactory.newInstance();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Failed to initialize SOAP message factory. Can not convert incoming requests.");
        }
    }
    
    /** Creates a new instance of HttpSoapNormalizer */
    public HttpServletNormalizer() throws MessagingException {
        soapNormalizer = new SoapNormalizerImpl();
    }
    
    public SOAPMessage toSOAPMessage(Object inputToNormalize) throws MessagingException {
        SOAPMessage msg = null;
        try {
            HttpServletRequest request = (HttpServletRequest) inputToNormalize;

            java.io.InputStream is = request.getInputStream();
            MimeHeaders headers = getHeaders(request);
            msg = factory.createMessage(headers, is);

        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00603.Exception_normalize"), ex);    
        }            

        return msg;
    }
    
    public void setPropagateSoapHeader(boolean propagateSoapHeader) {
        // no-op
    }
    
    public NormalizedMessage normalize(Object inputToNormalize, MessageExchange messageExchange, OperationMetaData operationMetaData, MessageContext context) throws MessagingException {
        NormalizedMessage result = null;
        try {
            SOAPMessage soapInputToNormalize = (SOAPMessage) inputToNormalize;
            result = soapNormalizer.normalize(soapInputToNormalize, messageExchange, operationMetaData, true, context);
        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00603.Exception_normalize"),
                    (ex.getCause() != null ? ex.getCause() : ex));
        }            
        
        return result;
    }

    /**
     * Extract the headers from the request as mime headers
     */
    static MimeHeaders getHeaders(HttpServletRequest req) {
        Enumeration headerEnum = req.getHeaderNames();
        MimeHeaders headers = new MimeHeaders();
        
        while (headerEnum.hasMoreElements()) {
            String headerName = (String) headerEnum.nextElement();
            String headerValue = req.getHeader(headerName);
            
            StringTokenizer values = new StringTokenizer(headerValue, ",");
            
            while (values.hasMoreTokens()) {
                headers.addHeader(headerName, values.nextToken().trim());
            }
        }
        
        return headers;
    }
}
