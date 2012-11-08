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
 * @(#)JAXWSXmlHttpDenormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.httpsoapbc.Denormalizer;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.XmlDenormalizer;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;

import javax.activation.DataSource;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.ws.http.HTTPException;

/**
 * Takes JBI Normalized Message and populates a DataSource object for use with JAX-WS
 * This normalizer primarily delegates to XmlDenormalizer.
 */
public class JAXWSXmlHttpDenormalizer implements Denormalizer {
    
    private static final Messages mMessages =
        Messages.getMessages(JAXWSXmlHttpDenormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(JAXWSXmlHttpDenormalizer.class);
    
    XmlDenormalizer denormalizer;
    
    /** Creates a new instance  */
    public JAXWSXmlHttpDenormalizer() {
        denormalizer = new XmlDenormalizer();
    }
    
    public Object denormalize(NormalizedMessage messageToDenormalize, MessageExchange exchange, Object targetToNormalizeTo, OperationMetaData operationMetaData) throws MessagingException {
        
        DataSource denormalizedMsg = null;
        try {
            denormalizedMsg = denormalizer.denormalize(messageToDenormalize, exchange, operationMetaData, false);
        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00799.Denormalize_fail"),
                    (ex.getCause() != null ? ex.getCause() : ex));
        }
        
        return denormalizedMsg;
    }
    
    public Object denormalizeException(Throwable exceptionToHandle, Object targetToNormalizeTo) {
    	// no-op for HTTP denormalizer
        return null;
    }    
    
    public Object denormalizeError(MessageExchange exchange, Object targetToNormalizeTo) {
    	// no-op for HTTP denormalizer
        return null;
    }
    
    public Object denormalizeError(MessageExchange exchange, String faultDetail) {
        // no-op for HTTP denormalizer
        return null;
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
