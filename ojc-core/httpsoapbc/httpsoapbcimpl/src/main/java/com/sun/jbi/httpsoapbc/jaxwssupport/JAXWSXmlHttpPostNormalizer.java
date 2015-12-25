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
 * @(#)JAXWSXmlHttpPostNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.jaxwssupport;

import com.sun.jbi.httpsoapbc.Normalizer;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import com.sun.jbi.httpsoapbc.XmlPostNormalizer;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;

/**
 * Takes a JBI Provider Source object and populates a JBI Normalized Message with it.
 */
public class JAXWSXmlHttpPostNormalizer implements Normalizer {

    private static final Messages mMessages =
        Messages.getMessages(JAXWSXmlHttpPostNormalizer.class);
    private static final Logger mLogger =
        Messages.getLogger(JAXWSXmlHttpPostNormalizer.class);
    
    private final XmlPostNormalizer mDelegate;
    
    /** Creates a new instance */
    public JAXWSXmlHttpPostNormalizer() throws MessagingException {
        mDelegate = new XmlPostNormalizer();
    }

    public SOAPMessage toSOAPMessage(Object inputToNormalize) throws MessagingException {
        throw new MessagingException("toSOAPMessage call unsupported by implementation");
    }
    
    public void setPropagateSoapHeader(boolean propagateSoapHeader) {
        // no-op
    }
    
    public NormalizedMessage normalize(Object inputToNormalize, MessageExchange messageExchange, OperationMetaData operationMetaData, MessageContext context) throws MessagingException {
        NormalizedMessage result = null;
        try {
            result = mDelegate.normalize(inputToNormalize, messageExchange, operationMetaData, true, context);
        } catch (Exception ex) {
            throw new MessagingException(mMessages.getString("HTTPBC-E00798.Normalize_fail"),
                    (ex.getCause() != null ? ex.getCause() : ex));
        }            
        
        return result;
    }
}
