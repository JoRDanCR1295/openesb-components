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
 * @(#)Normalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.handler.MessageContext;

/**
 *
 */
public interface Normalizer {
    /**
     * Convert the input to a SOAPMessage
     */
    SOAPMessage toSOAPMessage(Object inputToNormalize) throws MessagingException;    
    
    
    /**
     * Set the flag to indicate the propagation of SOAP headers
     */
    void setPropagateSoapHeader(boolean propagateSoapHeader);
    
    /**
     * Normalize a message into a JBI normalized message
     */
    NormalizedMessage normalize(Object inputToNormalize, MessageExchange messageExchange, OperationMetaData operationMetaData, MessageContext context) throws MessagingException;
}
