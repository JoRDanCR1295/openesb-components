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
 * @(#)Denormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;

/**
 *
 */
public interface Denormalizer {
    public static final String PROPERTY_ERROR_FAULTCODE = "com.sun.jbi.crl.faultcode";
    public static final String PROPERTY_ERROR_FAULTACTOR = "com.sun.jbi.crl.faultactor";
    public static final String PROPERTY_ERROR_FAULTSTRING = "com.sun.jbi.crl.faultstring";
    public static final String PROPERTY_ERROR_FAULTDETAIL = "com.sun.jbi.crl.faultdetail";
    
    Object denormalize(NormalizedMessage messageToDenormalize, MessageExchange messageExchange, 
            Object targetToNormalizeTo, OperationMetaData operationMetaData) throws MessagingException;
    
    Object denormalizeException(Throwable exceptionToHandle, Object targetToNormalizeTo);
    
    Object denormalizeError(MessageExchange exchange, Object targetToNormalizeTo);
    
    Object denormalizeError(MessageExchange exchange, Object targetToNormalizeTo,Endpoint endpoint);
    
    Object denormalizeError(MessageExchange exchange, String faultDetail);
    
    Object denormalizeError(MessageExchange exchange, String faultDetail,Endpoint endpoint);
}
