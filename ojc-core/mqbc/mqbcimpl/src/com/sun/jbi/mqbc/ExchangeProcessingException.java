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
 */

/*
 * @(#)$Id: ExchangeProcessingException.java,v 1.1 2008/12/10 21:54:51 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * Exception raised for message exchange processing events, that present fault
 * message/detail information.
 *
 * @author Noel.Ang@sun.com
 */
public final class ExchangeProcessingException
        extends MessagingException {

    public ExchangeProcessingException(MessageExchange exchange,
                                       String faultCode,
                                       String message,
                                       String faultDetail,
                                       Throwable cause) {
        super(message, cause);
        this.faultCode = faultCode;
        this.exchange = exchange;
        this.faultDetail = clean(faultDetail);

        if (exchange == null) {
            throw new NullPointerException("exchange");
        }
    }

    public ExchangeProcessingException(MessageExchange exchange,
                                       String faultCode,
                                       String message,
                                       String faultDetail) {
        super(message);
        this.faultCode = faultCode;
        this.exchange = exchange;
        this.faultDetail = clean(faultDetail);

        if (exchange == null) {
            throw new NullPointerException("exchange");
        }
    }

    public MessageExchange getMessageExchange() {
        return exchange;
    }
    
    public String getFaultCode() {
        return faultCode;
    }

    public String getFaultMessage() {
        return getLocalizedMessage();
    }

    public String getFaultDetail() {
        return faultDetail;
    }

    private String clean(String value) {
        return (value != null ? value.trim() : "");
    }

    private final MessageExchange exchange;
    private final String faultDetail;
    private final String faultCode;
}
