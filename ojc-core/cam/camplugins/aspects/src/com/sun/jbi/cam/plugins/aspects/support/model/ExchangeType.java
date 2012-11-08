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
 * @(#)ExchangeType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model;

/**
 * @author graj
 *
 */
public enum ExchangeType {
    FilterRequestReply("filterRequestReply"), 
    FilterOneWay("filterOneWay"), 
    RequestReply("requestReply");

    String exchangeType;

    /** @param protocolString */
    private ExchangeType(String exchangeTypeString) {
        this.exchangeType = exchangeTypeString;
    }

    /** @return the protocol */
    public String getExchangeType() {
        return exchangeType;
    }

    /**
     * 
     * @param aString
     * @return
     */
    public static ExchangeType convert(String aString) {
        ExchangeType type = null;
        if("filterRequestReply".equals(aString)) {
            type = FilterRequestReply;
        }
        if("filterOneWay".equals(aString)) {
            type = FilterOneWay;
        }
        if("requestReply".equals(aString)) {
            type = RequestReply;
        }
        return type;
    }
    
    /** @return the protocol */
    public String getDescription() {
        switch (this) {
        case FilterRequestReply:
            return "Filter Request Reply";
        case FilterOneWay:
            return "Filter One-Way";
        case RequestReply:
            return "Request-Reply";
        default:
            return "Unknown";
        }
    }

}
