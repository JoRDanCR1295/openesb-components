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
 * @(#)ExchangePattern.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.RobustInOnly;

/**
 * Enum for message exchange patterns.
 * 
 * @author Kevan Simpson
 */
public enum ExchangePattern {
    IN_ONLY("http://www.w3.org/2006/01/wsdl/in-only"),
    IN_OPTIONAL_OUT("http://www.w3.org/2006/01/wsdl/in-opt-out"),
    IN_OUT("http://www.w3.org/2006/01/wsdl/in-out"),
    ROBUST_IN_ONLY("http://www.w3.org/2006/01/wsdl/robust-in-only"),
    CUSTOM("custom-exhange-pattern-uri");
    
    private String mPattern = null;
    
    ExchangePattern(String pattern) { 
        mPattern = pattern;
    }
    
    public String getPattern() { 
        return mPattern; 
    }
    
    public static ExchangePattern valueOf(MessageExchange mex) {
        if (mex instanceof InOnly) {
            return IN_ONLY;
        }
        else if (mex instanceof InOut) {
            return IN_OUT;
        }
        else if (mex instanceof InOptionalOut) {
            return IN_OPTIONAL_OUT;
        }
        else if (mex instanceof RobustInOnly) {
            return ROBUST_IN_ONLY;
        }
        else {
            return CUSTOM;
        }
    }
    
    public static boolean isInOnly(MessageExchange exchange) {
        return exchange instanceof InOnly;
    }

    public static boolean isInOut(MessageExchange exchange) {
        return exchange instanceof InOut;
    }

    public static boolean isRobustInOnly(MessageExchange exchange) {
        return exchange instanceof RobustInOnly;
    }

    public static boolean isInOptionalOut(MessageExchange exchange) {
        return exchange instanceof InOptionalOut;
    }

}
