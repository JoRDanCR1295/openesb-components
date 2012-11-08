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

package com.sun.jbi.nms.exchange;

import javax.jbi.messaging.MessageExchange;

/**
 *
 *
 * @version      
 *
 */
public enum ExchangePattern {

    IN_ONLY,
    IN_OPTIONAL_OUT,
    IN_OUT,
    ROBUST_IN_ONLY,
    CUSTOM;

    public static ExchangePattern valueOf(MessageExchange exchange)
    {
        if (exchange instanceof javax.jbi.messaging.InOnly) {
            return IN_ONLY;
        } else if (exchange instanceof javax.jbi.messaging.InOut) {
            return IN_OUT;
        } else if (exchange instanceof javax.jbi.messaging.RobustInOnly) {
            return ROBUST_IN_ONLY;
        } else if (exchange instanceof javax.jbi.messaging.InOptionalOut) {
            return IN_OPTIONAL_OUT;
        } else {
            return CUSTOM;
        }
    }

    public static boolean isInOnly(MessageExchange exchange) {
        return exchange instanceof javax.jbi.messaging.InOnly;
    }

    public static boolean isInOut(MessageExchange exchange) {
        return exchange instanceof javax.jbi.messaging.InOut;
    }

    public static boolean isRobustInOnly(MessageExchange exchange) {
        return exchange instanceof javax.jbi.messaging.RobustInOnly;
    }

    public static boolean isInOptionalOut(MessageExchange exchange) {
        return exchange instanceof javax.jbi.messaging.InOptionalOut;
    }
}
