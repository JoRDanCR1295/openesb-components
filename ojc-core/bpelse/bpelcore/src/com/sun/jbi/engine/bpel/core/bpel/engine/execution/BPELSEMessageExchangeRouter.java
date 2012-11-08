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
 * @(#)BPELSEMessageExchangeRouter.java
 *
 * Copyright 2004-2011 Open ESB Community
 *
 * END_HEADER - DO NOT EDIT
 */
/**
 *
 * @author Alexander Lomov
 */
package com.sun.jbi.engine.bpel.core.bpel.engine.execution;

import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.component.toolkit.util.I18n;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;

public class BPELSEMessageExchangeRouter implements Runnable {

    private static final Logger LOGGER = Logger.getLogger(BPELSEMessageExchangeRouter.class.getName());
    private MessageExchange mex;

    public BPELSEMessageExchangeRouter(MessageExchange exchange) {
        if (exchange == null) {
            throw new RuntimeException("MessageExchange cannot be null");
        }
        mex = exchange;
    }

    public void run() {
        //<editor-fold defaultstate="collapsed" desc="FINE level logging">
        if (LOGGER.isLoggable(Level.FINE)) {
            LOGGER.log(Level.FINE, I18n.loc("BPJBI-3002: Pattern for exchange Id {0} is {1}", mex.getExchangeId(), mex.getPattern()));
        }
        //</editor-fold>

        Direction dir = getDirection(mex);
        ExchangePattern ep = ExchangePattern.valueOf(mex);

        /**/


    }

    private Direction getDirection(MessageExchange mex) {
        if (ExchangePattern.isInOut(mex) && (((InOut) mex).getOutMessage() != null || mex.getFault() != null)) {
            return Direction.OUT;
        } else if ((ExchangePattern.isInOnly(mex) && ((InOnly) mex).getInMessage() != null) ||
                (ExchangePattern.isInOut(mex) && ((InOut) mex).getInMessage() != null)) {
            return Direction.IN;
        } else {
            return Direction.UNKNOWN;
        }
    }

    private enum Direction {

        IN, OUT, UNKNOWN
    }
}
