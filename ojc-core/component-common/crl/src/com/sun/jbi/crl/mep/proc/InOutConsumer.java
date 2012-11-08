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
 * @(#)InOutConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;

/**
 * Defines interface for fulfilling consumer responsibilities
 * for {@link InOut} exchanges.
 * 
 * @author Kevan Simpson
 */
public interface InOutConsumer {
    /**
     * Processes incoming message as a consumer and responds 
     * with a terminating status.
     * <p>
     * Specifically, the incoming message exchange must have its status 
     * ({@link MessageExchange#setStatus(javax.jbi.messaging.ExchangeStatus)})
     * set to {@link ExchangeStatus#DONE} or {@link ExchangeStatus#ERROR}
     * prior to sending the exchange via the NMR.
     * 
     * @param msg The incoming message exchange.
     * @param ctx The context of the exchange.
     * @throws JBIException if an error occurs during processing.
     */
    public void processOut(CRLInOut msg, ExchangeContext ctx) throws JBIException;
    
    /**
     * Processes incoming fault as a consumer and responds 
     * with a terminating status.
     * <p>
     * Specifically, the incoming message exchange must have its status 
     * ({@link MessageExchange#setStatus(javax.jbi.messaging.ExchangeStatus)})
     * set to {@link ExchangeStatus#DONE} or {@link ExchangeStatus#ERROR}
     * prior to sending the exchange via the NMR.
     * 
     * @param msg The incoming message exchange which contains a {@link Fault}.
     * @param ctx The context of the exchange.
     * @throws JBIException if an error occurs during processing.
     */
    public void processFault(CRLInOut msg, ExchangeContext ctx) throws JBIException;
}
