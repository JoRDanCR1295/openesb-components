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
 * @(#)InOnlyProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;

/**
 * Defines interface for fulfilling provider responsibilities
 * for {@link InOnly} exchanges.
 * 
 * @author Kevan Simpson
 */
public interface InOnlyProvider {
    /**
     * Processes incoming message as a provisioner and responds 
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
    public void processIn(CRLInOnly msg, ExchangeContext ctx) throws JBIException;
}
