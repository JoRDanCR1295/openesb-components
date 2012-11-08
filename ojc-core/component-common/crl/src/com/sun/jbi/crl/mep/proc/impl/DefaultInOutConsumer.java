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
 * @(#)DefaultInOutConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.proc.InOutConsumer;
import com.sun.jbi.crl.util.I18n;

/**
 * Default implementation of {@link InOutConsumer}.
 * 
 * @author Kevan Simpson
 */
public class DefaultInOutConsumer extends AbstractInOutConsumer {
    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processFault(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processFault(CRLInOut msg, 
                             ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5012: {0} received FAULT for exchange: {2}", 
							this.getClass().getSimpleName(), msg.getExchangeId()));
        // reply to consumer
        msg.send();
    }

    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer#processOut(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public void processOut(CRLInOut msg,
                           ExchangeContext ctx) throws JBIException {
    	log().info(I18n.loc("CRL-5013: {0} received OUT message for exchange: {2}", 
							this.getClass().getSimpleName(), msg.getExchangeId()));

        msg.setStatus(ExchangeStatus.DONE);
        msg.send();
    }
}
