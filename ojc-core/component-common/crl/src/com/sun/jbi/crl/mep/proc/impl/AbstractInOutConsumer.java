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
 * @(#)AbstractInOutConsumer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import javax.jbi.JBIException;

import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.proc.InOutConsumer;

/**
 * Abstract base of {@link InOutConsumer}.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractInOutConsumer extends AbstractProcessor 
                                            implements InOutConsumer {
    
    /** @see com.sun.jbi.crl.mep.proc.impl.AbstractProcessor#process(com.sun.jbi.crl.mep.exchange.CRLMessageExchange, com.sun.jbi.crl.mep.ExchangeContext) */
    public void process(CRLMessageExchange msg, ExchangeContext ctx) throws JBIException {
        if (msg instanceof CRLInOut) {
            CRLInOut view = (CRLInOut) msg;
            if (msg.getFault() != null) {
                processFault(view, ctx);
            }
            else {
                processOut(view, ctx);
            }
            
            return;
        }
        else {
        	throw invalidExchange(msg);
        }
    }

    /** @see com.sun.jbi.crl.mep.proc.InOutConsumer#processFault(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public abstract void processFault(CRLInOut msg, 
                                      ExchangeContext ctx) throws JBIException;

    /** @see com.sun.jbi.crl.mep.proc.InOutConsumer#processOut(com.sun.jbi.crl.mep.exchange.CRLInOut, com.sun.jbi.crl.mep.ExchangeContext) */
    public abstract void processOut(CRLInOut msg, 
                                    ExchangeContext ctx) throws JBIException;
}