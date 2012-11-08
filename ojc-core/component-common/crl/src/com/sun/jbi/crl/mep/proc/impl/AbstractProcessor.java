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
 * @(#)AbstractProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.proc.impl;

import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.crl.mep.ContextAware;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Abstract base class for {@link Processor} implementations.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractProcessor implements Processor, ContextAware {
    private transient Logger mLog = null;

    /** @see com.sun.jbi.crl.mep.proc.Processor#process(com.sun.jbi.crl.mep.exchange.CRLMessageExchange, com.sun.jbi.crl.mep.ExchangeContext) */
    public abstract void process(CRLMessageExchange msg, ExchangeContext ctx) throws JBIException;

    /** @see com.sun.jbi.crl.mep.ContextAware#registerContext(javax.jbi.component.ComponentContext) */
	public void registerContext(ComponentContext ctx) {
		if (mLog == null) {
			synchronized (this) {	// TODO need synchronization?
				mLog = LogUtil.getLogger(ctx, this.getClass().getName());
			}
		}
	}

	/** @see com.sun.jbi.crl.mep.ContextAware#unregisterContext() */
	public void unregisterContext() {
		// no op, no reference to context is held
	}

	/**
     * Fetches {@link Logger} for this {@link Processor}.
     * @return a non-<code>null</code> Logger.
     */
    protected Logger log() {
        return (mLog == null) ? Logger.getLogger(this.getClass().getName())
        					  : mLog;
    }
    
    /**
     * Logs message and creates {@link JBIException} when an exchange
     * cannot be processed by a {@link Processor} implementation.
     * 
     * @param me The invalid exchange.
     * @return A <code>JBIException</code> describing the invalidity.
     */
    protected JBIException invalidExchange(MessageExchange me) {
    	String msg;
    	if (me == null) {
    		msg = I18n.loc("CRL-6048: {0} received NULL message exchange! Aborting processing...",
    					   this.getClass().getSimpleName());
    	}
    	else {
    		msg = I18n.loc("CRL-6049: Invalid message exchange (id={0}) for {1}: cannot process {2}-{3} exchange!",
    					   me.getExchangeId(),
    					   this.getClass().getSimpleName(),
    					   String.valueOf(ExchangePattern.valueOf(me)),
    					   String.valueOf(me.getRole()));
    	}
    	
    	log().warning(msg);
    	return new JBIException(msg);
    }
}
