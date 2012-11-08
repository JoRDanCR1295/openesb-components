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
 * @(#)DefaultMessageListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.component.alerter.Alerter;
import com.sun.jbi.component.alerter.AlerterFactory;
import com.sun.jbi.component.alerter.NotificationEvent.OperationalState;
import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.ContextAware;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.MessageListener;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.proc.Processor;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Default {@link MessageListener} implementation.
 * 
 * @author Kevan Simpson
 */
public class DefaultMessageListener implements MessageListener {
	private String mCompName;
    private Logger mLogger;
    private Alerter mAlerter;
    
    // requires generic constructor for use in DefaultMessageListenerFactory
    public DefaultMessageListener(ComponentContext ctx) {
    	this(ctx, MessageListener.class.getName());
    }
    
    protected DefaultMessageListener(ComponentContext ctx, String logger) {
    	mLogger = LogUtil.getLogger(ctx, logger);
    	mCompName = ctx.getComponentName();
    	mAlerter = AlerterFactory.newAlerter();
    }
    
    /** @see com.sun.jbi.crl.mep.MessageListener#onMessage(javax.jbi.messaging.MessageExchange, com.sun.jbi.crl.mep.ListenerContext) */
    public void onMessage(MessageExchange msg, ListenerContext ctx) throws JBIException {
        try {
            ExchangeRouter router = ctx.getExchangeRouter();
            ExchangeRouter.RouterKey key = router.toKey(msg);
            Processor proc = router.getProcessor(key);
            if (proc == null) {
                throw new JBIException(I18n.loc(	// logged below...
                		"CRL-6038: A processor was not configured for router key: {0}", 
                		String.valueOf(key)));
            }
            else if (proc instanceof ContextAware) {
            	((ContextAware) proc).registerContext(ctx);
            }
            
            CRLMessageExchange crlme = 
                    ctx.getCRLMessageExchangeFactory().createExchange(msg);
            // lookup and pass callback with exchange
            Callback cback = router.getCallback(key);
            crlme.setCallback(cback);
            // responsible for sending response, if any
            proc.process(crlme, ctx.acquireExchangeContext());
            
            if (proc instanceof ContextAware) {
            	((ContextAware) proc).unregisterContext();
            }
        }
        catch (Exception e) {
            handleError(I18n.loc("CRL-6039: Failed to handle message exchange: {0}", 
            					 e.getMessage()),
                        e);
        }
    }

    protected String getComponentName() {
    	return mCompName;
    }
    
    protected Alerter getAlerter() {
    	return mAlerter;
    }
    
    protected void handleError(String msg, Exception e) throws JBIException {
    	getAlerter().critical(msg, getComponentName(), null, null, null, 
    						  OperationalState.RUNNING, null);
        if (e == null) {
            log().warning(msg);
            throw new JBIException(msg);
        }
        else if (e instanceof JBIException) {
            log().log(Level.WARNING, msg, e);
            throw ((JBIException) e);
        }
        else {
            log().log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }
    }
        
    protected Logger log() {
        return mLogger;
    }
}
