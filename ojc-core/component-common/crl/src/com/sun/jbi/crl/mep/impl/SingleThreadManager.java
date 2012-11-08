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
 * @(#)SingleThreadManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.MessageListener;
import com.sun.jbi.crl.util.NDC;

/**
 * Invokes {@link MessageListener} in current thread.
 * 
 * @author Kevan Simpson
 */
public class SingleThreadManager extends AbstractThreadManager {
    
    public SingleThreadManager(ComponentContext ctx) {
    	super(ctx);
    }
    /** @see com.sun.jbi.crl.mep.ThreadManager#invoke(javax.jbi.messaging.MessageExchange, com.sun.jbi.crl.mep.ListenerContext) */
    public void invoke(MessageExchange msg, ManagerContext ctx) throws JBIException {
    	NDC ndc = null;
        try {
        	if (validateMessage(msg, ctx)) {
        		final ListenerContext listenerCtx = ctx.acquireListenerContext();
	        	// prepare NDC
	        	ServiceEndpoint endpt = msg.getEndpoint();
	        	ServiceUnit su = 
	        			listenerCtx.getEndpointManager().lookupServiceUnit(endpt);
	        	// TODO do we allow null SUs?
	        	if (su != null) {	// for now, avoid potential NullPointer...
	        		ndc = NDC.enter(su.getName(), msg.getExchangeId());
	        	}
	        	// process message exchange
	            MessageListener listener = ctx.getListenerFactory().getInstance();
	            listener.onMessage(msg, listenerCtx);
        	}
        }
        catch (Exception e) {
        	handleInvokeFailure(e);
        }
        finally {
            // exit NDC
            if (ndc != null) ndc.exit();
        }
    }
}
