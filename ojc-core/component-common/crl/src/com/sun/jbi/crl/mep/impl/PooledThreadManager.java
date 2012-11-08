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
 * @(#)PooledThreadManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

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
 * Invokes {@link MessageListener} in separate thread.
 * 
 * @author Kevan Simpson
 */
public class PooledThreadManager extends AbstractThreadManager {
    // TODO manage pool size
    private ExecutorService mService = Executors.newFixedThreadPool(5);

    public PooledThreadManager(ComponentContext ctx) {
    	super(ctx);
    }
    
    /** @see com.sun.jbi.crl.mep.ThreadManager#invoke(javax.jbi.messaging.MessageExchange, com.sun.jbi.crl.mep.AcceptManager) */
    public void invoke(final MessageExchange msg, final ManagerContext ctx) throws JBIException {
        try {
            if (validateMessage(msg, ctx)) {
	            final MessageListener listener = ctx.getListenerFactory().getInstance();
	            final ListenerContext listenerCtx = ctx.acquireListenerContext();
	            Future<JBIException> future = mService.submit(new Callable<JBIException>() {
	                public JBIException call() {
	                	NDC ndc = null;
	                    try {
	                    	// prepare NDC
	                    	ServiceEndpoint endpt = msg.getEndpoint();
	                    	ServiceUnit su = 
	                    			listenerCtx.getEndpointManager().lookupServiceUnit(endpt);
	                    	ndc = NDC.enter(su.getName(), msg.getExchangeId());
	                    	// process message exchange
	                        listener.onMessage(msg, listenerCtx);
	                        return null;
	                    }
	                    catch (JBIException je) {
	                        // will log in outer catch block
	                        return je;
	                    }
	                    finally {
	                        // exit NDC
	                    	if (ndc != null) ndc.exit();
	                    }
	                }
	            });
	            
	            JBIException error = future.get();
	            if (error != null) {
	                throw error;
	            }
            }
        }
        catch (Exception e) {
        	handleInvokeFailure(e);
        }
    }
}
