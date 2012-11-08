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
 * @(#)DefaultAcceptPoller.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange.Role;

import com.sun.jbi.crl.mep.AcceptPoller;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.ThreadManager;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Default implementation of {@link AcceptPoller}.
 * 
 * @author Kevan Simpson
 */
public class DefaultAcceptPoller implements AcceptPoller {
    private static final long DEFAULT_ACCEPT_WAIT = 5000;
    
    private ManagerContext mContext= null;
    private long mAcceptWaitInterval = DEFAULT_ACCEPT_WAIT;
    private Logger mLogger;
    
    // TODO should flag be volatile
    private boolean mRunning = false;
    
    public DefaultAcceptPoller(ManagerContext ctx) {
        this(ctx, DEFAULT_ACCEPT_WAIT);
    }
    
    public DefaultAcceptPoller(ManagerContext ctx, long acceptWaitInterval) {
        mContext = ctx;
        mAcceptWaitInterval = acceptWaitInterval;
        mLogger = LogUtil.getLogger(ctx, AcceptPoller.class.getName());
    }
    
    /** @see com.sun.jbi.crl.mep.AcceptPoller#stop() */
    public void stop() throws JBIException {
        mRunning = false;
    }

    public long getAcceptWaitInterval() {
        return mAcceptWaitInterval;
    }
    
    /** @see java.lang.Runnable#run() */
    public void run() {
        mRunning = true;
        ThreadManager threadMgr = mContext.getThreadManager();
        DeliveryChannel channel = null;

        while (mRunning) {
            try {
                channel = mContext.getDeliveryChannel();
                if (channel == null) {
                	log().info(I18n.loc(
                    		"CRL-5009: Failed to acquire delivery channel - may be closed!"));
                    break;
                }
                
                MessageExchange msg = channel.accept(getAcceptWaitInterval());
                if (msg != null) { // null msg means accept() timed out
                	// log msg, then process
                	if (log().isLoggable(Level.FINE)) {
                		String role = (Role.PROVIDER.equals(msg.getRole())) 
                				? "PROVIDER" : "CONSUMER";
                		log().log(Level.FINE, 
                				    "CRL-3021: Message exchange accepted from NMR: id={0},role={1},status={2}",
                					new Object[] { msg.getExchangeId(), role, msg.getStatus() });
                	}
                    threadMgr.invoke(msg, mContext);
                }
            }
            catch (MessagingException me) {
                String msg = I18n.loc("CRL-6034: An exception interrupted \"{1}\" NMR poller: {0}", 
                					  me.getMessage(), mContext.getComponentName());
                // if channel is closed, it gets interrupted (TODO verify)
                if (me.getCause() instanceof InterruptedException) {
                	log().log(Level.INFO, msg);
                }
                else {
                	log().log(Level.WARNING, msg, me);
                }
            }
            catch (Exception e) {
                String msg = I18n.loc("CRL-6035: \"{0}\" NMR poller encountered error: {1}", 
                					  mContext.getComponentName(), e.getMessage());
                log().log(Level.WARNING, msg, e);
            }
        }
    }

    public void cease() {
        mRunning = false;
    }
    
    protected Logger log() {
    	return mLogger;
    }
}
