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

package com.sun.jbi.component.toolkit.lifecycle.impl;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange.Role;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.I18n;

/**
 * Default implementation of {@link AcceptPoller}.
 * 
 * @author Kevan Simpson
 */
public class AcceptPoller implements Runnable {
    public static final String POLLER_COUNT_PROPERTY = "PollerCount";
    public static final Integer DEFAULT_POLLER_COUNT = Integer.valueOf(10);

    private static final long DEFAULT_ACCEPT_WAIT = 5000;
    
    private static int POLLER_INDEX = 0;
    
    private ManagerContext mContext= null;
    private long mAcceptWaitInterval = DEFAULT_ACCEPT_WAIT;
    private Logger mLogger;
    private String mName;   // cached
    
    // TODO should flag be volatile
    private boolean mRunning = false;
    private boolean mBlockingOnAccept;

    public AcceptPoller(ManagerContext ctx) {
        this(ctx, null);
    }

    public AcceptPoller(ManagerContext ctx, String name) {
        this(ctx, DEFAULT_ACCEPT_WAIT, name);
    }

    public AcceptPoller(ManagerContext ctx, long acceptWaitInterval) {
        this(ctx, acceptWaitInterval, null);
    }

    public AcceptPoller(ManagerContext ctx, long acceptWaitInterval, String name) {
        mContext = ctx;
        mAcceptWaitInterval = acceptWaitInterval;
        mLogger = Util.getLogger(ctx.getComponentContext(), AcceptPoller.class.getName());
        mName = (!Util.isEmpty(name)) ? name 
                : ctx.getComponentContext().getComponentName() +"-poller-"+ POLLER_INDEX++;
    }

    /**
     * Stops the poller from listening on the NMR.
     */
    public void stop() {
        mRunning = false;
        if (mBlockingOnAccept) {
            Thread.currentThread().interrupt();
        }
    }

    public long getAcceptWaitInterval() {
        return mAcceptWaitInterval;
    }
    
    public void setAcceptWaitInterval(long interval) {
        mAcceptWaitInterval = interval;
    }
    
    /** @see java.lang.Runnable#run() */
    public void run() {
        mRunning = true;
        MessagingChannel channel = null;
        // see if component stashed ThreadedExchangeHandler in correlation map
        ExchangeHandler handler = (ExchangeHandler) mContext
                .getCorrelationMap().get(ThreadedExchangeHandler.class.getName());
        if (handler == null) {
            handler = mContext.getExchangeHandler();
        }
        
        // log handler and classloader info
        if (log().isLoggable(Level.FINER)) {
            log().finer(I18n.format(
                    "COMPTK-2010: {0} is running with {1} using classloader: {2}",
                    this.toString(),
                    handler.getClass().getName(), 
                    String.valueOf(Thread.currentThread().getContextClassLoader())));
        }
        
        while (mRunning) {
            try {
                channel = mContext.getMessagingChannel();
                if (channel == null) {
                	log().info(I18n.loc(
                    		"COMPTK-5006: Failed to acquire delivery channel - may be closed!"));
                    break;
                }
                
                synchronized(this){
                    if(!mRunning){
                        break;
                    }
                    mBlockingOnAccept = true;
                }

                MessageExchange msg = channel.accept(getAcceptWaitInterval());
                
                synchronized(this){
                    mBlockingOnAccept = false;
                }

                if (msg != null) { // null msg means accept() timed out
                	// log msg, then process
                	if (log().isLoggable(Level.FINE)) {
                		String role = (Role.PROVIDER.equals(msg.getRole())) 
                				? "PROVIDER" : "CONSUMER";
                		log().log(Level.FINE,
                				  "COMPTK-3001: Message exchange accepted from NMR: id={0},role={1},status={2}",
                				  new Object[] { msg.getExchangeId(), role, msg.getStatus() });
                	}
                	
                	handler.handleExchange(msg);
                }
            }
            catch (MessagingException me) {
                String msg = I18n.loc("COMPTK-6015: An exception interrupted \"{1}\" NMR poller: {0}", 
                					  me.getMessage(), mContext.getComponentContext().getComponentName());
                // if channel is closed, it gets interrupted (TODO verify)
                if (me.getCause() instanceof InterruptedException) {
                	log().log(Level.INFO, msg);
                }
                else {
                	log().log(Level.WARNING, msg, me);
                }
            }
            catch (Exception e) {
                String msg = I18n.loc("COMPTK-6016: \"{0}\" NMR poller encountered error: {1}", 
                					  mContext.getComponentContext().getComponentName(), e.getMessage());
                log().log(Level.WARNING, msg, e);
            }
        }
    }

    /** @see java.lang.Object#toString() */
    @Override
    public String toString() {
        return mName;
    }

    protected Logger log() {
    	return mLogger;
    }
}
