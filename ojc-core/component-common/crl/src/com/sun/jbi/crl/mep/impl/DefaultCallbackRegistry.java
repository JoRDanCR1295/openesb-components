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
 * @(#)DefaultCallbackRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.crl.mep.Callback;
import com.sun.jbi.crl.mep.CallbackRegistry;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Registry for {@link Callback} instances.
 * 
 * @author Kevan Simpson
 */
public class DefaultCallbackRegistry implements CallbackRegistry {
    private static final long NEVER_TIMEOUT = -9999;
    
    Logger mLogger;
    private Map<Object, TimestampCallback> mCallbackMap = null;
    private CRLMessageExchangeFactory mExchangeFactory = null;
    
    public static DefaultCallbackRegistry createCallbackRegistry(ComponentContext ctx,
    															 CRLMessageExchangeFactory fac) {
        DefaultCallbackRegistry cbreg = new DefaultCallbackRegistry(ctx, fac);
        DefaultCallbackRegistry.CBDaemon daemon = cbreg.new CBDaemon();
        Thread t = new Thread(daemon);
        t.setDaemon(true);
        t.start();
        
        return cbreg;
    }
    
    protected DefaultCallbackRegistry(ComponentContext ctx, CRLMessageExchangeFactory fac) {
    	mLogger = LogUtil.getLogger(ctx, CallbackRegistry.class.getName());
        mExchangeFactory = fac;
        mCallbackMap = new HashMap<Object, TimestampCallback>();
    }
    
    /** @see com.sun.jbi.crl.mep.CallbackRegistry#invoke(java.lang.Object, java.lang.Object) */
    public CRLMessageExchange invoke(Object key, Object data) throws JBIException {
        if (key != null) {
            TimestampCallback tc = null;
            synchronized (mCallbackMap) {
                tc = mCallbackMap.remove(key);
            }
            
            if (tc != null) {
                if (tc.isTimedOut(System.currentTimeMillis())) {
                    return tc.getCallback().onTimeout();
                }
                else {
                    return tc.getCallback().onCallback(data);
                }
            }
        }
        // TODO else throw error?
        return null;
    }


    /** @see com.sun.jbi.crl.mep.CallbackRegistry#register(java.lang.Object, com.sun.jbi.crl.mep.Callback, long) */
    public void register(Object key, Callback cback, long timeout) {
        if (key != null && cback != null) {
            long expiration = (timeout <= 0) 
                    ? timeout : (System.currentTimeMillis() + timeout);
            cback.setExchangeFactory(mExchangeFactory);
            synchronized (mCallbackMap) {
                mCallbackMap.put(key, new TimestampCallback(cback, expiration));
            }
        }
    }
    
    /** @see com.sun.jbi.crl.mep.CallbackRegistry#register(java.lang.Object, com.sun.jbi.crl.mep.Callback) */
    public void register(Object key, Callback cback) {
        register(key, cback, NEVER_TIMEOUT);
    }
    
    public Callback lookup(Object key) {
        synchronized (mCallbackMap) {
            // removing because it should be completing the message exchange
            TimestampCallback tc = mCallbackMap.remove(key);
            if (tc != null) {
                return tc.getCallback();
            }
        }
        return null;
    }
    
    private class CBDaemon implements Runnable {
        private long mFrequency = 1000 * 60 * 5;// 5 minutes
        
        public CBDaemon() {
        }
        
        public void run() {
            while (true) {
                try { Thread.sleep(mFrequency); }
                catch (InterruptedException ie) {
                    mLogger.fine("CRL-3018: Daemon thread monitoring Callbacks has been interrupted: " 
                                 + ie.getMessage());
                }
                
                try {
                    checkForTimedOutCallbacks();
                }
                catch (Exception e) {
                    mLogger.log(Level.WARNING, 
                                I18n.loc("CRL-6036: Daemon callback thread has failed: {0}", 
                                	     e.getMessage()), 
                                e);
                }
            }
        }
        
        private void checkForTimedOutCallbacks() throws Exception {
            List<TimestampCallback> timedout = new ArrayList<TimestampCallback>();
            long currentTime = System.currentTimeMillis();
            
            synchronized (mCallbackMap) {
                // iterate through map, removing timed out entries
                for (Iterator<Object> iter = mCallbackMap.keySet().iterator(); iter.hasNext();) {
                    Object key = iter.next();
                    TimestampCallback tc = mCallbackMap.get(key);
                    if (tc.isTimedOut(currentTime)) {
                        timedout.add(tc);
                        iter.remove();
                    }
                }
            }   // release synchronization lock, timed out entries removed
            
            // invoke onTimeOut()
            if (!timedout.isEmpty()) {
                for (TimestampCallback tc : timedout) {
                    try {
                        tc.getCallback().onTimeout();
                    }
                    catch (Exception e) {
                        String msg = I18n.loc("CRL-6037: Callback onTimeout failed: {0}", 
                        					e.getMessage());
                        mLogger.log(Level.WARNING, msg, e);
                        throw new JBIException(msg, e);
                    }
                }
            }
        }
    }
    
    private static class TimestampCallback {
        private Callback mCallback = null;
        private long mExpiration = -1;
        
        public TimestampCallback(Callback cback, long expiration) {
            mCallback = cback;
            mExpiration = expiration;
        }
        
        public Callback getCallback() { 
            return mCallback;
        }
        public boolean isTimedOut(long currentTime) {
            return (mExpiration > 0 && mExpiration <= currentTime);
        }
    }
}
