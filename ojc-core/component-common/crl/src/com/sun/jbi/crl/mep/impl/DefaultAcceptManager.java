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
 * @(#)DefaultAcceptManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import com.sun.jbi.component.config.PollerConfigMBean;
import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.AcceptPoller;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.MessageListenerFactory;
import com.sun.jbi.crl.mep.ThreadManager;
import com.sun.jbi.crl.util.I18n;

/**
 * Default implementation of {@link AcceptManager}.
 * 
 * @author Kevan Simpson
 */
public class DefaultAcceptManager implements AcceptManager {
    private static final int DEFAULT_POLLER_COUNT = 5;
    
    private ComponentManager mComponentMgr = null;
    private ListenerContext mContext = null;
    private ExchangeRouter mRouter = null;
    private MessageListenerFactory mListenerFactory = null;
    private ThreadManager mThreadManager = null;
    private int mPollerCount = DEFAULT_POLLER_COUNT;
    private Set<AcceptPoller> mPollerSet = null;
    private Logger mLogger;
//    private PollerConfigMBean mConfigMBean;
    
    public DefaultAcceptManager(ComponentManager cmgr) {
        this(cmgr, null);
    }
    
    public DefaultAcceptManager(ComponentManager cmgr, PollerConfigMBean mbean) {
        setComponentManager(cmgr);
        mLogger = Logger.getLogger(AcceptManager.class.getName());
        // use default/backup values by passing null
        setExchangeRouter(null);
        setListenerFactory(null);
        setThreadManager(null);
        setListenerContext(null);
        setPollerSet(null);
        
        if (mbean != null) {
//            mConfigMBean = mbean;
            // hasn't yet been added as listener
            setPollerCount(mbean.getPollerCount().intValue());
        }
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#getComponentManager() */
    public ComponentManager getComponentManager() {
        return mComponentMgr;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#getExchangeRouter() */
    public ExchangeRouter getExchangeRouter() {
        return mRouter;
    }
    
    /** @see com.sun.jbi.crl.mep.AcceptManager#getListenerContext() */
    public ListenerContext getListenerContext() {
        return mContext;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#getListenerFactory() */
    public MessageListenerFactory getListenerFactory() {
        return mListenerFactory;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#getPollerCount() */
    public int getPollerCount() {
        return mPollerCount;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#getThreadManager() */
    public ThreadManager getThreadManager() {
        return mThreadManager;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setComponentManager(com.sun.jbi.component.lifecycle.ComponentManager) */
    public void setComponentManager(ComponentManager cmgr) {
        mComponentMgr = cmgr;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setExchangeRouter(com.sun.jbi.crl.mep.ExchangeRouter) */
    public void setExchangeRouter(ExchangeRouter router) {
        mRouter = (router == null) ? new PatternExchangeRouter(true) : router;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setListenerContext(com.sun.jbi.crl.mep.ListenerContext) */
    public void setListenerContext(ListenerContext ctx) {
        mContext = (ctx == null) 
                        ? new DefaultListenerContext(getComponentManager()) : ctx;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setListenerFactory(com.sun.jbi.crl.mep.MessageListenerFactory) */
    public void setListenerFactory(MessageListenerFactory factory) {
        mListenerFactory = (factory == null) 
                        ? new DefaultMessageListenerFactory(
                        		getComponentManager().getComponentContext()) 
        			 	: factory;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setPollerCount(int) */
    public void setPollerCount(int count) {
        if (count <= 0) {
            // TODO handle in some way (i.e. PropertyChangeEvent?)
            return;
        }
        mPollerCount = count;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#setThreadManager(com.sun.jbi.crl.mep.ThreadManager) */
    public void setThreadManager(ThreadManager mgr) {
        mThreadManager = (mgr == null) 
        		? new SingleThreadManager(getComponentManager().getComponentContext()) : mgr;
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#startAccepting() */
    public void startAccepting() throws JBIException {
        boolean logFiner = log().isLoggable(Level.FINER);
        String compName = getComponentManager().getComponentContext().getComponentName();

        try {
        	// start fresh
            getPollerSet().clear();
            for (int i = 0, I = getPollerCount(); i < I; i++) {
                AcceptPoller ap = new DefaultAcceptPoller(getComponentManager().getManagerContext());
                getPollerSet().add(ap);
                (new Thread(ap)).start();
                
                if (logFiner) {
                	log().finer("CRL-2013: NMR poller-"+ i +" started for "+ compName);
                }
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("CRL-6032: Failed to start \"{0}\" NMR pollers: {1}", 
            					  compName, e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#stopAccepting() */
    public void stopAccepting() throws JBIException {
        boolean logFiner = log().isLoggable(Level.FINER);
        String compName = getComponentManager().getComponentContext().getComponentName();
        for (AcceptPoller ap : mPollerSet) {
            // give each poller chance to stop, regardless of errors
            try {
                ap.stop();
                if (logFiner) {
                    log().finer("CRL-2010: NMR poller stopped for "+ compName);
                }
            }
            catch (Exception in) {
                log().log(Level.WARNING,
                		  I18n.loc("CRL-6033: Failed to stop \"{0}\" NMR poller: {1}", 
                				   compName, in.getMessage()),
                          in);
            }
        }
    }

    /** @see com.sun.jbi.crl.mep.AcceptManager#shutDown() */
    public void shutDown() throws JBIException {
        getPollerSet().clear();
    }
    
    protected Logger log() {
        return mLogger;
    }
    
    protected Set<AcceptPoller> getPollerSet() {
        return mPollerSet;
    }
    protected void setPollerSet(Set<AcceptPoller> set) {
        mPollerSet = (set == null) ? new HashSet<AcceptPoller>() : set;
    }
}
