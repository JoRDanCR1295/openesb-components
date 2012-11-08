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
 * @(#)ErrorMessager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime.messages;

import java.util.HashMap;
import java.util.Map;

/**
 * System for logging errors. 
 *
 * @author nang
 * @version $Revision: 1.1 $
 */
public abstract class ErrorManager
{
    /**
     * Create an ErrorManager instance.
     */ 
    protected ErrorManager()
    {
    }
    
    /**
     * Obtain an ErrorManager for a given subsystem.  A unique ErrorManager
     * instance is created for each unique subsystem, and is cached.
     *
     * @param name Subsystem identification
     *
     * @return a suitable instance of ErrorManager
     */ 
    public static ErrorManager getManager(Object name)
    {
        if (name == null) {
            name = "";
        }
        
        synchronized (mErrorManagers) {
            ErrorManager mgr = lookupMgr(name);
            if (mgr == null) {
                //Not good reference design time stuff here
                //if (isUsingNetBeans()) {
                //    mgr = new NetBeansErrorManager(name);
                //}
                //else {
                mgr = new DefaultErrorManager(name);
                //}
                mErrorManagers.put(name, mgr);
            }
            return mgr;
        }
    }

    private static ErrorManager lookupMgr(Object name)
    {
        synchronized (mErrorManagers) {
            return (ErrorManager) mErrorManagers.get(name);
        }
    }

    /**
     * Log an event and display a notification in the primary interface.  
     * <p/>
     * First, the method maps the specified severity is mapped to a
     * (implementation-native) value using {@link #mapSeverityToNativeLevel}.
     * <p/>
     * Then, if <code>msg</code> was specified, it is dispatched thru
     * {@link #postNotification}. If <code>msg</code> is null or blank, the call
     * fails quietly.
     *
     * @param sev  Severity of the message
     * @param t    Optional detail about the event
     * @param msg  Message to post
     */ 
    public void post(ErrorManager.Severity sev,
                     Throwable t,
                     String msg)
    {
        final Object level = mapSeverityToNativeLevel(sev);
        
        if (msg != null && !"".equals(msg)) {
            postNotification(level, msg, t);
        }
    }
    
    /**
     * Log an event, but only log; do not echo to the primary interface.
     * <p/>
     * First, the method maps the specified severity is mapped to a
     * (implementation-native) value using {@link #mapSeverityToNativeLevel}.
     * <p/>
     * Then, if <code>msg</code> was specified, it is dispatched thru
     * {@link #logNotification}. If <code>msg</code> is null or blank, the call
     * fails quietly.
     *
     * @param sev  Severity of the message
     * @param t    Optional detail about the event
     * @param msg  Message to post
     */ 
    public void log(ErrorManager.Severity sev,
                    Throwable t,
                    String msg)
    {
        final Object level = mapSeverityToNativeLevel(sev);
        
        if (msg != null && !"".equals(msg)) {
            logNotification(level, msg, t);
        }
    }
    
    /**
     * Send a notification to the primary interface and record it into the log.
     *
     * @param level Translated level value computed by
     *              {@link #mapSeverityToNativeLevel}
     * @param msg   Message to log
     * @param t     Optional detail
     */ 
    protected abstract void postNotification(Object level,
                                             String msg,
                                             Throwable t);
    
    /**
     * Record to log but do not send a notification to the primary interface.
     *
     * @param level Translated level value computed by
     *              {@link #mapSeverityToNativeLevel}
     * @param msg   Message to log
     * @param t     Optional detail
     */ 
    protected abstract void logNotification(Object level,
                                            String msg,
                                            Throwable t);
    
    /**
     * Map an ErrorManager severity to an equivalent value in the implementation
     * context. The type of object returned is implementation-defined, because
     * its use is also implementation-defined.
     */ 
    protected abstract Object mapSeverityToNativeLevel(Severity sev);

    public static class Severity
    {
        private Severity()
        {
        }

        public static final Severity ERROR = new Severity();
        public static final Severity WARN = new Severity();
        public static final Severity INFO = new Severity();
        public static final Severity DEBUG = new Severity();
    }
    
    private static final Map<Object, ErrorManager> mErrorManagers =
        new HashMap<Object, ErrorManager>(); 
}

// FINIS $RCSfile: ErrorManager.java,v $
