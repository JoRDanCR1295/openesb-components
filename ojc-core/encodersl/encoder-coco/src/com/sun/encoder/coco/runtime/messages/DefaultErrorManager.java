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
 * @(#)DefaultErrorManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime.messages;

import java.awt.Window;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

/**
 * ErrorManager implementation that offers standard Java-based facilities
 * (based on [java.*] libraries). 
 *
 * @author nang
 * @version $Revision: 1.1 $
 */
class DefaultErrorManager
        extends ErrorManager
{
    /**
     * Create a DefaultErrorManager instance without a Windowing (graphical)
     * interface.  An instance created in this manner causes dispatches to
     * primary interface conduits (e.g., via {@link #postNotification}) are
     * redirected to the log (e.g., {@link #logNotification} is used instead).
     * 
     * @param name Subsystem to associate with this ErrorManager
     */ 
    public DefaultErrorManager(Object name)
    {
        mLogger = Logger.getLogger(name.toString());
        mRoot = null;
    }

    /**
     * Create a DefaultErrorManager instance.
     *
     * @param name          Subsystem to associate with this ErrorManager
     * @param interfaceRoot Top-level component of the primary interface, used
     *                      for operations that require a reference point for
     *                      rendering considerations
     */ 
    public DefaultErrorManager(Object name, Window interfaceRoot)
    {
        if (name == null) {
            throw new NullPointerException("name");
        }
        
        if (interfaceRoot == null) {
            throw new NullPointerException("interfaceRoot");
        }
        
        mRoot = interfaceRoot;
        mLogger = Logger.getLogger(name.toString());
    }

    /**
     * Send a notification to the primary interface and record it into the log.
     *
     * @param level Translated level value computed by
     *              {@link #mapSeverityToNativeLevel}
     * @param msg   Message to log
     * @param t     Optional detail
     */
    protected void postNotification(final Object level,
                                    final String msg,
                                    final Throwable t)
    {
        if (mRoot == null) {
            postToLog(level, msg, t);
        }
        else if (SwingUtilities.isEventDispatchThread()) {
            postToLog(level, msg, t);
            
            int severity = ((SeverityMapping) level).getNotificationType();
            if (severity != Integer.MIN_VALUE) {
                JOptionPane.showMessageDialog(mRoot, msg, "", severity);
            }
        }
        else {
            SwingUtilities.invokeLater(new Runnable() {
                public void run()
                {
                    postNotification(level, msg, t);
                }
            });
        }
    }

    /**
     * Record to log but do not send a notification to the primary interface.
     *
     * @param level Translated level value computed by {@link
     *              #mapSeverityToNativeLevel}
     * @param msg   Message to log
     * @param t     Optional detail
     */
    protected void logNotification(Object level, String msg, Throwable t)
    {
        postToLog(level, msg, t);
    }

    /**
     * Write a message to a log.
     *
     * @param level Translated level value computed by
     *              {@link #mapSeverityToNativeLevel}
     * @param msg   Message to log
     * @param t     Optional detail
     */
    protected void postToLog(Object level, String msg, Throwable t)
    {
        Level logLevel = (Level) ((SeverityMapping) level).getLoggingLevel();
        if (t != null) {
            mLogger.log(logLevel, msg, t);
        }
        else {
            mLogger.log(logLevel, msg);
        }
    }
    
    /**
     * Map an ErrorManager severity to an equivalent value in the implementation
     * context. The type of object returned is implementation-defined, because
     * its use is also implementation-defined.
     */
    protected Object mapSeverityToNativeLevel(Severity sev)
    {
        Object swingMapping = null;
        Object loggerMapping = null;
        int messageType;

        if (sev != null) {
            swingMapping = cJOptionDialogSeverity.get(sev);
            loggerMapping = cLoggerSeverityMap.get(sev);
        }
        
        messageType = (swingMapping != null
                      ? ((Integer) swingMapping).intValue()
                      : Integer.MIN_VALUE);
        
        if (loggerMapping == null) {
            loggerMapping = Level.FINE;
        }
        
        return new SeverityMapping(messageType, loggerMapping);
    }

    
    private static final Map<Severity, Integer> cJOptionDialogSeverity;
    private static final Map<Severity, Level> cLoggerSeverityMap;
    static
    {
        // NetBeans ErrorManager severity mapping
        Map<Severity, Integer> map = new HashMap<Severity, Integer>();
        map.put(Severity.ERROR,
                new Integer(JOptionPane.ERROR_MESSAGE));
        map.put(Severity.WARN,
                new Integer(JOptionPane.WARNING_MESSAGE));
        map.put(Severity.INFO,
                new Integer(JOptionPane.INFORMATION_MESSAGE));
        cJOptionDialogSeverity = Collections.unmodifiableMap(map);
        
        // Java logging Levels mapping
        Map<Severity, Level> map2 = new HashMap<Severity, Level>();
        map2.put(Severity.ERROR, Level.SEVERE);
        map2.put(Severity.WARN, Level.WARNING);
        map2.put(Severity.INFO, Level.INFO);
        map2.put(Severity.DEBUG, Level.FINE);
        cLoggerSeverityMap = Collections.unmodifiableMap(map2);
    }
    
    private final Logger mLogger;
    private Window mRoot;
    
    private class SeverityMapping
    {
        public SeverityMapping(int swingMapping, Object logMapping)
        {
            mNotificationType = swingMapping;
            mLoggingLevel = logMapping;
        }

        public int getNotificationType()
        {
            return mNotificationType;
        }

        public Object getLoggingLevel()
        {
            return mLoggingLevel;
        }

        private final int mNotificationType;
        private final Object mLoggingLevel;
    }
}

// FINIS $RCSfile: DefaultErrorManager.java,v $
