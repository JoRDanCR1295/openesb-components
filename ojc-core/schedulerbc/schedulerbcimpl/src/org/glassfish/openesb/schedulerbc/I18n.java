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
 * @(#)I18n.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.AlerterImpl;
import com.sun.jbi.alerter.NotificationEvent;
import java.util.regex.Pattern;

import com.sun.jbi.common.util.LocalizationSupport;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.alerter.Event.ComponentType;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;

/**
 * Internationalization utility for SchedulerBC.
 * @author sunsoabi_edwong
 */
public class I18n extends LocalizationSupport {

    private static final I18n i18n = new I18n();
    
    public static final String SERVER_TYPE_GLASSFISH = "Glassfish";     //NOI18N

    private Pattern pattern;
    private AlerterProxy alerterProxy;
    private static final String COL_SPC = ": ";                         //NOI18N
    private static final String EMPTY_STR = "";                         //NOI18N

    protected I18n() {
        this(Pattern.compile(
                "(SCHEDBC-[4-7]\\d\\d\\d)(" + COL_SPC + ")(.*)",        //NOI18N
                Pattern.DOTALL));
    }
    
    protected I18n(Pattern pattern) {
        super(pattern, EMPTY_STR, null);
        this.pattern = pattern;
        alerterProxy = new AlerterProxy();
    }

    public static String loc(String message, Object... params) {
        String[] idMessage = idLoc(message, params);
        return Util.isEmpty(idMessage[0]) ? idMessage[1]
                : idMessage[0] + COL_SPC + idMessage[1];
    }
    
    public static String[] idLoc(String message, Object... params) {
        Matcher matcher = i18n.pattern.matcher(message);
        if (!matcher.matches()) {
            return new String[] {EMPTY_STR, i18n.format(message, params)};
        }
        return new String[] {
            matcher.group(1),
            i18n.t(message, params).substring(matcher.group(1).length()
                    + matcher.group(2).length())
        };
    }
    
    public static boolean finestLoggable(Logger logger) {
        return logger.isLoggable(Level.FINEST);
    }
    
    public static String finest(Logger logger, String message,
            Object... params) {
        if (finestLoggable(logger)) {
            logger.finest(message = loc(message, params));
            return message;
        }
        return EMPTY_STR;
    }
    
    public static boolean finerLoggable(Logger logger) {
        return logger.isLoggable(Level.FINER);
    }
    
    public static String finer(Logger logger, String message,
            Object... params) {
        if (finerLoggable(logger)) {
            logger.finer(message = loc(message, params));
            return message;
        }
        return EMPTY_STR;
    }
    
    public static boolean fineLoggable(Logger logger) {
        return logger.isLoggable(Level.FINE);
    }
    
    public static String fine(Logger logger, String message,
            Object... params) {
        if (fineLoggable(logger)) {
            logger.fine(message = loc(message, params));
            return message;
        }
        return EMPTY_STR;
    }
    
    public static boolean configLoggable(Logger logger) {
        return logger.isLoggable(Level.CONFIG);
    }
    
    public static String config(Logger logger, String message,
            Object... params) {
        if (configLoggable(logger)) {
            logger.config(message = loc(message, params));
            return message;
        }
        return EMPTY_STR;
    }
    
    public static boolean infoLoggable(Logger logger) {
        return logger.isLoggable(Level.INFO);
    }
    
    public static String info(Logger logger, String message,
            Object... params) {
        String[] idLocArr = idLoc(message, params);
        String id = idLocArr[0];
        message = idLocArr[1];
        
        logger.info(id + COL_SPC + message);
        
        i18n.alerterProxy.info(id, message);
        
        return message;
    }
    
    public static boolean warningLoggable(Logger logger) {
        return logger.isLoggable(Level.WARNING);
    }
    
    public static String warning(Logger logger, String message,
            Object... params) {
        String[] idLocArr = idLoc(message, params);
        String id = idLocArr[0];
        message = idLocArr[1];
        
        logger.warning(id + COL_SPC + message);
        
        i18n.alerterProxy.warning(id, message);
        
        return message;
    }
    
    public static String warning(Logger logger, String message,
            Throwable thrown, Object... params) {
        String[] idLocArr = idLoc(message, params);
        String id = idLocArr[0];
        message = idLocArr[1];
        
        logger.log(Level.WARNING, id + COL_SPC + message, thrown);
        
        i18n.alerterProxy.warning(id, message);
        
        return message;
    }
    
    public static boolean severeLoggable(Logger logger) {
        return logger.isLoggable(Level.SEVERE);
    }
    
    public static String severe(Logger logger, String message,
            Object... params) {
        String[] idLocArr = idLoc(message, params);
        String id = idLocArr[0];
        message = idLocArr[1];
        
        logger.severe(id + COL_SPC + message);
        
        i18n.alerterProxy.critical(id, message);
        
        return message;
    }
    
    public static String severe(Logger logger, String message,
            Throwable thrown, Object... params) {
        String[] idLocArr = idLoc(message, params);
        String id = idLocArr[0];
        message = idLocArr[1];
        
        logger.log(Level.SEVERE, id + COL_SPC + message, thrown);
        
        i18n.alerterProxy.critical(id, message);
        
        return message;
    }
    
    public static String getString(Class clazz, String key, Object... params) {
        String message = ResourceBundle.getBundle(clazz.getPackage().getName()
                + ".Bundle").getString(key);                            //NOI18N
        if (message != null) {
            return String.format(message, params);
        }
        
        return null;
    }
    
    private class AlerterProxy {
    
        private Alerter delegate;
        
        public AlerterProxy() {
            super();
            delegate = new AlerterImpl();
        }
        
        public void critical(String messageId, String message) {
            delegate.critical(message, SchedulerConstants.COMPONENT_NAME,
                    null, SERVER_TYPE_GLASSFISH,
                    ComponentType.BindingComponent.name(),
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, messageId);
        }
        
        public void warning(String messageId, String message) {
            delegate.warning(message, SchedulerConstants.COMPONENT_NAME,
                    null, SERVER_TYPE_GLASSFISH,
                    ComponentType.BindingComponent.name(),
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, messageId);
        }
        
        public void info(String messageId, String message) {
            delegate.info(message, SchedulerConstants.COMPONENT_NAME,
                    null, SERVER_TYPE_GLASSFISH,
                    ComponentType.BindingComponent.name(),
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, messageId);
        }
    }
}
