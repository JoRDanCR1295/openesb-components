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
 */

package com.sun.jbi.mqbc.monitoring;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.alerter.Alerter;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.MQBindingComponent;

/**
 * EventLogger takes log messages and sends it to two sources: a {@link
 * java.util.logging.Logger} and an {@link com.sun.jbi.alerter.Alerter}.
 *
 * @author Noel.Ang@sun.com
 */
public class EventLogger {

    public EventLogger(Logger logger, Alerter alerter) {
        if (logger == null) {
            throw new NullPointerException("logger");
        }
        if (alerter == null) {
            throw new NullPointerException("alerter");
        }
        this.logger = logger;
        this.alerter = alerter;
        componentName = MQBindingComponent.COMPONENT_NAME;
        componentType = MQBindingComponent.COMPONENT_TYPE;
        prefix = I18n.prefix;
    }

    public Logger getLogger() {
        return logger;
    }
    
    public void log(Level level,
                    int severity,
                    int state,
                    String msg,
                    Throwable thrown) {
        logger.log(level, msg, thrown);
        doAlert(severity, state, msg);
    }

    public void log(Level level,
                    int severity,
                    int state,
                    String msg) {
        logger.log(level, msg);
        doAlert(severity, state, msg);
    }

    public void severe(int severity, int state, String msg) {
        logger.severe(msg);
        doAlert(severity, state, msg);
    }

    public void warning(int severity, int state, String msg) {
        logger.warning(msg);
        doAlert(severity, state, msg);
    }

    public void info(int severity, int state, String msg) {
        logger.info(msg);
        doAlert(severity, state, msg);
    }

    public void config(int severity, int state, String msg) {
        logger.config(msg);
        doAlert(severity, state, msg);
    }

    public void fine(String msg) {
        logger.fine(msg);
    }

    public void finer(String msg) {
        logger.finer(msg);
    }

    public void finest(String msg) {
        logger.finest(msg);
    }

    public void setLevel(Level newLevel) throws SecurityException {
        logger.setLevel(newLevel);
    }

    public Level getLevel() {
        return logger.getLevel();
    }

    public boolean isLoggable(Level level) {
        return logger.isLoggable(level);
    }

    private void doAlert(int severity, int state, String msg) {

        // state validation - normalize undefined values.
        switch (state) {
            case NotificationEvent.OPERATIONAL_STATE_RUNNING: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_SHUTDOWN: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_SHUTTINGDOWN: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_STARTED: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_STARTING: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_STOPPED: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_STOPPING: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_SUSPENDED: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_SUSPENDING: // fallthru
            case NotificationEvent.OPERATIONAL_STATE_UNKNOWN:
                break;
            default:
                state = NotificationEvent.OPERATIONAL_STATE_UNKNOWN;
        }

        switch (severity) {
            case NotificationEvent.SEVERITY_TYPE_INFO:
                alertInfo(state, msg);
                break;
            case NotificationEvent.SEVERITY_TYPE_MINOR:
                alertMinor(state, msg);
                break;
            case NotificationEvent.SEVERITY_TYPE_MAJOR:
                alertMajor(state, msg);
                break;
            case NotificationEvent.SEVERITY_TYPE_WARNING:
                alertWarning(state, msg);
                break;
            case NotificationEvent.SEVERITY_TYPE_CRITICAL:
                alertCritical(state, msg);
                break;
            case NotificationEvent.SEVERITY_TYPE_FATAL:
                alertFatal(state, msg);
                break;
            default: // do nothing for unknown severity value
        }
    }

    private void alertMinor(int componentState, String msg) {
        alerter.minor(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private void alertMajor(int componentState, String msg) {
        alerter.major(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private void alertWarning(int componentState, String msg) {
        alerter.warning(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private void alertCritical(int componentState, String msg) {
        alerter.critical(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private void alertFatal(int componentState, String msg) {
        alerter.fatal(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private void alertInfo(int componentState, String msg) {
        alerter.info(msg,
                componentName,
                null,
                null,
                componentType,
                componentState,
                NotificationEvent.EVENT_TYPE_ALERT,
                makeMessageCode(msg));
    }

    private String makeMessageCode(String msg) {
        String msgCode = "";
        if (msg.startsWith(prefix)) {
            int end = msg.indexOf(":");
            msgCode = (end == -1 ? prefix : msg.substring(0, end));
        }
        return msgCode;
    }

    private final Logger logger;
    private final Alerter alerter;
    private final String componentName;
    private final String componentType;
    private final String prefix;
}
