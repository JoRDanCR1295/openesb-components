/*
 * @(#)LoggerImpl.java        $Revision: 1.2 $ $Date: 2008/07/05 04:01:46 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.logging;

import java.util.Formatter;
import java.util.logging.Level;
//import java.util.logging.LogRecord;

/**
 * Provides printf-style logging methods on top of a standard <code>java.util.logging.Logger</code>.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/05 04:01:46 $
 * 
 * @see java.util.logging.Logger
 * @since 0.1
 */
public class LoggerImpl implements Logger {
    
    /**
     * If not <code>null</code>, this prefix is placed in front of every logged message.
     */
    private final String prefix;
    
    /**
     * Internal logger from the Java Logging API that we delegate all the logging to.
     */
    private final java.util.logging.Logger logger;

    public LoggerImpl(String prefix, java.util.logging.Logger logger) {
        this.prefix = prefix;
        this.logger = logger;
    }

    public LoggerImpl(java.util.logging.Logger logger) {
        this(null, logger);
    }

    private void log(Level level, String msg, Throwable thrown, Object[] args) {
        if (logger.isLoggable(level)) {

            if (args != null) {

                /* Formatters are not thread-safe. We need to confine it to the executing thread. */
                Formatter formatter = new Formatter();
                msg = formatter.format(msg, args).toString();
            }

            if (prefix != null) {
                msg = prefix + ": " + msg;
            }

//            LogRecord logRecord = new LogRecord(level, msg);
//            logRecord.setThrown(thrown);
//            logRecord.setLoggerName(logger.getName());
//            logger.log(logRecord);

            if (thrown != null) {
                logger.log(level, msg, thrown);

            } else {
                logger.log(level, msg);
            }
        }
    }

    public void severe(String msg) {
        log(Level.SEVERE, msg, null, null);
    }

    public void severe(String msg, Object... args) {
        log(Level.SEVERE, msg, null, args);
    }

    public void severe(String msg, Throwable thrown) {
        log(Level.SEVERE, msg, thrown, null);
    }

    public void severe(String msg, Throwable thrown, Object... args) {
        log(Level.SEVERE, msg, thrown, args);
    }

    public void warning(String msg) {
        log(Level.WARNING, msg, null, null);
    }

    public void warning(String msg, Object... args) {
        log(Level.WARNING, msg, null, args);
    }

    public void warning(String msg, Throwable thrown) {
        log(Level.WARNING, msg, thrown, null);
    }

    public void warning(String msg, Throwable thrown, Object... args) {
        log(Level.WARNING, msg, thrown, args);
    }

    public void info(String msg) {
        log(Level.INFO, msg, null, null);
    }

    public void info(String msg, Object... args) {
        log(Level.INFO, msg, null, args);
    }

    public void info(String msg, Throwable thrown) {
        log(Level.INFO, msg, thrown, null);
    }

    public void info(String msg, Throwable thrown, Object... args) {
        log(Level.INFO, msg, thrown, args);
    }

    public void config(String msg) {
        log(Level.CONFIG, msg, null, null);
    }

    public void config(String msg, Object... args) {
        log(Level.CONFIG, msg, null, args);
    }

    public void config(String msg, Throwable thrown) {
        log(Level.CONFIG, msg, thrown, null);
    }

    public void config(String msg, Throwable thrown, Object... args) {
        log(Level.CONFIG, msg, thrown, args);
    }

    public void fine(String msg) {
        log(Level.FINE, msg, null, null);
    }

    public void fine(String msg, Object... args) {
        log(Level.FINE, msg, null, args);
    }

    public void fine(String msg, Throwable thrown) {
        log(Level.FINE, msg, thrown, null);
    }

    public void fine(String msg, Throwable thrown, Object... args) {
        log(Level.FINE, msg, thrown, args);
    }

    public void finer(String msg) {
        log(Level.FINER, msg, null, null);
    }

    public void finer(String msg, Object... args) {
        log(Level.FINER, msg, null, args);
    }

    public void finer(String msg, Throwable thrown) {
        log(Level.FINER, msg, thrown, null);
    }

    public void finer(String msg, Throwable thrown, Object... args) {
        log(Level.FINER, msg, thrown, args);
    }
    
    public void finest(String msg) {
        log(Level.FINEST, msg, null, null);
    }

    public void finest(String msg, Object... args) {
        log(Level.FINEST, msg, null, args);
    }

    public void finest(String msg, Throwable thrown) {
        log(Level.FINEST, msg, thrown, null);
    }

    public void finest(String msg, Throwable thrown, Object... args) {
        log(Level.FINEST, msg, thrown, args);
    }
    
//    public void entering(Class<?> sourceClass, String sourceMethod) {
//        log(Level.FINER, "Entering %s.%s()", null, new Object[] {sourceClass.getSimpleName(), sourceMethod});
//    }

    public void entering(Class<?> sourceClass, String sourceMethod, Object... args) {
        StringBuilder sb = new StringBuilder();
        
        if (args.length > 0) {
            for (int i = 0; i < args.length - 1; i++) {
                sb.append(args[i].toString());
                sb.append(", ");
            }

            sb.append(args[args.length - 1]);
        }
        
        log(Level.FINER, "Entering %s.%s(%s)", null, new Object[]{
                sourceClass.getSimpleName(), sourceMethod, sb.toString()
        });
    }
    
    public void exiting(Class<?> sourceClass, String sourceMethod) {
        log(Level.FINER, "Exiting %s.%s()", null, new Object[]{sourceClass.getSimpleName(), sourceMethod});
    }

    public void important(String msg, Object... args) {
        Formatter formatter = new Formatter();
        formatter.format(msg, args);
        important(formatter.toString());
    }
    
    public void important(String msg) {
        final int space = 4;
        
        logFullLine(msg.length() + 2 * space);
        logEmptyLine(msg.length() + 2 * space);

        StringBuilder sb = new StringBuilder();
        sb.append("+");
        for (int i = 0; i < space; i++) {
            sb.append(" ");

        }
        sb.append(msg);
        for (int i = 0; i < space; i++) {
            sb.append(" ");

        }
        sb.append("+");
        log(Level.FINEST, sb.toString(), null, null);
        
        logEmptyLine(msg.length() + 2 * space);
        logFullLine(msg.length() + 2 * space);
    }

    private void logFullLine(int length) {
        StringBuilder sb = new StringBuilder();
        sb.append("+");
        for (int i = 0; i < length; i++) {
            sb.append("-");

        }
        sb.append("+");
        log(Level.FINEST, sb.toString(), null, null);
    }

    private void logEmptyLine(int length) {
        StringBuilder sb = new StringBuilder();
        sb.append("+");
        for (int i = 0; i < length; i++) {
            sb.append(" ");

        }
        sb.append("+");
        log(Level.FINEST, sb.toString(), null, null);
    }
    
    public static void main(String[] args) {
        Logger logger = new LoggerImpl(Logger.GLOBAL_PREFIX,
                java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME));
        
        logger.important("Test message");
    }
}
