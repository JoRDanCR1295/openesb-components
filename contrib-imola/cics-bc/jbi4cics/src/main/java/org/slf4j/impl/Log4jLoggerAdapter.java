/*
 * Copyright (c) 2004-2005 SLF4J.ORG
 * Copyright (c) 2004-2005 QOS.ch
 *
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to  deal in  the Software without  restriction, including
 * without limitation  the rights to  use, copy, modify,  merge, publish,
 * distribute, and/or sell copies of  the Software, and to permit persons
 * to whom  the Software is furnished  to do so, provided  that the above
 * copyright notice(s) and this permission notice appear in all copies of
 * the  Software and  that both  the above  copyright notice(s)  and this
 * permission notice appear in supporting documentation.
 *
 * THE  SOFTWARE IS  PROVIDED  "AS  IS", WITHOUT  WARRANTY  OF ANY  KIND,
 * EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR  A PARTICULAR PURPOSE AND NONINFRINGEMENT
 * OF  THIRD PARTY  RIGHTS. IN  NO EVENT  SHALL THE  COPYRIGHT  HOLDER OR
 * HOLDERS  INCLUDED IN  THIS  NOTICE BE  LIABLE  FOR ANY  CLAIM, OR  ANY
 * SPECIAL INDIRECT  OR CONSEQUENTIAL DAMAGES, OR  ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS  OF USE, DATA OR PROFITS, WHETHER  IN AN ACTION OF
 * CONTRACT, NEGLIGENCE  OR OTHER TORTIOUS  ACTION, ARISING OUT OF  OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Except as  contained in  this notice, the  name of a  copyright holder
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * of the copyright holder.
 */


package org.slf4j.impl;

import it.imolinfo.jbi4cics.jbi.Messages;
import java.text.MessageFormat;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.slf4j.Marker;
import org.slf4j.helpers.MarkerIgnoringBase;
import org.slf4j.spi.LocationAwareLogger;

/**
 * A wrapper over <code>org.apache.log4j.Logger</code> in conformance with the
 * <code>org.slf4j.Logger</code> interface. Note that the logging levels
 * mentioned in this class refer to those defined in the <a
 * href="http://logging.apache.org/log4j/docs/api/org/apache/log4j/Level.html">
 * <code>org.apache.log4j.Level</code></a> class.
 * <p>
 * This adapter is capable to translate (I18N) messages logged with level &gt;=
 * <i>INFO</i>, while <i>DEBUG</i> messages are leaved as they are.
 * <p>
 *
 * @author Ceki G&uuml;lc&uuml
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
final class Log4jLoggerAdapter extends MarkerIgnoringBase
        implements LocationAwareLogger, it.imolinfo.jbi4cics.Logger {

    /**
     * Following the pattern discussed in pages 162 through 168 of "The
     * complete log4j manual".
     */
    private static final String FQCN = Log4jLoggerAdapter.class.getName();

    /**
     * The Log4j logger adapted by this instance.
     */
    private final Logger logger;

    /**
     * The optional <code>Messages</code> available to apply I18N to logged
     * messages.
     */
    private final Messages messages;

    /**
     * Creates a new adapter for the specificed Log4j logger.
     *
     * @param  logger    the Log4j logger. Must be not <code>null</code>.
     * @param  messages  the optional <code>Messages</code> instance,
     *                   responsible to apply I18N to messages logged by
     *                   <code>logger</code>. It may be <code>null</code>, so
     *                   there isn't I18N on logged messages.
     */
    Log4jLoggerAdapter(final Logger logger, final Messages messages) {
        this.logger   = logger;
        this.messages = messages;
    }

    /**
     * Indicates if this <code>Logger</code> is applying internationalization
     * to logged messages.
     *
     * @return  <code>true</code> if and only if this instance is applying I18N
     *          to logged messages.
     */
    private boolean isI18N() {
        return messages != null;
    }

    /**
     * Formats the specified message, applying I18N if it is available for this
     * logger.
     *
     * @param   format  the format string.
     * @param   args    the optional arguments.
     * @return  the formatted message, eventually internationalized.
     */
    private String formatMessage(final String format, final Object ... args) {
        String msg;

        if (isI18N()) {
            if (args.length == 0) {
                msg = messages.getString(format);
            } else {
                msg = messages.getString(format, args);
            }
        } else {
            if (args.length == 0) {
                msg = format;
            } else {
                try {
                    msg = MessageFormat.format(format, args);
                } catch (IllegalArgumentException e) {
                    msg = format;
                }
            }
        }
        return msg;
    }

    /**
     * Gets the name of this <code>Logger</code>.
     *
     * @return the name of this <code>Logger</code> instance.
     */
    public String getName() {
        return logger.getName();
    }

    /**
     * Is this logger instance enabled for the DEBUG level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for level DEBUG.
     */
    public boolean isDebugEnabled() {
        return logger.isDebugEnabled();
    }

    /**
     * Log a message object at level DEBUG.
     *
     * @param  msg  the message string to be logged.
     */
    public void debug(final String msg) {
        logger.log(FQCN, Level.DEBUG, msg, null);
    }

    /**
     * Log a message at level DEBUG according to the specified format and
     * argument.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for level DEBUG.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg     the argument.
     */
    public void debug(final String format, final Object arg) {
        if (isDebugEnabled()) {

            // Debug level -> no I18N, argument formatting only
            String msg = MessageFormat.format(format, arg);

            logger.log(FQCN, Level.DEBUG, msg, null);
        }
    }

    /**
     * Log a message at level DEBUG according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the DEBUG level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg1    the first argument
     * @param  arg2    the second argument.
     */
    public void debug(
            final String format, final Object arg1, final Object arg2) {
        if (isDebugEnabled()) {

            // Debug level -> no I18N, argument formatting only
            String msg = MessageFormat.format(format, arg1, arg2);

            logger.log(FQCN, Level.DEBUG, msg, null);
        }
    }

    /**
     * Log a message at level DEBUG according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the DEBUG level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     */
    public void debug(final String format, final Object[] args) {
        if (isDebugEnabled()) {

            // Debug level -> no I18N, argument formatting only
            String msg = MessageFormat.format(format, args);

            logger.log(FQCN, Level.DEBUG, msg, null);
        }
    }

    /**
     * Log an exception (throwable) at level DEBUG with an accompanying message.
     *
     * @param  msg  the message accompanying the exception.
     * @param  t    the exception (throwable) to log.
     */
    public void debug(final String msg, final Throwable t) {

        // Debug level -> no I18N
        logger.log(FQCN, Level.DEBUG, msg, t);
    }

    /**
     * Is this logger instance enabled for the INFO level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for the INFO level.
     */
    public boolean isInfoEnabled() {
        return logger.isInfoEnabled();
    }

    /**
     * Log a message object at the INFO level.
     *
     * @param  msg  the message string to be logged.
     */
    public void info(final String msg) {
        if (isInfoEnabled()) {
            logger.log(FQCN, Level.INFO, formatMessage(msg), null);
        }
    }

    /**
     * Log a message at level INFO according to the specified format and
     * argument.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the INFO level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg     the argument.
     */
    public void info(final String format, final Object arg) {
        if (isInfoEnabled()) {
            logger.log(FQCN, Level.INFO, formatMessage(format, arg), null);
        }
    }

    /**
     * Log a message at the INFO level according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the INFO level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg1    the first argument.
     * @param  arg2    the second argument.
     */
    public void info(
            final String format, final Object arg1, final Object arg2) {
        if (isInfoEnabled()) {
            String msg = formatMessage(format, arg1, arg2);

            logger.log(FQCN, Level.INFO, msg, null);
        }
    }

    /**
     * Log a message at level INFO according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the INFO level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     */
    public void info(final String format, final Object[] args) {
        if (isInfoEnabled()) {
            logger.log(FQCN, Level.INFO, formatMessage(format, args), null);
        }
    }

    /**
     * Log an exception (throwable) at the INFO level with an accompanying
     * message.
     *
     * @param  msg  the message accompanying the exception
     * @param  t    the exception (throwable) to log.
     */
    public void info(final String msg, final Throwable t) {
        if (isInfoEnabled()) {
            logger.log(FQCN, Level.INFO, formatMessage(msg), t);
        }
    }

    /**
     * Is this logger instance enabled for the WARN level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for the WARN level.
     */
    public boolean isWarnEnabled() {
        return logger.isEnabledFor(Level.WARN);
    }

    /**
     * Log a message object at the WARN level.
     *
     * @param  msg  the message string to be logged.
     */
    public void warn(final String msg) {
        if (isWarnEnabled()) {
            logger.log(FQCN, Level.WARN, formatMessage(msg), null);
        }
    }

    /**
     * Log a message at the WARN level according to the specified format and
     * argument.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the WARN level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg     the argument.
     */
    public void warn(final String format, final Object arg) {
        if (isWarnEnabled()) {
            logger.log(FQCN, Level.WARN, formatMessage(format, arg), null);
        }
    }

    /**
     * Log a message at the WARN level according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the WARN level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg1    the first argument.
     * @param  arg2    the second argument.
     */
    public void warn(
            final String format, final Object arg1, final Object arg2) {
        if (isWarnEnabled()) {
            String msg = formatMessage(format, arg1, arg2);

            logger.log(FQCN, Level.WARN, msg, null);
        }
    }

    /**
     * Log a message at level WARN according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the WARN level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     */
    public void warn(final String format, final Object[] args) {
        if (isWarnEnabled()) {
            logger.log(FQCN, Level.WARN, formatMessage(format, args), null);
        }
    }

    /**
     * Log an exception (throwable) at the WARN level with an accompanying
     * message.
     *
     * @param  msg  the message accompanying the exception.
     * @param  t    the exception (throwable) to log.
     */
    public void warn(final String msg, final Throwable t) {
        if (isWarnEnabled()) {
            logger.log(FQCN, Level.WARN, formatMessage(msg), t);
        }
    }

    /**
     * Is this logger instance enabled for level ERROR?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for level ERROR.
     */
    public boolean isErrorEnabled() {
        return logger.isEnabledFor(Level.ERROR);
    }

    /**
     * Log a message object at the ERROR level.
     *
     * @param  msg  the message string to be logged.
     */
    public void error(final String msg) {
        if (isErrorEnabled()) {
            logger.log(FQCN, Level.ERROR, formatMessage(msg), null);
        }
    }

    /**
     * Log a message at the ERROR level according to the specified format
     * and argument.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the ERROR level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg     the argument.
     */
    public void error(final String format, final Object arg) {
        if (isErrorEnabled()) {
            String msg = formatMessage(format, arg);

            logger.log(FQCN, Level.ERROR, msg, null);
        }
    }

    /**
     * Log a message at the ERROR level according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the ERROR level.
     * </p>
     *
     * @param  format  the format string.
     * @param  arg1    the first argument.
     * @param  arg2    the second argument.
     */
    public void error(
            final String format, final Object arg1, final Object arg2) {
        if (isErrorEnabled()) {
            String msg = formatMessage(format, arg1, arg2);

            logger.log(FQCN, Level.ERROR, msg, null);
        }
    }

    /**
     * Log a message at level ERROR according to the specified format and
     * arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the ERROR level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     */
    public void error(final String format, final Object[] args) {
        if (isErrorEnabled()) {
            logger.log(FQCN, Level.ERROR, formatMessage(format, args), null);
        }
    }

    /**
     * Log an exception (throwable) at the ERROR level with an accompanying
     * message.
     *
     * @param  msg  the message accompanying the exception.
     * @param  t    the exception (throwable) to log.
     */
    public void error(final String msg, final Throwable t) {
        if (isErrorEnabled()) {
            logger.log(FQCN, Level.ERROR, formatMessage(msg), t);
        }
    }

    /**
     * Printing method which support for location information.
     *
     * @param marker      the marker.
     * @param callerFQCN  the fully qualified class name of the <b>caller</b>.
     * @param level       the level.
     * @param msg         the message.
     * @param t           the exception (throwable).
     */
    public void log(final Marker marker, final String callerFQCN,
                    final int level, final String msg, final Throwable t) {
        Level log4jLevel;

        switch (level) {
        case LocationAwareLogger.DEBUG_INT:
            log4jLevel = Level.DEBUG;
            break;

        case LocationAwareLogger.INFO_INT:
            log4jLevel = Level.INFO;
            break;

        case LocationAwareLogger.WARN_INT:
            log4jLevel = Level.WARN;
            break;

        case LocationAwareLogger.ERROR_INT:
            log4jLevel = Level.ERROR;
            break;

        default:
            throw new IllegalArgumentException(
                    "Level number " + level + " is not recognized.");
        }
        logger.log(callerFQCN, log4jLevel, msg, t);
    }


    // New methods added to those provided by SLF4J: we want to log a formatted
    // string and a Throwable


    /**
     * Log an exception (throwable) at level DEBUG with an accompanying message
     * according to the specified format and arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the DEBUG level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     * @param  t       the exception (throwable) to log.
     */
    public void debug(
            final String format, final Object[] args, final Throwable t) {
        if (isDebugEnabled()) {

            // Debug level -> no I18N, argument formatting only
            String msg = MessageFormat.format(format, args);

            logger.log(FQCN, Level.DEBUG, msg, null);
        }
    }

    /**
     * Log an exception (throwable) at level INFO with an accompanying message
     * according to the specified format and arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the INFO level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     * @param  t       the exception (throwable) to log.
     */
    public void info(
            final String format, final Object[] args, final Throwable t) {
        if (isInfoEnabled()) {
            logger.log(FQCN, Level.INFO, formatMessage(format, args), t);
        }
    }

    /**
     * Log an exception (throwable) at level WARN with an accompanying message
     * according to the specified format and arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the WARN level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     * @param  t       the exception (throwable) to log.
     */
    public void warn(
            final String format, final Object[] args, final Throwable t) {
        if (isWarnEnabled()) {
            logger.log(FQCN, Level.WARN, formatMessage(format, args), t);
        }
    }

    /**
     * Log an exception (throwable) at level ERROR with an accompanying message
     * according to the specified format and arguments.
     * <p>
     * This form avoids superfluous object creation when the logger is disabled
     * for the ERROR level.
     * </p>
     *
     * @param  format  the format string.
     * @param  args    the arguments.
     * @param  t       the exception (throwable) to log.
     */
    public void error(
            final String format, final Object[] args, final Throwable t) {
        if (isErrorEnabled()) {
            logger.log(FQCN, Level.ERROR, formatMessage(format, args), t);
        }
    }
}
