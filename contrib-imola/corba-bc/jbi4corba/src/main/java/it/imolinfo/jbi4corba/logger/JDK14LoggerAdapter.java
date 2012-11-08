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


package it.imolinfo.jbi4corba.logger;

import it.imolinfo.jbi4corba.jbi.Messages;
import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;


//import org.slf4j.Marker;
//import org.slf4j.helpers.MarkerIgnoringBase;
//import org.slf4j.spi.LocationAwareLogger;

/**
 * A wrapper over {@link java.util.logging.Logger} in conformity with the
 * {@link org.slf4j.Logger} interface. Note that the logging levels mentioned in
 * this class refer to those defined in the <code>java.util.logging</code>
 * package.
 * <p>
 *
 * @author Ceki G&uuml;lc&uuml;
 * @author Peter Royal
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
final class JDK14LoggerAdapter  implements  it.imolinfo.jbi4corba.Logger {

    /**
     * This class name.
     */
    private static final String SELF = JDK14LoggerAdapter.class.getName();

    /**
     * The super-class name of this class.
     */
    //private static final String SUPER = MarkerIgnoringBase.class.getName();

    /**
     * The logger adapted by this instance.
     */
    private final Logger logger;

    /**
     * The optional <code>Messages</code> available to apply I18N to logged
     * messages.
     */
    private final Messages messages;

    /**
     * Creates a new adapter for the specificed logger.
     *
     * @param   logger                the logger.
     * @param   messages              the optional <code>Messages</code>
     *                                instance, responsible to apply I18N to
     *                                messages logged by <code>logger</code>. It
     *                                may be <code>null</code>, so there isn't
     *                                I18N on logged messages.
     */
    JDK14LoggerAdapter(final Logger logger, final Messages messages) {
        if (logger == null) {
            throw new NullPointerException("Log4j logger null");
        }

        this.logger   = logger;
        this.messages = messages;
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
     * Indicates if this <code>Logger</code> is applying internationalization
     * to logged messages.
     *
     * @return  <code>true</code> if and only if this instance is applying I18N
     *          to logged messages.
     */
    private boolean isI18N() {
        return (messages != null);
    }

    /**
     * Formats the specified message, applying I18N if it is available for this
     * logger.
     *
     * @param   format  the format string.
     * @param   args    the optional arguments.
     * @return  the formatted message, eventually internationalized.
     */
    private String formatMessage(final String format, final Object... args) {
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
     * Is this logger instance enabled for the DEBUG level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for level DEBUG.
     */
    public boolean isDebugEnabled() {
        return logger.isLoggable(Level.FINE);
    }

    /**
     * Log a message object at level DEBUG.
     *
     * @param  msg  the message string to be logged.
     */
    public void debug(final String msg) {
        log(SELF, Level.FINE, msg, null);
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
            String msgStr = MessageFormat.format(format, arg);

            log(SELF, Level.FINE, msgStr, null);
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
            String msgStr = MessageFormat.format(format, arg1, arg2);

            log(SELF, Level.FINE, msgStr, null);
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
            String msgStr = MessageFormat.format(format, args);

            log(SELF, Level.FINE, msgStr, null);
        }
    }

    /**
     * Log an exception (throwable) at level DEBUG with an accompanying message.
     *
     * @param  msg  the message accompanying the exception.
     * @param  t    the exception (throwable) to log.
     */
    public void debug(final String msg, final Throwable t) {
        log(SELF, Level.FINE, msg, t);
    }

    /**
     * Is this logger instance enabled for the INFO level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for the INFO level.
     */
    public boolean isInfoEnabled() {
        return logger.isLoggable(Level.INFO);
    }

    /**
     * Log a message object at the INFO level.
     *
     * @param  msg  the message string to be logged.
     */
    public void info(final String msg) {
        if (isInfoEnabled()) {
            log(SELF, Level.INFO, formatMessage(msg), null);
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
            log(SELF, Level.INFO, formatMessage(format, arg), null);
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
            log(SELF, Level.INFO, formatMessage(format, arg1, arg2), null);
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
            log(SELF, Level.INFO, formatMessage(format, args), null);
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
            log(SELF, Level.INFO, formatMessage(msg), t);
        }
    }

    /**
     * Is this logger instance enabled for the WARN level?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for the WARN level.
     */
    public boolean isWarnEnabled() {
        return logger.isLoggable(Level.WARNING);
    }

    /**
     * Log a message object at the WARN level.
     *
     * @param  msg  the message string to be logged.
     */
    public void warn(final String msg) {
        if (isWarnEnabled()) {
            log(SELF, Level.WARNING, formatMessage(msg), null);
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
            log(SELF, Level.WARNING, formatMessage(format, arg), null);
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
            log(SELF, Level.WARNING, formatMessage(format, arg1, arg2), null);
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
            log(SELF, Level.WARNING, formatMessage(format, args), null);
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
            log(SELF, Level.WARNING, formatMessage(msg), t);
        }
    }

    /**
     * Is this logger instance enabled for level ERROR?
     *
     * @return  <code>true</code> if and only if this
     *          <code>org.slf4j.Logger</code> is enabled for level ERROR.
     */
    public boolean isErrorEnabled() {
        return logger.isLoggable(Level.SEVERE);
    }

    /**
     * Log a message object at the ERROR level.
     *
     * @param  msg  the message string to be logged.
     */
    public void error(final String msg) {
        if (isErrorEnabled()) {
            log(SELF, Level.SEVERE, formatMessage(msg), null);
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
            log(SELF, Level.SEVERE, formatMessage(format, arg), null);
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
            log(SELF, Level.SEVERE, formatMessage(format, arg1, arg2), null);
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
            log(SELF, Level.SEVERE, formatMessage(format, args), null);
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
            log(SELF, Level.SEVERE, formatMessage(msg), t);
        }
    }

    /**
     * Log the message at the specified level with the specified throwable if
     * any. This method creates a <code>java.util.logging.LogRecord</code> and
     * fills in caller date before calling this instance's JDK14 logger.
     * <p>
     * See bug report #13 for more details.
     *
     * @param  callerFQCN  the fully qualified class name of the <b>caller</b>.
     * @param  level       the level.
     * @param  msg         the message.
     * @param  t           the exception (throwable).
     */
    private void log(final String callerFQCN, final Level level,
                     final String msg, final Throwable t) {

        // Millis and thread are filled by the constructor
        LogRecord record = new LogRecord(level, msg);

        record.setLoggerName(getName());
        record.setThrown(t);
        fillCallerData(callerFQCN, record);
        logger.log(record);
    }

    /**
     * Fill in caller data if possible.
     *
     * @param  callerFQCN  the fully qualified class name of the <b>caller</b>.
     * @param  record      the record to update.
     */
    private static void fillCallerData(final String callerFQCN,
                                       final LogRecord record) {
        StackTraceElement[] steArray = new Throwable().getStackTrace();
        int length = steArray.length;
        int selfIndex = -1;

        for (int i = 0; i < length; i++) {
            final String className = steArray[i].getClassName();

            if (className.equals(callerFQCN) ) {
                selfIndex = i;
                break;
            }
        }
        for (int i = selfIndex + 1; i < length; i++) {
            final String className = steArray[i].getClassName();

            if (!(className.equals(callerFQCN) )) {
                StackTraceElement ste = steArray[i];

                /*
                 * Setting the class name has the side effect of setting the
                 * needToInferCaller variable to false
                 */
                record.setSourceClassName(ste.getClassName());
                record.setSourceMethodName(ste.getMethodName());
                break;
            }
        }
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
            String msgStr = MessageFormat.format(format, args);

            log(SELF, Level.FINE, msgStr, t);
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
            log(SELF, Level.INFO, formatMessage(format, args), t);
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
            log(SELF, Level.WARNING, formatMessage(format, args), t);
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
            log(SELF, Level.SEVERE, formatMessage(format, args), t);
        }
    }

}
