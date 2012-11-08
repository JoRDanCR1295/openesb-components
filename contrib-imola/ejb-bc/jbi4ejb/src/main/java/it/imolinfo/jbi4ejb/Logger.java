/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4ejb;

/**
 * The main user interface to logging. It is expected that logging takes place
 * through concrete implementations of this interface.
 * <p>
 *
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public interface Logger extends org.slf4j.Logger {

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
    void debug(String format, Object[] args, Throwable t);

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
    void info(String format, Object[] args, Throwable t);

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
    void warn(String format, Object[] args, Throwable t);

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
    void error(String format, Object[] args, Throwable t);
}
