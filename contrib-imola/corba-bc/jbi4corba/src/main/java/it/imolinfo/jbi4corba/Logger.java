 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba;


import javax.jbi.component.ComponentContext;
import javax.xml.namespace.QName;


/**
 * The main user interface to logging. It is expected that logging takes place
 * through concrete implementations of this interface.
 * <p>
 *
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public interface Logger  {

     void debug(String msg);

     void debug(String string, Object[] object);

     void debug(String errmsg, Object object);

     void debug(String errmsg, Throwable  e);

     void debug(String string, Object obj1, Object  obj2);

     void error(String msg);

     void error(String string, Object[] args);

     void error(String msg, Throwable ex);

     void error(String msg,  Object object);

     void error(String string, Object obj1, Object  obj2);

     void info (String msg);

     void info (String string, Object[] args);

     void info (String msg, Throwable ex);

     void info (String msg,  Object object);

     void info (String string, Object obj1, Object  obj2);

     boolean isDebugEnabled();

     boolean isInfoEnabled();

     void warn(String string);

     void warn(String string, Object[] object);

     void warn(String string, Object object);

     void warn(String string, Throwable t);

     void warn(String string, Object obj1, Object  obj2);


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
