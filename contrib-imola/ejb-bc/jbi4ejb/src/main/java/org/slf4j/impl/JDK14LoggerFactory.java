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

import it.imolinfo.jbi4ejb.jbi.Messages;

import java.util.MissingResourceException;

import org.slf4j.ILoggerFactory;
import org.slf4j.Logger;

/**
 * <code>JDK14LoggerFactory</code> is an implementation of
 * {@link ILoggerFactory} returning the appropriately named
 * {@link JDK14LoggerAdapter} instance.
 * <p>
 *
 * @author Ceki G&uuml;lc&uuml;
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
final class JDK14LoggerFactory implements ILoggerFactory {

    /**
     * Creates a new instance of this class.
     */
    JDK14LoggerFactory() {}

    /**
     * Return an appropriate <code>org.slf4j.Logger</code> instance as
     * specified by the <code>name</code> parameter.
     * <p>
     * If <code>name</code> is equal to the string value <code>"NULL"</code>
     * (case insensitive), then the root logger of the underlying logging system
     * is returned.
     * <p>
     * Null-valued name arguments are considered invalid.
     * <p>
     * Certain extremely simple logging systems, e.g. NOP, may always return the
     * same logger instance regardless of the requested name.
     *
     * @param name
     *            the name of the Logger to return.
     * @return the <code>org.slf4j.Logger</code> instance as specified by
     *         <code>name</code>.
     */
    public Logger getLogger(final String name) {
        java.util.logging.Logger logger;
        Messages messages;

        // The root logger is called "" in JUL
        if (name.equalsIgnoreCase(Logger.ROOT_LOGGER_NAME)) {
            logger = java.util.logging.Logger.getLogger("");
        } else {
            logger = java.util.logging.Logger.getLogger(name);
        }

        // If name identifies a Class, there may be an associated resource
        // bundle to apply I18N
        try {
            Class clazz = Class.forName(name);

            messages = Messages.getMessages(clazz);
        } catch (ClassNotFoundException e) {
            messages = null;
        } catch (MissingResourceException e) {
            messages = null;
        }
        return new JDK14LoggerAdapter(logger, messages);
    }
}
