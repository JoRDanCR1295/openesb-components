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


import it.imolinfo.jbi4corba.Logger;
import java.util.MissingResourceException;
import it.imolinfo.jbi4corba.jbi.Messages;

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
public class JDK14LoggerFactory  {

    /**
     * Creates a new instance of this class.
     */
    public JDK14LoggerFactory() {
    }

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
    @SuppressWarnings("unchecked")
	public Logger getLogger(final String name) {
        Class clazz;
        Messages messages;
        java.util.logging.Logger logger=null;
        try {
            clazz = Class.forName(name);
            messages = Messages.getMessages(clazz);
         } catch (ClassNotFoundException e) {
            clazz = null;
            messages = null;
        } catch (MissingResourceException e) {
            clazz = null;
            messages = null;
        }
        
        if(clazz !=null){
            logger = com.sun.jbi.internationalization.Messages.getLogger(clazz);
        }
        if (logger == null) {
            if (name != null && name.equalsIgnoreCase(java.util.logging.Logger.global.getName())) {
                logger = java.util.logging.Logger.getLogger("");
            } else {
                logger = java.util.logging.Logger.getLogger(name);
            }
        }
        return new JDK14LoggerAdapter(logger, messages);
    }
}
