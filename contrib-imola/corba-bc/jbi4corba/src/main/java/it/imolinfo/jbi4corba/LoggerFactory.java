 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba;

import it.imolinfo.jbi4corba.logger.JDK14LoggerFactory;

/**
 * Factory class producing {@link Logger} for various logging APIs, most notably
 * for Log4j and JDK 1.4 logging.
 * <p>
 *
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class LoggerFactory {

    /**
     * The internal factory used to obtain loggers.
     */


    private static final JDK14LoggerFactory FACTORY=new JDK14LoggerFactory();
    /**
     * Initializes a new instance of this class. It is declared <i>private</i>
     * to avoid creation of instances of this class.
     */
    private LoggerFactory() {
    }

    /**
     * Returns a logger named corresponding to the class passed as parameter.
     *
     * @param   clazz   the returned <code>Logger</code> will be named as this
     *                  class. Must be not <code>null</code>.
     * @return  the logger named as <code>clazz</code>.
     */
    @SuppressWarnings({ "unchecked"})
	public static Logger getLogger(final Class clazz) {
        return (Logger) FACTORY.getLogger(clazz.getName());
    }
}
