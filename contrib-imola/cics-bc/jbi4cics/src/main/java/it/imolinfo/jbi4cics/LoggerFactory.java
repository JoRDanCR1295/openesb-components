/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics;


import org.slf4j.ILoggerFactory;
import org.slf4j.impl.StaticLoggerBinder;

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
    private static final ILoggerFactory FACTORY
            = StaticLoggerBinder.SINGLETON.getLoggerFactory();

    /**
     * Initializes a new instance of this class.
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
    public static Logger getLogger(final Class clazz) {
        return (Logger) FACTORY.getLogger(clazz.getName());
    }
}
