 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.exception;

/**
 * An exception used within the component during the 'service creation' phase.
 *
 * @author raffaele
 */
public class ServiceCreationException extends Jbi4CorbaException {

    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = -8577828176482419052L;

    /**
     * A constructor.
     *
     * @param    message        The message of the exception.
     */
    public ServiceCreationException(final String message) {
        super(message);
    }

    /**
     * A constructor.
     *
     * @param    message        The message of the exception.
     * @param    cause        The cause of the exception.
     */
    public ServiceCreationException(
            final String message, final Throwable cause) {
        super(message, cause);
    }

    /**
     * A constructor.
     *
     * @param    cause    The cause of the exception.
     */
    public ServiceCreationException(final Throwable cause) {
        super(cause);
    }

    /**
     * XXXJavadoc.
     * 
     * @param format  The format
     * @param args    The argoments
     * @param cause   The cause
     */
    public ServiceCreationException(
            final String format, final Object[] args, final Throwable cause) {
        super(format, args, cause);
    }
}
