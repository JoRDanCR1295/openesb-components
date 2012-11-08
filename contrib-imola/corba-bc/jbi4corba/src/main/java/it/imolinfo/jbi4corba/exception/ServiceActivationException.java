 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.exception;

/**
 * An exception used within the component during the activation phase.
 */
public class ServiceActivationException extends Jbi4CorbaException {

    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = -612271741334505029L;

    /**
     * A constructor.
     *
     * @param message
     *            The message of the exception.
     */
    public ServiceActivationException(final String message) {
        super(message);
    }

    /**
     * A constructor.
     *
     * @param message
     *            The message of the exception.
     * @param cause
     *            The cause of the exception.
     */
    public ServiceActivationException(
            final String message, final Throwable cause) {
        super(message, cause);
    }

    /**
     * A constructor.
     *
     * @param cause
     *            The cause of the exception.
     */
    public ServiceActivationException(final Throwable cause) {
        super(cause);
    }

    /**
     * A constructor with i18n support.
     *
     * @param  message  the message of the exception.
     * @param  args     the optional arguments applied to <code>message</code>.
     * @param  cause    the cause of the exception.
     */
    public ServiceActivationException(
            final String message, final Object[] args, final Throwable cause) {
        super(message, args, cause);
    }
}
