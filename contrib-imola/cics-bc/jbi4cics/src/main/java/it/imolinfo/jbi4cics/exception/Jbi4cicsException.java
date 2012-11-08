/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/
package it.imolinfo.jbi4cics.exception;

import it.imolinfo.jbi4cics.jbi.Messages;
import java.util.MissingResourceException;

/**
 * The default exception used within the component.
 *
 * @author <a href="mailto:rspazzoli@imolinfo.it">Raffaele Spazzoli</a>
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 */
public class Jbi4cicsException extends Exception {
    
    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = 3762815969835563319L;

    /**
     * The localized description of this <code>Throwable</code>.
     */
    private String localizedMessage;

    /**
     * A constructor.
     *
     * @param    message        The message of the exception.
     */
    public Jbi4cicsException(final String message) {
        this(message, null, null);
    }

    /**
     * A constructor.
     *
     * @param    message        The message of the exception.
     * @param    cause        The cause of the exception.
     */
    public Jbi4cicsException(final String message, final Throwable cause) {
        this(message, null, cause);
    }

    /**
     * A constructor.
     *
     * @param    cause    The cause of the exception.
     */
    public Jbi4cicsException(final Throwable cause) {
        this(cause.toString(), null, cause);

        // 'message' is computed from 'cause', so there is no need to I18N
        localizedMessage = getMessage();
    }

    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     */
    public Jbi4cicsException(final String message, final Object[] args) {
        this(message, args, null);
    }

    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     * @param   cause    The cause of the exception.
     */
    public Jbi4cicsException(final String message, final Object[] args,
                              final Throwable cause) {
        super(message, cause);
        setupLocalizedMessage(args);
    }

    /**
     * Calculates {@link #localizedMessage} value.
     *
     * @param  args  the optional arguments to define the complete message. It
     *               may be <code>null</code>.
     */
    private void setupLocalizedMessage(final Object[] args) {
        StackTraceElement[] stackTrace = getStackTrace();

        if (stackTrace.length == 0) {
            localizedMessage = getMessage();
        } else {
            try {
                Class clazz = Class.forName(stackTrace[0].getClassName());
                Messages messages = Messages.getMessages(clazz);

                localizedMessage = messages.getString(getMessage(), args);
            } catch (ClassNotFoundException e) {
                localizedMessage = getMessage();
            } catch (MissingResourceException e) {
                localizedMessage = getMessage();
            }
        }
    }

    @Override
    public String getLocalizedMessage() {                       // Overridden
        return localizedMessage;
    }
}
