/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.exception;

/**
 * The exception used when there is an error during the class generation phase.
 */
public class ClassGenerationException extends Jbi4EjbException {

    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = -9052085981255532462L;

    /**
     * A constructor.
     *
     * @param message
     *            The message of the exception.
     */
    public ClassGenerationException(final String message) {
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
    public ClassGenerationException(final String message, final Throwable cause) {
        super(message, cause);
    }

    /**
     * A constructor.
     *
     * @param cause
     *            The cause of the exception.
     */
    public ClassGenerationException(final Throwable cause) {
        super(cause);
    }

    /**
     * Instantiates a new class generation exception.
     *
     * @param format 
     *              The format of the message
     * @param args  
     *              The args for the format
     * @param cause 
     *              The cause
     */
    public ClassGenerationException(final String format, final Object[] args,
            final Throwable cause) {
        super(format, args, cause);
    }

    /**
     * Instantiates a new class generation exception.
     *
     * @param format
     *              The format of the message
     * @param args
     *              The args for the format
     */
    public ClassGenerationException(final String format, final Object[] args) {
        super(format, args);
    }
}
