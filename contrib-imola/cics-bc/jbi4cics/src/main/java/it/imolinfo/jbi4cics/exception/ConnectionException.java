/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.exception;

/**
 *
 * @author <a href="mailto:rspazzoli@imolinfo.it">Raffaele Spazzoli</a>
 * @author <a href="mailto:acannone@imolinfo.it">Amedeo Cannone</a>
 */
public class ConnectionException extends Jbi4cicsException {
    
    
    /**
     * Serial Version UID.
     */
    private static final long serialVersionUID = -8836081596680996553L;
    
    public ConnectionException(String message) {
        super(message);
        // TODO Auto-generated constructor stub
    }
    
    public ConnectionException(String message, Throwable cause) {
        super(message, cause);
        // TODO Auto-generated constructor stub
    }
    
    public ConnectionException(Throwable cause) {
        super(cause);
        // TODO Auto-generated constructor stub
    }
    
    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     */
    public ConnectionException(String message, Object[] args) {
        super(message, args);
    }
    
    /**
     * A constructor with i18n support.
     *
     * @param   message  The message of the exception.
     * @param   args     The <code>MessageFormat</code> arguments.
     * @param   cause    The cause of the exception.
     */
    public ConnectionException(String message, Object[] args, Throwable cause) {
        super(message, args, cause);
    }
    
}
