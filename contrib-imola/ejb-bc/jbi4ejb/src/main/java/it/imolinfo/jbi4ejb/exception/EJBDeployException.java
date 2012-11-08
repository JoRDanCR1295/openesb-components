/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.exception;


/**
 * Deploy exception for EJB BC.
 * 
 * @author marco
 */
@SuppressWarnings("serial")
public class EJBDeployException extends Jbi4EjbException {
    
    /**
     * Instantiates a new EJB deploy exception.
     * 
     * @param th the cause
     */
    public EJBDeployException(Throwable th) {
        super(th);
    }
    
    
    /**
     * Instantiates a new EJB deploy exception.
     * 
     * @param msg the message
     */
    public EJBDeployException(String msg) {
        super(msg);
    }
    

    /**
     * A constructor.
     *
     * @param    message        The message of the exception.
     * @param    cause        The cause of the exception.
     */
    public EJBDeployException(String message, Throwable cause) {
        super(message, cause);
    }
}
