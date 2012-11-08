/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.exception;

/**
 * Exception for WSDL generation from EJB remote interface.
 * 
 * @author marco
 */
@SuppressWarnings("serial")
public class EJBWSDLGenerationException extends Jbi4EjbException {

    /**
     * Instantiates a new EJBWSDL generation exception.
     * 
     * @param th the Exception cause
     */
    public EJBWSDLGenerationException(Throwable th) {
        super(th);
    }
    
    /**
     * Instantiates a new EJBWSDL generation exception.
     * 
     * @param msg the Exception message
     */
    public EJBWSDLGenerationException(String msg) {
        super(msg);
    }
    
    /**
     * Instantiates a new EJBWSDL generation exception.
     * 
     * @param msg the Exception message
     */
    public EJBWSDLGenerationException(String msg,Throwable th) {
        super(msg,th);
    }    
}
