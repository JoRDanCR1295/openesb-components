/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.processor;

import javax.xml.transform.Source;

/**
 * Wraps a source (with jbi-wrapping informations).
 */
public class Jbi4EjbMessage {
    
    /** The message source. */
    private Source messageSource = null;
    
    /** The is wrapped. */
    private boolean isWrapped = false;
    
    /**
     * Instantiates a new jbi4 ejb message.
     * 
     * @param message
     * @param isWrapped
     */
    public Jbi4EjbMessage(Source message, boolean isWrapped) {
        this.messageSource = message;
        this.isWrapped = isWrapped;
    }
    
    /**
     * Gets the message source.
     * 
     * @return the message source
     */
    public Source getMessageSource() {
        return messageSource;
    }

    /**
     * Sets the message source.
     * 
     * @param messageSource
     *            the new message source
     */
    public void setMessageSource(Source messageSource) {
        this.messageSource = messageSource;
    }

    /**
     * Checks if is wrapped.
     * 
     * @return true, if is wrapped
     */
    public boolean isWrapped() {
        return isWrapped;
    }

    /**
     * Sets the wrapped.
     * 
     * @param isWrapped
     *            the new wrapped
     */
    public void setWrapped(boolean isWrapped) {
        this.isWrapped = isWrapped;
    }        

}
