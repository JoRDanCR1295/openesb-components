/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.binding.email.protocol.send.smtp;

import com.sun.jbi.binding.email.protocol.*;

/**
 *
 * @author skini
 */
public class SMTPConfigurationException extends EmailBindingComponentConfigurationException {

    /**
     * Creates a new instance of <code>SMTPConfigurationException</code> without detail message.
     */
    public SMTPConfigurationException() {
    }


    /**
     * Constructs an instance of <code>SMTPConfigurationException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public SMTPConfigurationException(String msg) {
        super(msg);
    }
}
