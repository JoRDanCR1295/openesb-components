/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.binding.email.protocol;

/**
 *
 * @author skini
 */
public class EmailBindingComponentConfigurationException extends Exception {

    /**
     * Creates a new instance of <code>EmailBindingComponentConfigurationException</code> without detail message.
     */
    public EmailBindingComponentConfigurationException() {
    }


    /**
     * Constructs an instance of <code>EmailBindingComponentConfigurationException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public EmailBindingComponentConfigurationException(String msg) {
        super(msg);
    }
}
