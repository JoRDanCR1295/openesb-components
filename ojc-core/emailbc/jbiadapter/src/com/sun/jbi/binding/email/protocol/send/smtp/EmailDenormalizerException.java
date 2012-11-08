/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.binding.email.protocol.send.smtp;

/**
 *
 * @author skini
 */
public class EmailDenormalizerException extends Exception {

    /**
     * Creates a new instance of <code>EmailNormalizerException</code> without detail message.
     */
    public EmailDenormalizerException() {
    }


    /**
     * Constructs an instance of <code>EmailNormalizerException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public EmailDenormalizerException(String msg) {
        super(msg);
    }
}
