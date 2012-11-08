/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.binding.email.protocol.receive;

/**
 *
 * @author skini
 */
public class EmailNormalizerException extends Exception {

    /**
     * Creates a new instance of <code>EmailNormalizerException</code> without detail message.
     */
    public EmailNormalizerException() {
    }


    /**
     * Constructs an instance of <code>EmailNormalizerException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public EmailNormalizerException(String msg) {
        super(msg);
    }
}
