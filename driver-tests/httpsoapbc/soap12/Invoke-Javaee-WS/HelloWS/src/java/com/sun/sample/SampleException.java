/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.sample;

/**
 *
 * @author sbiswas
 */
public class SampleException extends Exception {

    /**
     * Creates a new instance of <code>SampleException</code> without detail message.
     */
    public SampleException() {
    }


    /**
     * Constructs an instance of <code>SampleException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public SampleException(String msg) {
        super(msg);
    }
}
