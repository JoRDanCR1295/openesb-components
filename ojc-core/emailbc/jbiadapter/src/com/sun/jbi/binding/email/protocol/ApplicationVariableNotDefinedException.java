/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.binding.email.protocol;

/**
 *
 * @author skini
 */
public class ApplicationVariableNotDefinedException extends Exception {

    /**
     * Creates a new instance of <code>ApplicationVariableNotDefinedException</code> without detail message.
     */
    public ApplicationVariableNotDefinedException() {
    }

    /**
     * Constructs an instance of <code>ApplicationVariableNotDefinedException</code> with the specified detail message.
     * @param msg the detail message.
     */
    public ApplicationVariableNotDefinedException(String msg) {
        super(msg);
    }

    /**
     * Constructs an instance of <code>ApplicationVariableNotDefinedException</code> with the specified detail message, and nested exception.
     * @param msg the detail message
     * @param t the nested exception
     */
    public ApplicationVariableNotDefinedException(String msg, Throwable t) {
        super(msg, t);
    }
}
