/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc;

/**
 *
 * @author jfu
 */
public class NormalizerException extends Exception {

    public NormalizerException(String msg) {
        super(msg);
    }

    public NormalizerException(String msg, Exception e) {
        super(msg, e);
    }

    public NormalizerException(String msg, Error e) {
        super(msg, e);
    }
}
