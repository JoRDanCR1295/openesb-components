/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc;

/**
 *
 * @author jfu
 */
public class EncoderNotFoundException extends Exception {

    public EncoderNotFoundException(String msg) {
        super(msg);
    }

    public EncoderNotFoundException(String msg, Exception e) {
        super(msg, e);
    }
}
