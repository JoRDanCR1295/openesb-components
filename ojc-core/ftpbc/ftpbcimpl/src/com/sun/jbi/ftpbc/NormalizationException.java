/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc;

/**
 *
 * @author jfu
 */
public class NormalizationException extends Exception {

    public NormalizationException(String msg) {
        super(msg);
    }

    public NormalizationException(String msg, Exception e) {
        super(msg, e);
    }
}
