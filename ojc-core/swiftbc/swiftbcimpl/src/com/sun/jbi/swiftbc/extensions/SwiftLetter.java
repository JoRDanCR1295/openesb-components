/*
 * Letter.java
 *
 * Created on April 7, 2007, 7:18 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftLetter {
    public void clone(String letter);
      /** 
     * Set the contents of the letter
     * @param length the length of the buffer
     * @param buffer the buffer of the letter
     */
    public void set(int length, String buffer);

    /** 
     * returns the length of the buffer of the letter 
     * @return lonh
     */
    public int getLength();

    /** 
     * returns the buffer of the letter 
     * @return String
     */
    public String getBuffer();
}
