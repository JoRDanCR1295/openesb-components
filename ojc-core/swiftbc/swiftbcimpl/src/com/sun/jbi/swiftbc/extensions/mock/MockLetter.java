/*
 * MockLetter.java
 *
 * Created on April 7, 2007, 7:55 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;
import com.sun.jbi.swiftbc.extensions.SwiftLetter;
/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockLetter implements SwiftLetter{
    private String mockBuffer;
    private int length;
    /** Creates a new instance of MockLetter */
    public MockLetter() {
    }

    public void set(int length, String buffer) {
        this.length = length;
        mockBuffer = buffer;
    }

    public int getLength() {
        return length;
    }

    public String getBuffer() {
        return mockBuffer;
    }

    public void clone(String letter) {
        mockBuffer = letter;
        length = letter.length();
    }
    
}
