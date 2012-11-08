/*
 * Message.java
 *
 * Created on April 7, 2007, 7:07 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftMessage extends javax.wsdl.extensions.ExtensibilityElement {
    public static final String USE_TYPE_ENCODED = "encoded";
    public static final String USE_TYPE_LITERAL = "literal";
    
    /**
     * Set the letter object for this message
     * @param letter Letter
     */
    void setLetter(SwiftLetter letter);
    
    /**
     * Set the envelope for this message
     * @param envelope Envelope
     */
    void setEnvelope(SwiftEnvelope envelope);
    
    /**
     * Return the letter object for this message
     * @return Letter
     */
    SwiftLetter getLetter();
    
    /**
     * Return the envelope for this message
     * @return Envelope
     */
    SwiftEnvelope getEnvelope();
    /**
     * Sets the use defined type.
     */
    void setUseType(String ut);
    /*
     * Return use type
     * @return String
     */
    String getUseType();
    
    /*
     * return encoding style
     * @return String
    */
    String getEncodingStyle();
    
    void setEncodingStyle(String es);
}
