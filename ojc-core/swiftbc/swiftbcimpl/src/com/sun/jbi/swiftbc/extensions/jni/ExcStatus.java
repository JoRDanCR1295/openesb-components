/************************************************************************************
*
*   Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara, 
*   California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has 
*   intellectual property rights relating to technology embodied in the product 
*   that is described in this document. In particular, and without limitation, 
*   these intellectual property rights may include one or more of the U.S. patents 
*   listed at http://www.sun.com/patents and one or more additional patents or 
*   pending patent applications in the U.S. and in other countries. THIS PRODUCT 
*   CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC. 
*   USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN 
*   PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial 
*   software.  Government users are subject to the Sun Microsystems, Inc. standard 
*   license agreement and applicable provisions of the FAR and its supplements.  
*   Use is subject to license terms.  This distribution may include materials 
*   developed by third parties. Sun, Sun Microsystems, the Sun logo, Java 
*   Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
*   eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
*   Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are 
*   used under license and are trademarks or registered trademarks of SPARC 
*   International, Inc. in the U.S. and other countries. Products bearing SPARC 
*   trademarks are based upon architecture developed by Sun Microsystems, Inc. 
*   UNIX is a registered trademark in the U.S. and other countries, exclusively 
*   licensed through X/Open Company, Ltd. This product is covered and controlled by 
*   U.S. Export Control laws and may be subject to the export or import laws in 
*   other countries.  Nuclear, missile, chemical biological weapons or nuclear 
*   maritime end uses or end users, whether direct or indirect, are strictly 
*   prohibited.  Export or reexport to countries subject to U.S. embargo or to 
*   entities identified on U.S. export exclusion lists, including, but not limited 
*   to, the denied persons and specially designated nationals lists is strictly 
*   prohibited.
*
*************************************************************************************/
package com.sun.jbi.swiftbc.extensions.jni;

/**
 * ExcStatus represents an exception thrown from the JNI layer.  Unlike 
 * the other JNI-wrapped classes, this class is simply a container for the
 * data in the C++ equivalent ExcStatus class; it does NOT contain a pointer
 * to the original C++ ExcStatus class.  There were memory errors associated
 * with instantiating and deleting that class.
 * It is derived from sagappltypes.hpp.
 * 
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:48 $
 *
 */
public class ExcStatus extends Exception {

    private long token = -1;
    private int severity = -1;
    private String code;
    private String data;
    private String text;
    private String action;

    /**
     * Creates a new ExcStatus object
     *
     * @param        token the exception identifier
     * @param        severity the exception severity
     * @param        data exception data
     * @param        text explanation of the error condition
     * @param        action the action that caused the error
     */
    public ExcStatus(long token ,
                     int severity,
                     String code,
                     String data,
                     String text,
                     String action) {
        super(text);
        this.token = token;
        this.severity = severity;
        this.code = code;
        this.data = data;
        this.text = text;
        this.action = action;
    }

    /**
     * Static method to create and throw an ExcStatus exception.  This
     * is generally used for testing purposes.
     *
     * @param        token the exception identifier
     * @param        severity the exception severity
     * @param        data exception data
     * @param        text explanation of the error condition
     * @param        action the action that caused the error
     * @exception    ExcStatus the error to be thrown
     */
    public native static void throwit(long token,
                                      int severity,
                                      String code,
                                      String data,
                                      String text,
                                      String action) throws ExcStatus;

    /**
     * Method getToken
     *
     * @return long
     */
    public long getToken() {
        return token;
    }

    /**
     * Method getSeverity.
     * @return int
     */
    public int getSeverity() {
        return severity;
    }

    /**
     * Method getCode.
     * @return String
     */
    public String getCode() {
        return code;
    }

    /**
     * Method getData.
     * @return String
     */
    public String getData() {
        return data;
    }

    /**
     * Method getText.
     * @return String
     */
    public String getText() {
        return text;
    }

    /**
     * Method getAction.
     * @return String
     */
    public String getAction() {
        return action;
    }

    /**
     * Concatenates all the various exception fields into a human-readable
     * message
     *
     * @return       the error message
     */
    public String getMessage() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("Token:    " + getToken() + "\r\n");
        buffer.append("Severity: " + getSeverity() + "\r\n");
        buffer.append("Code:     " + getCode() + "\r\n");
        buffer.append("Data:     " + getData() + "\r\n");
        buffer.append("Text:     " + getText() + "\r\n");
        buffer.append("Action:   " + getAction() + "\r\n");
        return buffer.toString();
    }

    static {
        System.loadLibrary("stcsagjni");
    }
}
