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
 * Wrapper class for the SAG C++ HandleClient class
 * It is derived from sagapp.hpp.
 * 
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:48 $
 *
 */
public class HandleClient {
    /** 
     * This member is used to store the address of its SAG C++ object.
     * Would like to declare it as private since it should never be
     * referenced on the Java side.  However, it is referenced
     * by other classes on the JNI C++ side and is therefore declared
     * as public. (Harry - It doesn't have to be public, declare it as private).
     */
    private long peer = 0;

    /** 
     * Default constructor - make it private.
     * Caller needs to create the object from factory.
     */
    private HandleClient() {}

    /**
     * Method free.
     */
    private native void free();

    /**
     * @see java.lang.Object#finalize()
     */
    protected void finalize() {
        free();
    }

    /** 
     * This method will send a request message to SAG.  It invokes
     * the "putRequest" method of the associated C++ SAG
     * HandleClient class.
     * @return Returns a token associated with the request
     * @param message The request message to be sent
     * @throws ExcStatus on error
     */
    public native long putRequest(SAGJNIMessage message) throws ExcStatus;

    /**
     * 
     * This method will attempt to retrieve the first available response SAGJNIMessage object 
     * from SAG.  The reconciliation token of the associated response SAGJNIMessage object will 
     * be returned if successful.
     * 
     * 
     * @param response Response message that is received
     * @return Reconciliation returned by method "putRequest".
     * The method "getResponse" will return only the response associated
     * to this token.
     * @throws ExcStatus on error
     */
    public native long getAnyResponse(SAGJNIMessage response) throws ExcStatus;

    /**
     * 
     * This method will attempt to retrieve the first available response SAGJNIMessage object
     * from SAG.  A timeout value may be specified to control the maximum amount of time the
     * method will block for before returning.  The reconciliation token of the
     * associated response SAGJNIMessage object will be returned if successful.
     * 
     * 
     * @param timeout The maximum time to wait for a response
     * @param response The resulting response object.  The user must create and pass in their own
     * SAGJNIMessage object to call this method.
     * @return The reconciliation token value associated with the returned response SAGJNIMessage
     * object.
     * @throws ExcStatus on error
     */
    public native long getAnyResponse(long timeout, SAGJNIMessage response)
        throws ExcStatus;

    /**
     * 
     * This method will attempt to retrieve the response SAGJNIMessage object assicated with
     * the input reconciliation token.
     * 
     * 
     * @param token The token returned from a previous call to putRequest
     * @param response The resulting response object.  The user must create and pass in their own
     * SAGJNIMessage object to call this method.
     * @throws ExcStatus on error
     */
    public native void getResponse(long token, SAGJNIMessage response)
        throws ExcStatus;

    /**
     * 
     * This method will attempt to retrieve the response SAGJNIMessage object associated with
     * the input reconciliation token.  A timeout value may be specified to control
     * the length of time the method will block for before returning.
     * 
     * 
     * @param timeout The maximum time to wait for a response
     * @param token The token value returned from a previous call to putRequest
     * @param response The resulting response object.  The user must create and pass in their own
     * SAGJNIMessage object to call this method.
     * @throws ExcStatus on error
     */
    public native void getResponse(long timeout, long token, SAGJNIMessage response)
        throws ExcStatus;

    /**
     * 
     * This method will send the input request SAGJNIMessage object and return the resulting
     * response message stored in the input response SAGJNIMessage object.
     * 
     * 
     * @param request Request message
     * @param response The resulting response value of executing the request.  User must create and
     * pass in there own Message object
     * @return Corresponding reSAGMessagemessage
     * @throws ExcStatus on error
     */
    public native void call(SAGJNIMessage request, SAGJNIMessage response)
        throws ExcStatus;

    static {
        System.loadLibrary("stcsagjni");
    }
}
