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
 * Wrapper class for the SAG C++ Handle class
 * It is derived from sagcontrol.hpp.
 * 
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $   Last Modified: $Date: 2007/04/20 05:37:47 $
 *
 */
public class Handle {

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
    private Handle() {}

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

    //newly-added for sag 5.0.0
    public native long putRequest(Primitive primitive)
        throws ExcStatus;
    public native long getAnyResponse(Primitive primitive)
        throws ExcStatus;
    public native void getResponse(long token, Primitive primitive)
        throws ExcStatus;
    public native long getAnyResponse(long msTimeOut, Primitive primitive)
        throws ExcStatus;
    public native void getResponse(long msTimeOut, long token, Primitive primitive)
        throws ExcStatus;
    public native void call(Primitive primitiveRequest, Primitive primitiveResponse)
        throws ExcStatus;

    static {
        System.loadLibrary("stcsagjni");
    }

    // the sag 4.0 stuff (check, start, stop, ..., archive) will be removed later
    /**
     * Validates the operator's password and returns the operating profile
     * of the operator.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @return       the operating profile as XML text
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native String check(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Starts the SAG system.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void start(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Stops the SAG system.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void stop(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Reinitializes the ProcessControl API.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void refresh(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Retrieves the status of the SAG System.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @param        level the status level to retrieve
     * @return       the status of the SAG as a XML text
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native String getStatus(String operator, String password,
    //                               String level)
    //    throws ExcStatus;
    

    /**
     * Stop tracing of the SAG system.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @exception    ExcStatus if any exception occurs    
     * @deprecated
     */
    //sag4.0
    //public native void traceReset(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Generates a random string
     *
     * @return       a random string suitable for use as a one time
     * password in the SAG system
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native String oneTimeString() throws ExcStatus;
    

    /**
     * Start tracing of the SAG system.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @param        oneTimePassword a string generated using the
     * oneTimeString() command
     * @param        traceSetting the trace level
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void traceSet(String operator, String password,
    //                            String oneTimePassword, String traceSetting)
    //    throws ExcStatus;
    

    /**
     * Verifies the integrity of the SAG system and produces a human
     * readable report.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @exception    ExcStatus if any exception occurs    
     * @deprecated
     */
    //sag4.0
    //public native String checkIntegrity(String operator, String password)
    //    throws ExcStatus;
    

    /**
     * Backs up the SAG database to the path on the file system.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @param        path the path where the backup is placed
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void backup(String operator, String password, String path)
    //    throws ExcStatus;
    

    /**
     * Dumps the log file to the given directory.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @param        path the path where the log file is placed
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void readLog(String operator, String password, String path)
    //    throws ExcStatus;
    

    /**
     * Archives the SAG database to the given fully-qualified archive name.
     *
     * @param        operator the operator name
     * @param        password the operator's password
     * @param        archiveFileName the fully-qualified archive name
     * @exception    ExcStatus if any exception occurs
     * @deprecated
     */
    //sag4.0
    //public native void archive(String operator, String password,
    //                           String archiveFileName)
    //    throws ExcStatus;
    
}
