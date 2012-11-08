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
package com.sun.jbi.swiftbc;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;
/**
 * A convenient place for the commonly-used SAG constants:
 * <pre>
 * 1. Defined in C++ header files.
 * 2. Used for message Envelope
 * 3. Used for SNL Primitives (from SNL specification)
 * </pre>
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
 */
public class SAGConstants {
    public static final QName QNAME_OPERATION = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", Constants.ELEM_OPERATION);
    public static final QName QNAME_BINDING = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", Constants.ELEM_BINDING);
    public static final QName QNAME_ADDRESS = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "address");
    public static final QName QNAME_PROTOCOLPROPERTIES = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "protocolproperties");
    public static final QName QNAME_MESSAGE = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "message");;
    
    public static final String XSD_FILENAME ="swift-ext.xsd";
    public static final String URL_SCHEME = "jar:file:";
    /**
     * ConnectionMode
     */
    public static final String CONNECTION_MODE_AUTOMATIC = "Automatic";
    
    public static final String CONNECTION_MODE_MANUAL = "Manual";
    
    /**
     * for C++ type MsTimeOut_t
     */
    // Indicates that the APIs have to return immediately. Used to do polling
    public static final long MS_TIMEOUT_NULL = 0;
    
    // Indicates that the APIs can wait without limit. Used to do a blocking
    // call
    public static final long MS_TIMEOUT_INFINITE = -1;
    
    /**
     * for C++ type Token_t
     */
    // This reserved value is used as a wildcard to indicate “any token”.
    public static final long TOKEN_ANY = 0;
    
    /**
     * for C++ type Severity
     */
    public static final int SEVERITY_SUCCESS = 0;
    
    public static final int SEVERITY_TRANSIENT = 1;
    
    public static final int SEVERITY_LOGIC = 2;
    
    public static final int SEVERITY_FATAL = 3;
    
    /**
     * for C++ type VisitInstruction
     */
    public static final int VISIT_PLUGIN = 0;
    
    public static final int VISIT_INTERFACE = 1;
    
    public static final int VISIT_RETURN = 2;
    
    /**
     * for C++ type Direction
     */
    public static final int DIR_REQUEST = 0;
    
    public static final int DIR_RESPONSE = 1;
    
    /**
     * For C++: Error codes defined for SAG binary compatibility
     */
    public static final int SWLIB_E_SUCCESS = 0;
    
    public static final int SWLIB_E_INTERNAL = 1;
    
    public static final int SWLIB_E_IS_CLIENT = 2;
    
    public static final int SWLIB_E_CALLED_TWICE = 3;
    
    public static final int SWLIB_E_NO_CALLBACKS = 4;
    
    public static final int SWLIB_E_BAD_DLL = 5;
    
    public static final int SWLIB_E_ACCESS_DLL = 6;
    
    public static final int SWLIB_E_ACCESS_CFG = 7;
    
    public static final int SWLIB_E_NO_VERSION = 8;
    
    public static final int SWLIB_E_FORMAT_CFG = 9;
    
    public static final int SWLIB_E_FORMAT_ARG = 10;
    
    public static final int SWLIB_E_PREVIOUS_ERROR = 11;
    
    /**
     * FileOpRequestConfig
     */
    public static final String FILE_OPERATION_PUT = "Put File";
    
    public static final String FILE_OPERATION_GET = "Get File";
    
    public static final String FILE_OPERATION_SNF_FETCH = "SnF Fetch File";
    
    public static final String FILE_OPERATION_ACK = "Ack File";
    
    /**
     * ReceiveMode
     */
    public static final String RECEIVE_MODE_REAL_TIME = "Real Time";
    
    public static final String RECEIVE_MODE_SNF = "Store and Forward";
    
    
    //
    //
    //****************************************
    // Constants For Envelope
    //****************************************
    //
    //
    
    /**
     * Envelope.ApplicationStatus
     */
    public final static String APPLICATION_STATUS_FAILURE = "FAILURE";
    
    public final static String APPLICATION_STATUS_SUCCESS = "SUCCESS";
    
    /**
     * Envelope.MsgFormat
     */
    public static final String MSG_FORMAT_DEFAULT = " "; // use SAG configured as "default"
    
    public static final String MSG_FORMAT_SNL = "Sag:SNL";
    
    public static final String MSG_FORMAT_RELAXED_SNL = "Sag:RelaxedSNL";
    
    public static final String MSG_FORMAT_BASIC_INTERACT = "Sag:BasicInterAct";
    
    public static final String MSG_FORMAT_PRIMITIVE = "Sag:Primitive";
    
    //
    //
    //****************************************
    // Constants For SNL Primitive
    //****************************************
    //
    //
    
    /**
     * Sw_Boolean
     */
    public static final String TRUE = "TRUE";
    
    public static final String FALSE = "FALSE";
    
    /**
     * SwGbl_Severity
     */
    public static final String SWGBL_SEVERITY_FATAL = "Fatal";
    
    public static final String SWGBL_SEVERITY_TRANSIENT = "Transient";
    
    public static final String SWGBL_SEVERITY_LOGIC = "Logic";
    
    public static final String SWGBL_SEVERITY_SUCCESS = "Success";
    
    public static final String SWGBL_SEVERITY_WARNING = "Warning";
    
    /**
     * SwInt_NRType
     */
    public static final String NR_TYPE_SVCMAND = "SVCMAND";
    
    public static final String NR_TYPE_SVCOPT = "SVCOPT";
    
    /**
     * SwInt_DeliveryMode
     */
    public static final String DELIVERY_MODE_SNF = "SnF";
    
    /**
     * SwInt_Priority
     */
    public static final String PRIORITY_NORMAL = "Normal";
    
    public static final String PRIORITY_URGENT = "Urgent";
    
    /**
     * SwInt_ValResult
     */
    public static final String VALIDATION_RESULT_SUCCESS = "Success";
    
    public static final String VALIDATION_RESULT_WARNING = "Warning";
    
    public static final String VALIDATION_RESULT_FATAL = "Fatal";
    
    /**
     * Sw_TransferAnswer for Sw:GetFileResponse and Sw:GetFileResponseHandle
     */
    public static final String TRANSFER_ANSWER_GETFILE_ACCEPTED = "Accepted";
    
    public static final String TRANSFER_ANSWER_GETFILE_REJECTED = "Rejected";
    
    /**
     * Sw_TransferAnswer for Sw:PutFileResponse and Sw:PutFileResponseHandle
     */
    public static final String TRANSFER_ANSWER_PUTFILE_ACCEPTED = "Accepted";
    
    public static final String TRANSFER_ANSWER_PUTFILE_REJECTED = "Rejected";
    
    public static final String TRANSFER_ANSWER_PUTFILE_DUPLICATED = "Duplicated";
    
    /**
     * Sw_FileStatus.Sw_TransferSide
     */
    public static final String TRANSFER_SIDE_REQUESTOR = "Requestor";
    
    public static final String TRANSFER_SIDE_RESPONDER = "Responder";
    
    /**
     * Sw_FileStatus.Sw_TransferType
     */
    public static final String TRANSFER_TYPE_PUT = "Put";
    
    public static final String TRANSFER_TYPE_GET = "Get";
    
    public static final String TRANSFER_TYPE_FETCH = "Fetch";
    
    /**
     * Sw_FileStatus.Sw_TransferDirection
     */
    public static final String TRANSFER_DIRECTION_INCOMING = "Incoming";
    
    public static final String TRANSFER_DIRECTION_OUTGOING = "Outgoing";
    
    /**
     * Sw_FileStatus.Sw_TransferStatus
     */
    public static final String TRANSFER_STATUS_INITIATED = "Initiated";
    
    public static final String TRANSFER_STATUS_ACCEPTED = "Accepted";
    
    public static final String TRANSFER_STATUS_ONGOING = "Ongoing";
    
    public static final String TRANSFER_STATUS_COMPLETED = "Completed";
    
    public static final String TRANSFER_STATUS_REJECTED = "Rejected";
    
    public static final String TRANSFER_STATUS_DUPLICATED = "Duplicated";
    
    public static final String TRANSFER_STATUS_ABORTED = "Aborted";
    
    public static final String TRANSFER_STATUS_FAILED = "Failed";
    
    public static final String TRANSFER_STATUS_UNKNOWN = "Unknown";
    
    /**
     * Sw_DeliveryStatus
     */
    public static final String DELIVERY_STATUS_ACCEPTED = "Accepted";
    
    public static final String DELIVERY_STATUS_REJECTED = "Rejected";
    
    public static final String DELIVERY_STATUS_DUPLICATED = "Duplicated";
    
    public static final String DELIVERY_STATUS_FAILED = "Failed";
    
    /**
     * Sw_SessionMode
     */
    public static final String SESSION_MODE_PULL = "Pull";
    
    public static final String SESSION_MODE_PUSH = "Push";
    
    /**
     * Sw_OrderBy
     */
    public static final String ORDER_BY_INTERACT = "InterAct";
    
    public static final String ORDER_BY_FILEACT = "FileAct";
    
    public static final String ORDER_BY_URGENT = "Urgent";
    
    public static final String ORDER_BY_FIFO = " ";
    
    
    /**
     * Sw_AcquiredStatus
     */
    public static final String ACQUIRED_STATUS_ACCEPTED = "Accepted";
    
    public static final String ACQUIRED_STATUS_REJECTED = "Rejected";
    
    /**
     * Sw_SnFRefType
     */
    public static final String SNF_REF_TYPE_INTERACT = "InterAct";
    
    public static final String SNF_REF_TYPE_FILEACT = "FileAct";
    
    /**
     * Sw_AcceptStatus for Sw:NotifySnFRequestHandle
     */
    public static final String ACCEPT_STATUS_NOTIFY_SNF_ACCEPTED = "Accepted";
    
    public static final String ACCEPT_STATUS_NOTIFY_SNF_REJECTED = "Rejected";
    
    public static final String ACCEPT_STATUS_NOTIFY_SNF_DUPLICATED = "Duplicated";
    
    public static final String ACCEPT_STATUS_NOTIFY_SNF_FAILED = "Failed";
    
    /**
     * Sw_AcceptStatus for Sw:AckMessage
     */
    public static final String ACCEPT_STATUS_ACK_MSG_ACCEPTED = "Accepted";
    
    public static final String ACCEPT_STATUS_ACK_MSG_REJECTED = "Rejected";
    
    public static final String ACCEPT_STATUS_ACK_MSG_DUPLICATED = "Duplicated";
    
    /**
     * Sw_SessionStatus
     */
    public static final String SESSION_STATUS_ACTIVE = "Active";
    
    public static final String SESSION_STATUS_ACQUIRING = "Acquiring";
    
    public static final String SESSION_STATUS_TIMEDOUT = "TimedOut";
    
    public static final String SESSION_STATUS_CLOSED = "Closed";
    
    /**
     * Sw_DigestAlgorithm
     */
    public static final String DIGEST_ALGORITHM_SHA1 = "SHA-1";
    
    //
    //
    //**************************************
    // getters to expose to users
    //*************************************
    //
    //
    /**
     * Gets the envelope message format.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_EnvelopeMsgFormat</CODE> - Returns EnvelopeMsgFormat.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_EnvelopeMsgFormat getEnvelopeMsgFormat() {
        return new C_EnvelopeMsgFormat();
    }
    
    /**
     * Gets the envelope application status
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_EnvelopeApplicationStatus</CODE> - Returns EnvelopeApplicationStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_EnvelopeApplicationStatus getEnvelopeApplicationStatus() {
        return new C_EnvelopeApplicationStatus();
    }
    
    /**
     * Gets Sw_Boolean.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_Boolean</CODE> - Returns Boolean.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_Boolean getSw_Boolean() {
        return new C_Sw_Boolean();
    }
    
    /**
     * Gets the NR (non-repudiation)type.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_SwInt_NRType</CODE> - Returns NRType.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_SwInt_NRType getSwInt_NRType() {
        return new C_SwInt_NRType();
    }
    
    /**
     * Gets the priority.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_SwInt_Priority</CODE> - Returns Priority.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_SwInt_Priority getSwInt_Priority() {
        return new C_SwInt_Priority();
    }
    
    /**
     * Gets the transfer answer.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_TransferAnswer</CODE> - Returns TransferAnswer.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_TransferAnswer getSw_TransferAnswer() {
        return new C_Sw_TransferAnswer();
    }
    
    /**
     * Gets the transfer type.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_TransferType</CODE> - Returns TransferType.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_TransferType getSw_TransferType() {
        return new C_Sw_TransferType();
    }
    
    /**
     * Gets the transfer status.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_TransferStatus</CODE> - Returns TransferStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_TransferStatus getSw_TransferStatus() {
        return new C_Sw_TransferStatus();
    }
    
    /**
     * Gets the session mode.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_SessionMode</CODE> - Returns SessionMode.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_SessionMode getSw_SessionMode() {
        return new C_Sw_SessionMode();
    }
    
    /**
     * Gets the aquire status.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_AcquiredStatus</CODE> - Returns AcquiredStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_AcquiredStatus getSw_AcquiredStatus() {
        return new C_Sw_AcquiredStatus();
    }
    
    /**
     * Gets the accept status.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_AcceptStatus</CODE> - Returns AcceptStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_AcceptStatus getSw_AcceptStatus() {
        return new C_Sw_AcceptStatus();
    }
    
    /**
     *
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_DigestAlgorithm</CODE> - Returns DigestAlgorithm.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_DigestAlgorithm getSw_DigestAlgorithm() {
        return new C_Sw_DigestAlgorithm();
    }
    
    /**
     * Gets the session status.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_SessionStatus</CODE> - Returns SessionStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_SessionStatus getSw_SessionStatus() {
        return new C_Sw_SessionStatus();
    }
    
    /**
     * Gets the SnF reference type.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_SnFRefType</CODE> - Returns SnFRefType.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_SnFRefType getSw_SnFRefType() {
        return new C_Sw_SnFRefType();
    }
    
    /**
     * Gets the OrderBy, indicates the order in which the messages
     * must be retrieved on the queue.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_OrderBy</CODE> - Returns OrderBy.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_OrderBy getSw_OrderBy() {
        return new C_Sw_OrderBy();
    }
    
    /**
     * Gets the delivery status.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_DeliveryStatus</CODE> - Returns DeliveryStatus.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_DeliveryStatus getSw_DeliveryStatus() {
        return new C_Sw_DeliveryStatus();
    }
    
    /**
     * Gets the transfer direction.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_TransferDirection</CODE> - Returns TransferDirection.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_TransferDirection getSw_TransferDirection() {
        return new C_Sw_TransferDirection();
    }
    
    /**
     * Gets the transfer side.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_Sw_TransferSide</CODE> - Returns TransferSide.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_Sw_TransferSide getSw_TransferSide() {
        return new C_Sw_TransferSide();
    }
    
    /**
     * Gets the value result.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_SwInt_ValResult</CODE> - Returns ValResult.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_SwInt_ValResult getSwInt_ValResult() {
        return new C_SwInt_ValResult();
    }
    
    /**
     * Gets the delivery mode.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_SwInt_DeliveryMode</CODE> - Returns DeliveryMode.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_SwInt_DeliveryMode getSwInt_DeliveryMode() {
        return new C_SwInt_DeliveryMode();
    }
    
    /**
     * Gets the Gbl_Severity.
     * <p>
     * <DL><DT><B>Parameters:</B><DD>None.</DL>
     * @return <CODE>C_SwGbl_Severity</CODE> - Returns Severity.
     * <DT><B>Throws:</B><DD>None.
     * @include
     */
    public final static C_SwGbl_Severity getSwGbl_Severity() {
        return new C_SwGbl_Severity();
    }
    
    /**
     * Constants for EnvelopeApplicationStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $   Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_EnvelopeApplicationStatus {
        /**
         * Returns APPLICATION_STATUS_FAILURE.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns APPLICATION_STATUS_FAILURE.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static String getFAILURE() {
            return APPLICATION_STATUS_FAILURE;
        }
        
        /**
         * Returns APPLICATION_STATUS_SUCCESS.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns APPLICATION_STATUS_SUCCESS.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static String getSUCCESS() {
            return APPLICATION_STATUS_SUCCESS;
        }
    }
    
    /**
     * Constants for EnvelopeMsgFormat
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_EnvelopeMsgFormat {
        /**
         * Returns MSG_FORMAT_DEFAULT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns MSG_FORMAT_DEFAULT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSagDefault() {
            return MSG_FORMAT_DEFAULT;
        }
        
        /**
         * Returns MSG_FORMAT_SNL.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns MSG_FORMAT_SNL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSagSNL() {
            return MSG_FORMAT_SNL;
        }
        
        /**
         * Returns MSG_FORMAT_RELAXED_SNL.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns MSG_FORMAT_RELAXED_SNL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSagRelaxedSNL() {
            return MSG_FORMAT_RELAXED_SNL;
        }
        
        /**
         * Returns MSG_FORMAT_BASIC_INTERACT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns MSG_FORMAT_BASIC_INTERACT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSagBasicInterAct() {
            return MSG_FORMAT_BASIC_INTERACT;
        }
        
        /**
         * Returns MSG_FORMAT_PRIMITIVE.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns MSG_FORMAT_PRIMITIVE.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSagPrimitive() {
            return MSG_FORMAT_PRIMITIVE;
        }
        
    }
    
    /**
     * Constants for Sw_Boolean
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_Boolean {
        /**
         * Returns TRUE for Sw_Boolean.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRUE for Sw_Boolean.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getTRUE() {
            return TRUE;
        }
        
        /**
         * Returns FALSE for Sw_Boolean.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns FALSE for Sw_Boolean.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFALSE() {
            return FALSE;
        }
        
    }
    
    /**
     * Constants for SwGbl_Severity
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_SwGbl_Severity {
        /**
         * Returns SWGBL_SEVERITY_FATAL.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SWGBL_SEVERITY_FATAL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFatal() {
            return SWGBL_SEVERITY_FATAL;
        }
        
        /**
         * Returns SWGBL_SEVERITY_TRANSIENT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SWGBL_SEVERITY_TRANSIENT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getTransient() {
            return SWGBL_SEVERITY_TRANSIENT;
        }
        
        /**
         * Returns SWGBL_SEVERITY_LOGIC.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SWGBL_SEVERITY_LOGIC.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getLogic() {
            return SWGBL_SEVERITY_LOGIC;
        }
        
        /**
         * Returns SWGBL_SEVERITY_SUCCESS.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SWGBL_SEVERITY_SUCCESS.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSuccess() {
            return SWGBL_SEVERITY_SUCCESS;
        }
        
        /**
         * Returns SWGBL_SEVERITY_WARNING.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SWGBL_SEVERITY_WARNING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getWarning() {
            return SWGBL_SEVERITY_WARNING;
        }
        
    }
    
    /**
     * Constants for SwInt_NRType
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_SwInt_NRType {
        /**
         * Returns NR_TYPE_SVCMAND.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns NR_TYPE_SVCMAND.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSVCMAND() {
            return NR_TYPE_SVCMAND;
        }
        
        /**
         * Returns NR_TYPE_SVCOPT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns NR_TYPE_SVCOPT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSVCOPT() {
            return NR_TYPE_SVCOPT;
        }
        
    }
    
    /**
     * Constants for SwInt_DeliveryMode
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_SwInt_DeliveryMode {
        /**
         * Returns DELIVERY_MODE_SNF.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DELIVERY_MODE_SNF.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSnF() {
            return DELIVERY_MODE_SNF;
        }
        
    }
    
    /**
     * Constants for SwInt_Priority
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_SwInt_Priority {
        /**
         * Returns PRIORITY_NORMAL.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns PRIORITY_NORMAL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getNormal() {
            return PRIORITY_NORMAL;
        }
        
        /**
         * Returns PRIORITY_URGENT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns PRIORITY_URGENT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getUrgent() {
            return PRIORITY_URGENT;
        }
        
    }
    
    /**
     * Constants for SwInt_ValResult
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_SwInt_ValResult {
        /**
         * Returns VALIDATION_RESULT_SUCCESS.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns VALIDATION_RESULT_SUCCESS.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSuccess() {
            return VALIDATION_RESULT_SUCCESS;
        }
        
        /**
         * Returns VALIDATION_RESULT_WARNING.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns VALIDATION_RESULT_WARNING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getWarning() {
            return VALIDATION_RESULT_WARNING;
        }
        
        /**
         * Returns VALIDATION_RESULT_FATAL.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns VALIDATION_RESULT_FATAL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFatal() {
            return VALIDATION_RESULT_FATAL;
        }
    }
    
    /**
     * Constants for Sw_TransferAnswer (for Sw_GetFile)
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferAnswer4Sw_GetFile {
        /**
         * Returns TRANSFER_ANSWER_GETFILE_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_ANSWER_GETFILE_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return TRANSFER_ANSWER_GETFILE_ACCEPTED;
        }
        
        /**
         * Returns TRANSFER_ANSWER_GETFILE_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_ANSWER_GETFILE_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return TRANSFER_ANSWER_GETFILE_REJECTED;
        }
        
    }
    
    /**
     * Constants for Sw_TransferAnswer (for Sw_PutFile)
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferAnswer4Sw_PutFile {
        /**
         * Returns TRANSFER_ANSWER_PUTFILE_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_ANSWER_PUTFILE_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return TRANSFER_ANSWER_PUTFILE_ACCEPTED;
        }
        
        /**
         * Returns TRANSFER_ANSWER_PUTFILE_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_ANSWER_PUTFILE_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return TRANSFER_ANSWER_PUTFILE_REJECTED;
        }
        
        /**
         * Returns TRANSFER_ANSWER_PUTFILE_DUPLICATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_ANSWER_PUTFILE_DUPLICATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getDuplicated() {
            return TRANSFER_ANSWER_PUTFILE_DUPLICATED;
        }
        
    }
    
    /**
     * Constants for Sw_TransferAnswer
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferAnswer {
        /**
         * Returns TransferAnswer for GetFile.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>C_Sw_TransferAnswer4Sw_GetFile</CODE> - Returns TransferAnswer for GetFile.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static C_Sw_TransferAnswer4Sw_GetFile getGetFile() {
            return new C_Sw_TransferAnswer4Sw_GetFile();
        }
        
        /**
         * Returns TransferAnswer for PutFile.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>C_Sw_TransferAnswer4Sw_PutFile</CODE> - Returns TransferAnswer for PutFile.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static C_Sw_TransferAnswer4Sw_PutFile getPutFile() {
            return new C_Sw_TransferAnswer4Sw_PutFile();
        }
    }
    
    /**
     * Constants for Sw_TransferSide
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferSide {
        /**
         * Returns TRANSFER_SIDE_REQUESTOR.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_SIDE_REQUESTOR.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRequestor() {
            return TRANSFER_SIDE_REQUESTOR;
        }
        
        /**
         * Returns TRANSFER_SIDE_RESPONDER.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_SIDE_RESPONDER.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getResponder() {
            return TRANSFER_SIDE_RESPONDER;
        }
        
    }
    
    /**
     * Constants for Sw_TransferType
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferType {
        /**
         * Returns TRANSFER_TYPE_PUT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_TYPE_PUT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getPut() {
            return TRANSFER_TYPE_PUT;
        }
        
        /**
         * Returns TRANSFER_TYPE_GET.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_TYPE_GET.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getGet() {
            return TRANSFER_TYPE_GET;
        }
        
        /**
         * Returns TRANSFER_TYPE_FETCH.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_TYPE_FETCH.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFetch() {
            return TRANSFER_TYPE_FETCH ;
        }
        
    }
    
    /**
     * Constants for Sw_TransferDirection
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferDirection {
        /**
         * Returns TRANSFER_DIRECTION_INCOMING.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_DIRECTION_INCOMING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getIncoming() {
            return TRANSFER_DIRECTION_INCOMING;
        }
        
        /**
         * Returns TRANSFER_DIRECTION_OUTGOING.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_DIRECTION_OUTGOING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getOutgoing() {
            return TRANSFER_DIRECTION_OUTGOING;
        }
        
    }
    
    /**
     * Constants for Sw_TransferStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_TransferStatus {
        /**
         * Returns TRANSFER_STATUS_INITIATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_INITIATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getInitiated() {
            return TRANSFER_STATUS_INITIATED;
        }
        
        /**
         * Returns TRANSFER_STATUS_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return TRANSFER_STATUS_ACCEPTED;
        }
        
        /**
         * Returns TRANSFER_STATUS_ONGOING.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_ONGOING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getOngoing() {
            return TRANSFER_STATUS_ONGOING;
        }
        
        /**
         * Returns TRANSFER_STATUS_COMPLETED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_COMPLETED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getCompleted() {
            return TRANSFER_STATUS_COMPLETED;
        }
        
        /**
         * Returns TRANSFER_STATUS_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return TRANSFER_STATUS_REJECTED;
        }
        
        /**
         * Returns TRANSFER_STATUS_DUPLICATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_DUPLICATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getDuplicated() {
            return TRANSFER_STATUS_DUPLICATED;
        }
        
        /**
         * Returns TRANSFER_STATUS_ABORTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_ABORTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAborted() {
            return TRANSFER_STATUS_ABORTED;
        }
        
        /**
         * Returns TRANSFER_STATUS_FAILED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_FAILED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFailed() {
            return TRANSFER_STATUS_FAILED;
        }
        
        /**
         * Returns TRANSFER_STATUS_UNKNOWN.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns TRANSFER_STATUS_UNKNOWN.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getUnknown() {
            return TRANSFER_STATUS_UNKNOWN;
        }
        
    }
    
    /**
     * Constants for Sw_DeliveryStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_DeliveryStatus {
        /**
         * Returns DELIVERY_STATUS_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DELIVERY_STATUS_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return DELIVERY_STATUS_ACCEPTED;
        }
        
        /**
         * Returns DELIVERY_STATUS_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DELIVERY_STATUS_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return DELIVERY_STATUS_REJECTED;
        }
        
        /**
         * Returns DELIVERY_STATUS_DUPLICATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DELIVERY_STATUS_DUPLICATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getDuplicated() {
            return DELIVERY_STATUS_DUPLICATED;
        }
        
        /**
         * Returns DELIVERY_STATUS_FAILED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DELIVERY_STATUS_FAILED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFailed() {
            return DELIVERY_STATUS_FAILED;
        }
        
    }
    
    /**
     * Constants for Sw_SessionMode
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_SessionMode {
        /**
         * Returns SESSION_MODE_PULL
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_MODE_PULL.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getPull() {
            return SESSION_MODE_PULL;
        }
        
        /**
         * Returns SESSION_MODE_PUSH.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_MODE_PUSH.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getPush() {
            return SESSION_MODE_PUSH;
        }
        
    }
    
    /**
     * Constants for Sw_OrderBy
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_OrderBy {
        /**
         * Returns ORDER_BY_INTERACT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ORDER_BY_INTERACT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getInterAct() {
            return ORDER_BY_INTERACT;
        }
        
        /**
         * Returns ORDER_BY_FILEACT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ORDER_BY_FILEACT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFileAct() {
            return ORDER_BY_FILEACT;
        }
        
        /**
         * Returns ORDER_BY_URGENT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ORDER_BY_URGENT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getUrgent() {
            return ORDER_BY_URGENT;
        }
        
        /**
         * Returns ORDER_BY_FIFO.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ORDER_BY_FIFO.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFIFO() {
            return ORDER_BY_FIFO;
        }
        
    }
    
    /**
     * Constants for Sw_AcquiredStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_AcquiredStatus {
        /**
         * Returns ACQUIRED_STATUS_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACQUIRED_STATUS_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return ACQUIRED_STATUS_ACCEPTED;
        }
        
        /**
         * Returns ACQUIRED_STATUS_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACQUIRED_STATUS_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return ACQUIRED_STATUS_REJECTED;
        }
        
    }
    
    /**
     * Constants for Sw_SnFRefType
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_SnFRefType {
        /**
         * Returns SNF_REF_TYPE_INTERACT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SNF_REF_TYPE_INTERACT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getInterAct() {
            return SNF_REF_TYPE_INTERACT;
        }
        
        /**
         * Returns SNF_REF_TYPE_FILEACT.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SNF_REF_TYPE_FILEACT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFileAct() {
            return SNF_REF_TYPE_FILEACT;
        }
        
    }
    
    /**
     * Constants for Sw_AcceptStatus (for Sw_NotifySnF)
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_AcceptStatus4Sw_NotifySnF {
        /**
         * Returns ACCEPT_STATUS_NOTIFY_SNF_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_NOTIFY_SNF_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return ACCEPT_STATUS_NOTIFY_SNF_ACCEPTED;
        }
        
        /**
         * Returns ACCEPT_STATUS_NOTIFY_SNF_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_NOTIFY_SNF_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return ACCEPT_STATUS_NOTIFY_SNF_REJECTED;
        }
        
        /**
         * Returns ACCEPT_STATUS_NOTIFY_SNF_DUPLICATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_NOTIFY_SNF_DUPLICATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getDuplicated() {
            return ACCEPT_STATUS_NOTIFY_SNF_DUPLICATED;
        }
        
        /**
         * Returns ACCEPT_STATUS_NOTIFY_SNF_FAILED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_NOTIFY_SNF_FAILED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getFailed() {
            return ACCEPT_STATUS_NOTIFY_SNF_FAILED;
        }
        
    }
    
    /**
     * Constants for Sw_AcceptStatus (for Sw_AckMessage)
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_AcceptStatus4Sw_AckMessage {
        /**
         * Returns ACCEPT_STATUS_ACK_MSG_ACCEPTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_ACK_MSG_ACCEPTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAccepted() {
            return ACCEPT_STATUS_ACK_MSG_ACCEPTED;
        }
        
        /**
         * Returns ACCEPT_STATUS_ACK_MSG_REJECTED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_ACK_MSG_REJECTED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getRejected() {
            return ACCEPT_STATUS_ACK_MSG_REJECTED;
        }
        
        /**
         * Returns ACCEPT_STATUS_ACK_MSG_DUPLICATED.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns ACCEPT_STATUS_ACK_MSG_DUPLICATED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getDuplicated() {
            return ACCEPT_STATUS_ACK_MSG_DUPLICATED;
        }
        
    }
    
    /**
     * Constants for Sw_AcceptStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_AcceptStatus {
        /**
         * Returns AcceptStatus for NotifySnF.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>C_Sw_AcceptStatus4Sw_NotifySnF</CODE> - Returns AcceptStatus for NotifySnF.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static C_Sw_AcceptStatus4Sw_NotifySnF getSw_NotifySnF() {
            return new C_Sw_AcceptStatus4Sw_NotifySnF();
        }
        
        /**
         * Returns AcceptStatus for AckMessage.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>C_Sw_AcceptStatus4Sw_AckMessage</CODE> - Returns AcceptStatus for AckMessage.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public final static C_Sw_AcceptStatus4Sw_AckMessage getSw_AckMessage() {
            return new C_Sw_AcceptStatus4Sw_AckMessage();
        }
    }
    
    /**
     * Constants for Sw_SessionStatus
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_SessionStatus {
        /**
         * Returns active session status.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_STATUS_ACTIVE.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getActive() {
            return SESSION_STATUS_ACTIVE;
        }
        
        /**
         * Returns acquiring session status.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_STATUS_ACQUIRING.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getAcquiring() {
            return SESSION_STATUS_ACQUIRING;
        }
        
        /**
         * Returns the session status timeout.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_STATUS_TIMEDOUT.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getTimedOut() {
            return SESSION_STATUS_TIMEDOUT;
        }
        
        /**
         * Returns the session status.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns SESSION_STATUS_CLOSED.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getClosed() {
            return SESSION_STATUS_CLOSED;
        }
        
    }
    
    /**
     * Constants for Sw_DigestAlgorithm
     * @author Harry Liu (harry.liu@sun.com)
     * @version cvs revision: $Revision: 1.2 $ Last Modified: $Date: 2007/05/15 23:14:36 $
     */
    public static class C_Sw_DigestAlgorithm {
        /**
         * Gets the digest algorithm - SHA1.
         * <p>
         * <DL><DT><B>Parameters:</B><DD>None.</DL>
         * @return <CODE>String</CODE> - Returns DIGEST_ALGORITHM_SHA1.
         * <DT><B>Throws:</B><DD>None.
         * @include
         */
        public static final String getSHA1() {
            return DIGEST_ALGORITHM_SHA1;
        }
        
    }
    
}
