/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MSMQAPIReturnCode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

/**
 * @author Sun Microsystems
 */
public class MSMQAPIReturnCode {
    /**
     * Indicates success.
     */
    public static long MQ_OK = 0;

    /**
     * The action specified in dwAction does not agree with the access mode with which the queue was
     * opened.
     */
    public static long MQ_ERROR_ACCESS_DENIED = 0xC00E0025;

    public static String MQ_ERROR_ACCESS_DENIED_STR = "The access rights for opening or reading the queue with the access mode specified by are not allowed for the calling process.";

    /**
     * The buffer supplied for PROPID_M_BODY, PROPID_M_COMPOUND_MESSAGE, PROPID_M_EXTENSION, or
     * PROPID_M_SOAP_ENVELOPE is too small. Details can be retrieved from the aStatus array. In each
     * case, the portion of the property that fits is copied to the buffer, but the message is not
     * removed from the queue.
     */
    public static long MQ_ERROR_BUFFER_OVERFLOW = 0xC00E001A;

    public static String MQ_ERROR_BUFFER_OVERFLOW_STR = "The buffer supplied for is too small";

    /*
     * The lpwcsFormatName parameter specified an illegal format name.
     */
    public static long MQ_ERROR_ILLEGAL_FORMATNAME = 0xC00E001E;

    public static String MQ_ERROR_ILLEGAL_FORMATNAME_STR = "The FormatName parameter specified an illegal format name in openQueue";

    /*
     * Message Queuing was unable to connect to the MS DTC.
     */
    public static long MQ_ERROR_DTC_CONNECT = 0xC00E004C;

    public static String MQ_ERROR_DTC_CONNECT_STR = "Message Queuing was unable to connect to the MS DTC";

    /*
     * Message Queuing cannot find the queue. The queue may be a public queue not registered in the
     * directory service or an Internet queue that does not exist in the MSMQ namespace.
     */
    public static long MQ_ERROR_QUEUE_NOT_FOUND = 0xC00E0003;

    public static String MQ_ERROR_QUEUE_NOT_FOUND_STR = "Message Queuing cannot find the queue.";

    /*
     * The remote computer that hosts the queue being opened for reading messages is not available.
     */
    public static long MQ_ERROR_REMOTE_MACHINE_NOT_AVAILABLE = 0xC00E0069;

    public static String MQ_ERROR_REMOTE_MACHINE_NOT_AVAILABLE_STR = "he remote computer that hosts the queue being opened for reading messages is not available.";

    /*
     * The Message Queuing service is not available.
     */
    public static long MQ_ERROR_SERVICE_NOT_AVAILABLE = 0xC00E000B;

    public static String MQ_ERROR_SERVICE_NOT_AVAILABLE_STR = " The Message Queuing service is not available.";

    /*
     * Another process already opened this queue with dwShareMode set to MQ_DENY_RECEIVE_SHARE, or
     * another process has already opened the queue for receive so you can't specify
     * MQ_DENY_RECEIVE_SHARE.
     */
    public static long MQ_ERROR_SHARING_VIOLATION = 0xC00E0009;

    public static String MQ_ERROR_SHARING_VIOLATION_STR = "Another process already opened this queue with sharing mode conflicts with yours";

    /*
     * The access mode parameter (dwAccess) is set to an invalid value, or dwAcess is set to
     * MQ_SEND_MESSAGE and the share mode parameter (dwShareMode) is set to MQ_DENY_RECEIVE_SHARE.
     */
    public static long MQ_ERROR_UNSUPPORTED_ACCESS_MODE = 0xC00E0045;

    public static String MQ_ERROR_UNSUPPORTED_ACCESS_MODE_STR = "The access mode parameter is set to an invalid value Acess is set to MQ_SEND_MESSAGE and the share mode parameter is set to MQ_DENY_RECEIVE_SHARE ";

    /**
     * The supplied format name buffer is too small to hold the format name of the queue.
     */
    public static long MQ_ERROR_FORMATNAME_BUFFER_TOO_SMALL = 0xC00E001F;

    public static String MQ_ERROR_FORMATNAME_BUFFER_TOO_SMALL_STR = "The supplied format name buffer is too small to hold the format name of the queue.";

    /**
     * MQ_ACTION_PEEK_NEXT cannot be used when the current cursor position is at the end of the
     * queue.
     */
    public static long MQ_ERROR_ILLEGAL_CURSOR_ACTION = 0xC00E001C;

    public static String MQ_ERROR_ILLEGAL_CURSOR_ACTION_STR = "MQ_ACTION_PEEK_NEXT cannot be used when the current cursor position is at the end of the queue.";

    /**
     * One of the following message properties was specified (in pMessageProps) without its
     * associated length property: PROPID_M_ADMIN_QUEUE, PROPID_M_DEST_QUEUE, PROPID_M_LABEL,
     * PROPID_M_RESP_QUEUE, PROPID_M_XACT_STATUS_QUEUE, or PROPID_M_PROV_NAME.
     */
    public static long MQ_ERROR_INSUFFICIENT_PROPERTIES = 0xC00E003F;

    public static String MQ_ERROR_INSUFFICIENT_PROPERTIES_STR = "some message properties was specified without its associated length property";

    /**
     * The queue handle specified in hSource is not valid.
     */
    public static long MQ_ERROR_INVALID_HANDLE = 0xC00E0007;

    public static String MQ_ERROR_INVALID_HANDLE_STR = "The queue handle specified is not valid.";

    /**
     * No message was received within the time-out period specified by dwTimeout.
     */
    public static long MQ_ERROR_IO_TIMEOUT = 0xC00E001B;

    public static String MQ_ERROR_IO_TIMEOUT_STR = "No message was received within the time-out period specified";

    /**
     * The supplied message label buffer is too small to hold the label of the message.
     */
    public static long MQ_ERROR_LABEL_BUFFER_TOO_SMALL = 0xC00E005E;

    public static String MQ_ERROR_LABEL_BUFFER_TOO_SMALL_STR = "The supplied message label buffer is too small to hold the label of the message.";

    /**
     * A message that is currently pointed at by the cursor has been removed from the queue. It can
     * be removed by another process or by another call to MQReceiveMessage using a different
     * cursor, or the message time-to-be-received timer has expired.
     */
    public static long MQ_ERROR_MESSAGE_ALREADY_RECEIVED = 0xC00E001D;

    public static String MQ_ERROR_MESSAGE_ALREADY_RECEIVED_STR = "A message that is currently pointed at by the cursor has been removed from the queue. It can be removed by another process or by another call to MQReceiveMessage using a different cursor, or the message time-to-be-received timer has expired.";

    /**
     * The operation was canceled before it could be completed. For example, the queue handle was
     * closed by another thread while waiting for a message.
     */
    public static long MQ_ERROR_OPERATION_CANCELLED = 0xC00E0008;

    public static String MQ_ERROR_OPERATION_CANCELLED_STR = "The operation was canceled before it could be completed. For example, the queue handle was closed by another thread while waiting for a message.";

    /**
     * The supplied provider name buffer is too small to hold the cryptographic service provider's
     * name.
     */
    public static long MQ_ERROR_PROV_NAME_BUFFER_TOO_SMALL = 0xC00E0063;

    public static String MQ_ERROR_PROV_NAME_BUFFER_TOO_SMALL_STR = "The supplied provider name buffer is too small to hold the cryptographic service provider's name.";

    /**
     * One or more message properties specified in pMessageProps resulted in an error.
     */
    public static long MQ_ERROR_PROPERTY = 0xC00E0002;

    public static String MQ_ERROR_PROPERTY_STR = "One or more message properties specified resulted in an error.";

    /**
     * The queue was deleted before the message could be read. The specified queue handle is no
     * longer valid and the queue handle must be closed.
     */
    public static long MQ_ERROR_QUEUE_DELETED = 0xC00E005A;

    public static String MQ_ERROR_QUEUE_DELETED_STR = "The queue was deleted before the message could be read. ";

    /*
     * The supplied sender certificate buffer is too small to hold the user certificate.
     */
    public static long MQ_ERROR_SENDER_CERT_BUFFER_TOO_SMALL = 0xC00E002B;

    public static String MQ_ERROR_SENDER_CERT_BUFFER_TOO_SMALL_STR = "The supplied sender certificate buffer is too small to hold the user certificate. ";

    // The sender identifier buffer supplied is too small.
    public static long MQ_ERROR_SENDERID_BUFFER_TOO_SMALL = 0xC00E0022;

    public static String MQ_ERROR_SENDERID_BUFFER_TOO_SMALL_STR = "The sender identifier buffer supplied is too small. ";

    /*
     * The supplied signature buffer is too small to hold the message's digital signature.
     */
    public static long MQ_ERROR_SIGNATURE_BUFFER_TOO_SMALL = 0xC00E0062;

    public static String MQ_ERROR_SIGNATURE_BUFFER_TOO_SMALL_STR = "The supplied signature buffer is too small to hold the message's digital signature.";

    /*
     * The specified queue handle was obtained in a previous session of the Message Queuing service.
     * Close the queue and open it again to obtain a fresh handle.
     */
    public static long MQ_ERROR_STALE_HANDLE = 0xC00E0056;

    public static String MQ_ERROR_STALE_HANDLE_STR = "The specified queue handle was obtained in a previous session of the Message Queuing service. Close the queue and open it again to obtain a fresh handle.";

    /*
     * The supplied symmetric key buffer is too small to hold the symmetric key.
     */
    public static long MQ_ERROR_SYMM_KEY_BUFFER_TOO_SMALL = 0xC00E0061;

    public static String MQ_ERROR_SYMM_KEY_BUFFER_TOO_SMALL_STR = "The supplied symmetric key buffer is too small to hold the symmetric key.";

    /*
     * There are insufficient memory resources to complete operation for example, an attempt was
     * made to send a message containing more than 4 MB of data.
     */
    public static long MQ_ERROR_INSUFFICIENT_RESOURCES = 0xC00E0027;

    public static String MMQ_ERROR_INSUFFICIENT_RESOURCES_STR = "There are insufficient memory resources to complete operation.";

    /**
     * One of the following actions was attempted within the context of a transaction. An attempt
     * was made to open a remote queue for read access. An attempt was made to read a message from a
     * nontransactional queue. an attempt was made to read a message using a callback function or an
     * OVERLAPPED structure.
     */
    public static long MQ_ERROR_TRANSACTION_USAGE = 0xC00E0050;

    public static String MQ_ERROR_TRANSACTION_USAGE_STR = "One of the following actions was attempted within the context of a transaction.\n"
            + "An attempt was made to open a remote queue for read access.\n"
            + "An attempt was made to read a message from a nontransactional queue."
            + "an attempt was made to read a message using a callback function or an OVERLAPPED structure.";

    /*
     * An asynchronous operation is pending.
     */
    public static long MQ_INFORMATION_OPERATION_PENDING = 0x400E0006;

    public static String MQ_INFORMATION_OPERATION_PENDING_STR = "An asynchronous operation is pending.";

    public static long MQ_INFORMATION_PROPERTY = 0x400E0001;

    public static String MQ_INFORMATION_PROPERTY_STR = "";

    // One or more of the properties specified in pMessageProps resulted in a warning code even
    // though the function is completed.

    public static String MapMSMQCodeToStringMsg(long r) {

        if (r == MQ_ERROR_ACCESS_DENIED)
            return MQ_ERROR_ACCESS_DENIED_STR;
        else if (r == MQ_ERROR_SERVICE_NOT_AVAILABLE)
            return MQ_ERROR_SERVICE_NOT_AVAILABLE_STR;
        else if (r == MQ_ERROR_QUEUE_NOT_FOUND)
            return MQ_ERROR_QUEUE_NOT_FOUND_STR;
        else if (r == MQ_ERROR_TRANSACTION_USAGE)
            return MQ_ERROR_TRANSACTION_USAGE_STR;
        else if (r == MQ_ERROR_STALE_HANDLE)
            return MQ_ERROR_STALE_HANDLE_STR;
        else if (r == MQ_ERROR_QUEUE_DELETED)
            return MQ_ERROR_QUEUE_DELETED_STR;
        else if (r == MQ_ERROR_PROPERTY)
            return MQ_ERROR_PROPERTY_STR;
        else if (r == MQ_ERROR_INVALID_HANDLE)
            return MQ_ERROR_INVALID_HANDLE_STR;
        else if (r == MQ_ERROR_MESSAGE_ALREADY_RECEIVED)
            return MQ_ERROR_MESSAGE_ALREADY_RECEIVED_STR;
        else if (r == MQ_ERROR_OPERATION_CANCELLED)
            return MQ_ERROR_OPERATION_CANCELLED_STR;
        else
            return "Please look at Microsoft document for the error code explaination";
    }

}
