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
 * @(#)ACKErrorCodes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

/**
 * This interface contains static classes contains message error condition codes and the corresonding error
 * message Texts. These error codes should be set to ERR-1
 * 
 * @author T.V.A.Raghunadh
 */

public interface ACKErrorCodes {

    /*
     * Success. Optional, As the AA conveys sucess. Used for systems that must always
     * return a status code.
     */
    public static class MessageAccepted {

        public static String errorCode = "0";

        public static String errorMessage = "Message accepted";

    }
    /*
     * error code for Segment sequence error. 
     */
    
    public static class SegmentSequenceError {

        public static String errorCode = "100";

        public static String errorMessage = "Segment sequence error";

    }
    
    /*
     * error code for Required Field Missing. 
     */
    
    public static class MissingRequiredField {

        public static String errorCode = "101";

        public static String errorMessage = "Required field missing";

    }
    
    /*
     * error code for Data Type Error. 
     */
    
    public static class DataTypeError {

        public static String errorCode = "102";

        public static String errorMessage = "Data type error";

    }
    /*
     * error code for Table Value not found. 
     */
    
    public static class TableValueNotFound {

        public static String errorCode = "103";

        public static String errorMessage = "Table value not found";

    }

    /*
     * error code for Unsupported message Type 
     */
    public static class UnsupportedMessageType {

        public static String errorCode = "200";

        public static String errorMessage = "Unsupported message type";

    }

    /*
     * error code for Unsupported event code 
     */
    public static class UnsupportedEventCode {

        public static String errorCode = "201";

        public static String errorMessage = "Unsupported event code";

    }

    /*
     * error code for Unsupported Processing ID 
     */
    public static class UnsupportedProcessingID {

        public static String errorCode = "202";

        public static String errorMessage = "Unsupported Processing ID";

    }

    /*
     *  error code for Unsupported Version ID 
     */
    public static class UnsupportedVersionID {

        public static String errorCode = "203";

        public static String errorMessage = "Unsupported Version ID";

    }

    /*
     *  error code for Unknown Key Identifier 
     */
    public static class UnknownKeyIdentifier {

        public static String errorCode = "204";

        public static String errorMessage = "Unknown key identifier";

    }

    /*
     *  error code for Duplicate Key Identifier 
     */
    public static class DuplicateKeyIdentifier {

        public static String errorCode = "205";

        public static String errorMessage = "Duplicate key identifier";

    }

    /*
     * error code for Application Record Locked 
     */
    public static class ApplicationRecordLocked {

        public static String errorCode = "206";

        public static String errorMessage = "Application record locked";

    }

    /*
     * error code for Application Internal Error 
     */
    public static class ApplicationInternalError {

        public static String errorCode = "207";

        public static String errorMessage = "Application internal error";

    }

}
