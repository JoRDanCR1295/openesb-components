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
 * @(#)FileNamePatternValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.validator;

import java.util.logging.Logger;
import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.util.FileNamePatternUtil;
import com.sun.jbi.internationalization.Messages;

/*
 * Multiple endpoints are allowed to poll/write/read(solicited) from/to the same dir. 
 * But following situations are to be avoided:
 * 1. two endpoints polling the same file 
 * 2. two endpoints writing to the same file
 * 3. one endpoint polling and other endpoint reading (solicited) the same file
 * 
 * This class provides methods to help detect such overlaps for various combinations for Inbound 
 * and Outbound endpoints.
 */
public class FileNamePatternOverlapValidator {

    private static final Messages mMessages = Messages.getMessages(EndpointValidator.class);
    private static Logger mLogger = Messages.getLogger(FileNamePatternOverlapValidator.class);

    private FileNamePatternOverlapValidator() {}
    
    /*
     * Check file name pattern overlap between two inbound endpoints. 
     */
    public static void checkOverlapInIn(Endpoint inEp1,
            FileOperation inOp1, Endpoint inEp2,
            FileOperation inOp2) throws Exception {

        checkInputToInput(inEp1, inOp1, inEp2, inOp2);

        checkOutputToOutput(inEp1, inOp1, inEp2, inOp2);
    }

    /*
     * Check file name pattern overlap between a inbound and a outbound endpoint. 
     */
    public static void checkOverlapInOut(Endpoint inEp,
            FileOperation inOp, Endpoint outEp,
            FileOperation outOp) throws Exception {

        if (FileOperation.VERB_READ.equals(outOp.getVerb())) {

            checkInputToOutput(inEp, inOp, outEp, outOp);

        } else if (inOp.getFileOperationOutput() != null) {
            //inbound Operation is a request-response poll and
            //outbound Operation is a one-way write.

            //one-way write has the filename in the <input>, so pass outEp and
            //outOp as first two params.
            checkInputToOutput(outEp, outOp, inEp, inOp);
        }

    }

    /*
     * Check file name pattern overlap between a outbound and a inbound endpoint. 
     */
    public static void checkOverlapOutIn(Endpoint outEp,
            FileOperation outOp, Endpoint inEp,
            FileOperation inOp) throws Exception {

        if (FileOperation.VERB_READ.equals(outOp.getVerb())) {
            checkInputToOutput(inEp, inOp, outEp, outOp);
        } else {
            //outboundEP is a one-way write
            if (inOp.getFileOperationOutput() != null) {
                //inbound EP is a request-response, compare file written by
                //one-way write with that written by the response of the request-response.
                checkInputToOutput(outEp, outOp, inEp, inOp);
            }
        }

    }

    /*
     * Check file name pattern overlap between a outbound and a inbound endpoint. 
     */
    public static void checkOverlapOutOut(Endpoint outEp1,
            FileOperation outOp1, Endpoint outEp2,
            FileOperation outOp2) throws Exception {

        if (!FileOperation.VERB_READ.equals(outOp1.getVerb()) && !FileOperation.VERB_READ.equals(outOp2.getVerb())) {
            // if both operations are not read, it means both are write.
            // check that they do not write to the same file.
            checkInputToInput(outEp1, outOp1, outEp2, outOp2);
        }

    }

    private static void checkInputToInput(Endpoint ep1,
            FileOperation op1, Endpoint ep2,
            FileOperation op2) throws Exception {

        boolean overlap = false;
        if ((op1.getFileOperationInput() != null) &&
                (op2.getFileOperationInput() != null)) {

            boolean isPattern = op1.getFileOperationInput().getFileMessage().getFileNameIsPattern() || op2.getFileOperationInput().getFileMessage().getFileNameIsPattern();

            if (!isPattern) {
                return;
            }

            String filename1 = op1.getFileOperationInput().getFileMessage().getFileName();
            int epType1 = ep1.getEndpointType();
            String verb1 = op1.getVerb();

            String filename2 = op2.getFileOperationInput().getFileMessage().getFileName();
            int epType2 = ep2.getEndpointType();
            String verb2 = op2.getVerb();

            overlap = FileNamePatternUtil.checkOverlap(filename1, epType1, verb1,
                    filename2, epType2, verb2);

            if (overlap) {
                throw new FileNamePatternOverlapException(mMessages.getString("FILEBC-E00910.EPV_Overlapping_File_Name",
                        new String[]{filename2, ep2.getServiceName().toString(), ep2.getEndpointName(),
                            filename1, ep1.getServiceName().toString(), ep1.getEndpointName()}));
            }
        }

    }

    private static void checkOutputToOutput(Endpoint ep1,
            FileOperation op1, Endpoint ep2,
            FileOperation op2) throws Exception {

        boolean overlap = false;
        if ((op1.getFileOperationOutput() != null) &&
                (op2.getFileOperationOutput() != null)) {

            boolean isPattern = op1.getFileOperationOutput().getFileMessage().getFileNameIsPattern() || op2.getFileOperationOutput().getFileMessage().getFileNameIsPattern();

            if (!isPattern) {
                return;
            }

            String filename1 = op1.getFileOperationOutput().getFileMessage().getFileName();
            int epType1 = ep1.getEndpointType();
            String verb1 = op1.getVerb();

            String filename2 = op2.getFileOperationOutput().getFileMessage().getFileName();
            int epType2 = ep2.getEndpointType();
            String verb2 = op2.getVerb();

            overlap = FileNamePatternUtil.checkOverlap(filename1, epType1, verb1,
                    filename2, epType2, verb2);

            if (overlap) {
                throw new FileNamePatternOverlapException(mMessages.getString("FILEBC-E00910.EPV_Overlapping_File_Name",
                        new String[]{filename2, ep2.getServiceName().toString(), ep2.getEndpointName(),
                            filename1, ep1.getServiceName().toString(), ep1.getEndpointName()}));
            }
        }

    }

    private static void checkInputToOutput(Endpoint inEp,
            FileOperation inOp, Endpoint outEp,
            FileOperation outOp) throws Exception {

        boolean overlap = false;
        if ((inOp.getFileOperationInput() != null) &&
                (outOp.getFileOperationOutput() != null)) {

            boolean isPattern = inOp.getFileOperationInput().getFileMessage().getFileNameIsPattern() || outOp.getFileOperationOutput().getFileMessage().getFileNameIsPattern();

            if (!isPattern) {
                return;
            }

            String filename1 = inOp.getFileOperationInput().getFileMessage().getFileName();
            int epType1 = inEp.getEndpointType();
            String verb1 = inOp.getVerb();

            String filename2 = outOp.getFileOperationOutput().getFileMessage().getFileName();
            int epType2 = outEp.getEndpointType();
            String verb2 = outOp.getVerb();

            overlap = FileNamePatternUtil.checkOverlap(filename1, epType1, verb1,
                    filename2, epType2, verb2);

            if (overlap) {
                throw new FileNamePatternOverlapException(mMessages.getString("FILEBC-E00910.EPV_Overlapping_File_Name",
                        new String[]{filename2, outEp.getServiceName().toString(), outEp.getEndpointName(),
                            filename1, inEp.getServiceName().toString(), inEp.getEndpointName()}));
            }
        }

    }
}
