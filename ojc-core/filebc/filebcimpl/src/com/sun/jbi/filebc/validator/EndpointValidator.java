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
 * @(#)EndpointValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.validator;

import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.Endpoint.EndpointType;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.util.EPUtil;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

/**
 * This class performs validation for endpoints.
 *
 * @author Sherry Weng
 */
public class EndpointValidator {

    private static final Messages mMessages =
            Messages.getMessages(EndpointValidator.class);
    private static Logger mLogger = Messages.getLogger(EndpointValidator.class);

    public static void validateEndpointForUniqueness(Collection<Endpoint> endpoints, Endpoint aEndpoint, boolean endpointValidated) throws Exception {
        if (!endpointValidated) {
            validateEndpoint(aEndpoint);
        }

        int endpointType = aEndpoint.getEndpointType();
        String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();

        for (Endpoint aDeployedEndpoint : endpoints) {
            if (aDeployedEndpoint.getEndpointType() != endpointType) {
                continue;
            } else {
                String aDeployedServiceEPRef = aDeployedEndpoint.getServiceName().toString() + aDeployedEndpoint.getEndpointName();
                if (serviceEndpointRef.equals(aDeployedServiceEPRef)) {
                    throw new Exception(mMessages.getString("FILEBC-E00901.EPV_endpoint_already_exists",
                            new String[]{aEndpoint.getServiceName().toString(), aEndpoint.getEndpointName()}));
                }
                String aEndpointFileDir = EPUtil.getFileDirectory(aEndpoint);
                String aDeployedFileDir = EPUtil.getFileDirectory(aDeployedEndpoint);

                if (!aEndpointFileDir.equals(aDeployedFileDir)) {
                    continue;
                }

                Set<Map.Entry<QName, FileOperation>> opEntries = aEndpoint.getFileOperations().entrySet();
                for (Map.Entry<QName, FileOperation> aEntry : opEntries) {
                    String opname = aEntry.getKey().toString();
                    FileOperation aOperation = aEntry.getValue();
                    String aFileInputResource = null;
                    String aFileOutputResource = null;
                    try {
                        aFileInputResource =
                                (aOperation.getFileOperationInput() != null) ? new File(aEndpointFileDir, aOperation.getFileOperationInput().getFileMessage().getFileName()).getCanonicalPath() : null;
                        aFileOutputResource =
                                (aOperation.getFileOperationOutput() != null) ? new File(aEndpointFileDir, aOperation.getFileOperationOutput().getFileMessage().getFileName()).getCanonicalPath() : null;
                    } catch (Exception ex) {
                        throw new Exception(mMessages.getString("FILEBC-E00902.EPV_invalid_file_resource", new String[]{opname, serviceEndpointRef, ex.getMessage()}));
                    }

                    Set<Map.Entry<QName, FileOperation>> deployedOpEntries = aDeployedEndpoint.getFileOperations().entrySet();

                    for (Map.Entry<QName, FileOperation> aDeployedEntry : deployedOpEntries) {
                        String aDeployedOpname = aDeployedEntry.getKey().toString();
                        FileOperation aDeployeOperation = aDeployedEntry.getValue();

                        String aDeployedFileInputResource = null;
                        String aDeployedFileOutputResource = null;

                        try {
                            aDeployedFileInputResource =
                                    (aDeployeOperation.getFileOperationInput() != null) ? new File(aDeployedFileDir, aDeployeOperation.getFileOperationInput().getFileMessage().getFileName()).getCanonicalPath() : null;
                            aDeployedFileOutputResource = (aDeployeOperation.getFileOperationOutput() != null) ? new File(aDeployedFileDir, aDeployeOperation.getFileOperationOutput().getFileMessage().getFileName()).getCanonicalPath() : null;
                        } catch (Exception ex) {
                            throw new Exception(mMessages.getString("FILEBC-E00902.EPV_invalid_file_resource", new String[]{aDeployedOpname, aDeployedServiceEPRef, ex.getMessage()}));
                        }

                        if (aFileInputResource != null && aFileInputResource.equals(aDeployedFileInputResource)) {
                            throw new Exception(mMessages.getString("FILEBC-E00903.EPV_Invalid_duplicate_file_resource",
                                    new String[]{opname, serviceEndpointRef, aDeployedFileInputResource}));
                        }

                        if (aFileOutputResource != null &&
                                aDeployedFileOutputResource != null &&
                                aFileOutputResource.equals(aDeployedFileOutputResource)) {

                            if (!(FileOperation.VERB_READ.equals(aOperation.getVerb()) || FileOperation.VERB_READ.equals(aDeployeOperation.getVerb()))) {

                                //throw exception only if none of the operations is solict-read.

                                throw new Exception(mMessages.getString("FILEBC-E00903.EPV_Invalid_duplicate_file_resource",
                                        new String[]{opname, serviceEndpointRef, aDeployedFileOutputResource}));

                            }
                        }

                        String aEPDirPath = new File(aEndpointFileDir).getCanonicalPath();
                        String aDeployedDirPath = new File(aDeployedFileDir).getCanonicalPath();
//                        boolean isPattern = aOperation.getFileOperationInput().getFileMessage().getFileNameIsPattern()
//                             || aDeployeOperation.getFileOperationInput().getFileMessage().getFileNameIsPattern();

                        /**
                         * jim.fu@SUN.COM:
                         *
                         * note that this overlap checking might not be able to prevent the conflicts as listed below
                         * (1) two endpoints polling for same set of files
                         * (2) two endpoints writting to the same set of files
                         * (3) one endpoint writes and another endpoint read from the same file
                         *
                         * Since there are NM properties for fileDirectory and fileName, and they can be set by application
                         * code dynamically
                         *
                         * Also, (2) and (3) are over protective:
                         *
                         * for example, using UUID pattern in a output name, two endpoints can write files into the same
                         * destination directory without overwriting each other's outputs, using a global persisted sequence
                         * can also achieve the same.
                         *
                         * by turning on outbound staging (by default - it is enabled)
                         */
                        if (aEPDirPath.equals(aDeployedDirPath)) {
                            // If the dirs are the same, check for overlapping file-names.
                            checkFileNamePatternOverlap(aDeployedEndpoint, aDeployeOperation, aEndpoint, aOperation);
                        }

                    }
                }
            }
        }
    }


    /*
     * Multiple endpoints are allowed to poll/write/read(solicited) from/to the same dir. 
     * But situations where they might step on each-other's data has to be prevented.
     */
    private static void checkFileNamePatternOverlap(Endpoint aDeployedEndpoint,
            FileOperation aDeployedOperation, Endpoint aEndpoint,
            FileOperation aOperation) throws Exception {

        boolean overlap = false;

        if (EndpointType.INBOUND == aDeployedEndpoint.getEndpointType()) {

            if (EndpointType.INBOUND == aEndpoint.getEndpointType()) {
                FileNamePatternOverlapValidator.checkOverlapInIn(aDeployedEndpoint, aDeployedOperation, aEndpoint, aOperation);
            } else if (EndpointType.OUTBOUND == aEndpoint.getEndpointType()) {
                FileNamePatternOverlapValidator.checkOverlapInOut(aDeployedEndpoint, aDeployedOperation, aEndpoint, aOperation);
            }

        }

        if (EndpointType.OUTBOUND == aDeployedEndpoint.getEndpointType()) {
            if (EndpointType.INBOUND == aEndpoint.getEndpointType()) {
                if (FileOperation.VERB_READ.equals(aDeployedOperation.getVerb())) {
                    //aDeployedOperation is solicited-read and aOperation can be one-way or request-response poll.
                    //check that they do not read from the same file, because poll would remove the file after
                    //reading it.
                    FileNamePatternOverlapValidator.checkOverlapOutIn(aDeployedEndpoint, aDeployedOperation, aEndpoint, aOperation);
                }

            } else if (EndpointType.OUTBOUND == aEndpoint.getEndpointType()) {
                if (!FileOperation.VERB_READ.equals(aDeployedOperation.getVerb()) && !FileOperation.VERB_READ.equals(aOperation.getVerb())) {
                    // both operations are one-way writes
                    FileNamePatternOverlapValidator.checkOverlapInIn(aDeployedEndpoint, aDeployedOperation, aEndpoint, aOperation);
                }
            }
        }

    }

    public static void validateEndpoint(Endpoint aEndpoint) throws Exception {
        String aEndpointFileDir = EPUtil.getFileDirectory(aEndpoint);
        String serviceEndpointRef = aEndpoint.getServiceName().toString() + aEndpoint.getEndpointName();

        if (aEndpointFileDir == null) {
            throw new Exception(mMessages.getString("FILEBC-E00904.EPV_invalid_null_file_directory", serviceEndpointRef));
        }

        Set<Map.Entry<QName, FileOperation>> opEntries = aEndpoint.getFileOperations().entrySet();
        Map<QName, String> operationMeps = aEndpoint.getOperationMsgExchangePattern();

        if (aEndpoint.getEndpointType() == EndpointType.INBOUND) {
            for (Map.Entry<QName, FileOperation> aEntry : opEntries) {
                QName opname = aEntry.getKey();
                FileOperation aOperation = aEntry.getValue();

                String mep = (String) operationMeps.get(opname);
                if (mep == null) {
                    throw new Exception(mMessages.getString("FILEBC-E00905.EPV_Invalid_operation_mep", new String[]{opname.toString(), serviceEndpointRef}));
                }

                if (Endpoint.EndpointMessageType.IN_OUT.equals(mep)) {
                    FileInput input = aOperation.getFileOperationInput();
                    FileOutput output = aOperation.getFileOperationOutput();

                    if (input == null) {
                        throw new Exception(mMessages.getString("FILEBC-E00906.EPV_Invalid_missing_input", new String[]{opname.toString(), serviceEndpointRef}));
                    }
                    if (output == null) {
                        throw new Exception(mMessages.getString("FILEBC-E00907.EPV_Invalid_missing_output", new String[]{opname.toString(), serviceEndpointRef}));
                    }
                } else if (Endpoint.EndpointMessageType.IN_ONLY.equals(mep)) {
                    FileInput input = aOperation.getFileOperationInput();
                    FileOutput output = aOperation.getFileOperationOutput();

                    if (input == null) {
                        throw new Exception(mMessages.getString("FILEBC-E00906.EPV_Invalid_missing_input", new String[]{opname.toString(), serviceEndpointRef}));
                    }
                    if (output != null) {
                        throw new Exception(mMessages.getString("FILEBC-E00908.EPV_Invalid_output", new String[]{opname.toString(), serviceEndpointRef}));
                    }
                } else {
                    throw new Exception(mMessages.getString("FILEBC-E00909.EPV_Invalid_MEP", new String[]{opname.toString(), serviceEndpointRef}));
                }
            }
        }
    }
}
