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
 * @(#)HL7InboundMessageValidationProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.extservice.exception.HL7ApplicationException;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extservice.ValidationInfo;
import com.sun.jbi.hl7bc.extservice.ack.ACKErrorCodes;
import com.sun.jbi.hl7bc.I18n;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import java.util.logging.Logger;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * This class validates the HL7 Inbound message
 * 
 * @author Raghunadh
 */

public class HL7InboundMessageValidationProcessor {

    private String logMsg;

    private static final Logger mLog = Logger.getLogger(HL7InboundMessageValidationProcessor.class.getName());

    private DOMSource domSource;
    // HL7 MSH segment validation - set to true to do MSH validation on required fields, false to
    // skip the MSH field checks
    private boolean validateMSH = false;

    private String msh1FieldSeparator;

    private String msh2EncodingCharacters;

    private String msh10MessageControlId;

    private String msh11ProcessingId;

    private String msh12VersionId;

    private ValidationInfo mValidationInfo;

    public ValidationInfo messageValidationProcess(Endpoint destination, Source source)
            throws HL7ApplicationException {
        
        HL7ProtocolProperties protocolProperties = destination.getHL7ProtocolProperties();
        mValidationInfo = new ValidationInfo();
        // ensure that the message received from the queue is not null
        if (source == null) {
             mLog.log(Level.SEVERE, I18n.msg("E0284: Input normalized message is null, unable to process the message"));
            logMsg = I18n.msg("E0284: Input normalized message is null, unable to process the message");
            throw new HL7ApplicationException(logMsg);
        }

        if (source instanceof DOMSource) {
            this.domSource = (DOMSource) source;
        }

        // check to see if MSH validation is required
        validateMSH = protocolProperties.getValidateMSHEnabled().booleanValue();

        try {
            if (validateMSH) {
                getMSHFieldValues(this.domSource);
                validateHL7Message(protocolProperties, mValidationInfo);
            }
        } catch (Exception ex) {
            //ignore exception
        }

        return mValidationInfo;

    }

    private void validateHL7Message(HL7ProtocolProperties protocolProperties, ValidationInfo validationInfo) throws Exception {

        if (mLog.isLoggable(Level.INFO)){
            mLog.log(Level.INFO, I18n.msg("I0163: Validating the HL7 message in Inbound"));
        }

        // validate and populate the required fields in the MSH segment
        if (!checkPopulateMSHRequiredFields(protocolProperties, validationInfo)) {
            mLog.log(Level.SEVERE, I18n.msg("E0285: MSH segment validation failed"));
            logMsg = I18n.msg("E0285: MSH segment validation failed");
            throw new Exception(logMsg);
        }

    }

    private boolean checkPopulateMSHRequiredFields(HL7ProtocolProperties protocolProperties,
                                                   ValidationInfo validationInfo) throws Exception {

        boolean valid = true;
        validationInfo.setValidationStatus(true);
        // Check MSH-1 field separator
        if (this.msh1FieldSeparator != null) {
            mLog.log(Level.INFO, I18n.msg("I0164: MSH-1 field separator Value is {0}",
                    msh1FieldSeparator ));
            // check MSH-2 encoding characters
            if (this.msh2EncodingCharacters != null) {
                mLog.log(Level.INFO, I18n.msg("I0165: MSH-2 encoding characters value is {0}",
                        msh2EncodingCharacters ));
                // check MSH-10 message control id
                if (this.msh10MessageControlId != null) {
                    mLog.log(Level.INFO, I18n.msg("I0166: MSH-10 message control id is {0}",
                            msh10MessageControlId ));
                // check MSH-11 processing id
                if (this.msh11ProcessingId != null) {
                    mLog.log(Level.INFO, I18n.msg("I0167: MSH-11 processing id is {0}",
                            msh11ProcessingId));
                    if (protocolProperties.getProcessingID() != null) {
                        if (this.msh11ProcessingId.equals(protocolProperties.getProcessingID())) {
                            mLog.log(Level.INFO, I18n.msg("I0168: MSH-11 processing ID : {0} validation successful",
                                     msh11ProcessingId ));
                            // Check for version id's match
                            if (this.msh12VersionId != null) {
                                mLog.log(Level.INFO, I18n.msg("I0169: MSH-12 version id is {0}",
                                        msh12VersionId ));
                                if (protocolProperties.getVersionID() != null) {
                                    if (this.msh12VersionId.equals(protocolProperties.getVersionID())) {
                                        mLog.log(Level.INFO,
                                                I18n.msg("I0170: MSH-12 version ID : {0} validation successful",
													msh12VersionId ));
                                    } else {
                                        valid = false;
                                        validationInfo.setErrorCode(ACKErrorCodes.UnsupportedVersionID.errorCode);
                                        validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedVersionID.errorMessage);
                                        validationInfo.setValidationStatus(false);
                                        mLog.log(Level.SEVERE,
                                                I18n.msg("E0286: MSH Validation failed; Version IDs DO NOT match {0}",
													msh12VersionId ));
                                    }

                                } else {
                                    valid = false;
                                    validationInfo.setErrorCode(ACKErrorCodes.UnsupportedVersionID.errorCode);
                                    validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedVersionID.errorMessage);
                                    validationInfo.setValidationStatus(false);
                                    mLog.log(Level.SEVERE,
                                            I18n.msg("E0287: Version ID is not set in the WSDL configuration, therefore invalidating the message"));
                                }

                            } else {
                                valid = false;
                                validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                                validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                                validationInfo.setValidationStatus(false);
                                mLog.log(Level.SEVERE, I18n.msg("E0288: MSH Validation failed; HL7 message does not have MSH-12 (version ID) value"));
                            }

                        } else {
                            valid = false;
                            validationInfo.setErrorCode(ACKErrorCodes.UnsupportedProcessingID.errorCode);
                            validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedProcessingID.errorMessage);
                            validationInfo.setValidationStatus(false);
                            mLog.log(Level.SEVERE, I18n.msg("E0289: MSH Validation failed; Processing IDs DO NOT match : {0}",
                                    msh11ProcessingId));
                        }

                    } else {
                        valid = false;
                        validationInfo.setErrorCode(ACKErrorCodes.UnsupportedProcessingID.errorCode);
                        validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedProcessingID.errorMessage);
                        validationInfo.setValidationStatus(false);
                        mLog.log(Level.SEVERE, I18n.msg("E0290: Processing ID is not set in the WSDL configuration, therefore invalidating the message"));
                    }
                } else {
                    valid = false;
                    validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                    validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                    validationInfo.setValidationStatus(false);
                    mLog.log(Level.SEVERE, I18n.msg("E0291: MSH Validation failed; HL7 message does not have MSH-11 (processing ID) value"));
                }

            } else {
                valid = false;
                validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                validationInfo.setValidationStatus(false);
                mLog.log(Level.SEVERE, I18n.msg("E0292: MSH Validation failed; HL7 message does not have MSH-10 (messagecontrolid) value"));
            }
			} else {
				valid = false;
				validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
				validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
				validationInfo.setValidationStatus(false);
				mLog.log(Level.SEVERE, I18n.msg("E0293: MSH Validation failed; HL7 message does not have MSH-2 (encoding characters) value"));
			}
        } else {
            valid = false;
            validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
            validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
            validationInfo.setValidationStatus(false);
            mLog.log(Level.SEVERE, I18n.msg("E0294: MSH Validation failed; HL7 message does not have MSH-1 (field separator) value"));
        }

        return valid;

    }

    public void getMSHFieldValues(DOMSource source) {
        Node rootNode = source.getNode();
        NodeList MSHNodeList = rootNode.getFirstChild().getFirstChild().getFirstChild().getFirstChild().getChildNodes();

        for (int i = 0; i < MSHNodeList.getLength(); i++) {
            Node mshField = MSHNodeList.item(i);
            String name = mshField.getLocalName();
            if (name.equals("MSH.1")) {
                this.msh1FieldSeparator = mshField.getFirstChild().getNodeValue();
                continue;
            } else if (name.equals("MSH.2")) {
                this.msh2EncodingCharacters = mshField.getFirstChild().getNodeValue();
                continue;
            } else if (name.equals("MSH.10")) {
                if (mshField.getFirstChild().getNodeValue() == null) {
                    // get the current time in milliseconds
                    long currentTimeMS = System.currentTimeMillis();
                    this.msh10MessageControlId = Long.toString(currentTimeMS);
                    mshField.getFirstChild().setNodeValue(this.msh10MessageControlId);
                } else {
                    this.msh10MessageControlId = mshField.getFirstChild().getNodeValue();
                }
                continue;
            } else if (name.equals("MSH.11")) {
                if(mshField.getFirstChild().getNodeValue() != null) {
                    //For HL7 versions - 2.1, 2.2
                    this.msh11ProcessingId = mshField.getFirstChild().getNodeValue();
                } else {
                    //For HL7 versions - 2.3, 2.3.1, 2.4, 2.5, and 2.5.1
                    this.msh11ProcessingId = mshField.getFirstChild().getFirstChild().getNodeValue();
                }
                continue;
            } else if (name.equals("MSH.12")) {
                if(mshField.getFirstChild().getNodeValue() != null) {
                    //For HL7 versions - 2.1, 2.2
                    this.msh12VersionId = mshField.getFirstChild().getNodeValue();
                } else {
                    //For HL7 versions - 2.3, 2.3.1, 2.4, 2.5, 2.5.1
                    this.msh12VersionId = mshField.getFirstChild().getFirstChild().getNodeValue();
                }
                continue;
            } 
        }
    }
}
