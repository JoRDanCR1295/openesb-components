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
 * @(#)SwiftInboundMessageValidationProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2006, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.swiftbc.extservice;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.swiftbc.Endpoint;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;
import com.sun.jbi.swiftbc.extservice.ValidationInfo;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.ApplicationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import java.util.logging.Logger;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * This class validates the Swift Inbound message
 * 
 * @author Raghunadh
 */

public class SwiftInboundMessageValidationProcessor {

    private String logMsg;
    
	private static final Messages mMessages = Messages.getMessages(SwiftInboundMessageValidationProcessor.class);

    private static final Logger mLog = Messages.getLogger(SwiftInboundMessageValidationProcessor.class);

    private DOMSource domSource;
    // Swift MSH segment validation - set to true to do MSH validation on required fields, false to
    // skip the MSH field checks
    private boolean validateMSH = false;

    private String msh1FieldSeparator;

    private String msh2EncodingCharacters;

    private String msh10MessageControlId;

    private String msh11ProcessingId;

    private String msh12VersionId;

    private ValidationInfo mValidationInfo;

    public ValidationInfo messageValidationProcess(Endpoint destination, Source source)
            throws ApplicationException {
        
        SwiftProtocolProperties protocolProperties = destination.getSwiftProtocolProperties();
        mValidationInfo = new ValidationInfo();
        // ensure that the message received from the queue is not null
        if (source == null) {
             mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Invalid_NMSG");
            logMsg = mMessages.getString("SwiftInboundMessageValidationProcessor_Invalid_NMSG");
            throw new ApplicationException(logMsg);
        }

        if (source instanceof DOMSource) {
            this.domSource = (DOMSource) source;
        }


        return mValidationInfo;

    }

    private void validateSwiftMessage(SwiftProtocolProperties protocolProperties, ValidationInfo validationInfo) throws Exception {

        if (mLog.isLoggable(Level.INFO)){
            mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_Inside_Validate_Message");
        }

        // validate and populate the required fields in the MSH segment
        if (!checkPopulateMSHRequiredFields(protocolProperties, validationInfo)) {
            mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Invalid_MSH_Data");
            logMsg = mMessages.getString("SwiftInboundMessageValidationProcessor_Invalid_MSH_Data");
            throw new Exception(logMsg);
        }

    }

    private boolean checkPopulateMSHRequiredFields(SwiftProtocolProperties protocolProperties,
                                                   ValidationInfo validationInfo) throws Exception {

        boolean valid = true;
        /*
        validationInfo.setValidationStatus(true);
        // Check MSH-1 field separator
        if (this.msh1FieldSeparator != null) {
            mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MSH_FieldSeparator",
                    new Object[] { msh1FieldSeparator });
            // check MSH-2 encoding characters
            if (this.msh2EncodingCharacters != null) {
                mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MSH_EncodingCharacter",
                        new Object[] { msh2EncodingCharacters });
                // check MSH-10 message control id
                if (this.msh10MessageControlId != null) {
                    mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MSH_MessageControlID",
                            new Object[] { msh10MessageControlId });
                // check MSH-11 processing id
                if (this.msh11ProcessingId != null) {
                    mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MSH_ProcessingID",
                            new Object[] { msh11ProcessingId });
                    if (protocolProperties.getProcessingID() != null) {
                        if (this.msh11ProcessingId.equals(protocolProperties.getProcessingID())) {
                            mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MATCH_MSH_ProcessingID",
                                    new Object[] { msh11ProcessingId });
                            // Check for version id's match
                            if (this.msh12VersionId != null) {
                                mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MSH_VersionID",
                                        new Object[] { msh12VersionId });
                                if (protocolProperties.getVersionID() != null) {
                                    if (this.msh12VersionId.equals(protocolProperties.getVersionID())) {
                                        mLog.log(Level.INFO,
                                                "SwiftInboundMessageValidationProcessor_MATCH_MSH_VersionID",
                                                new Object[] { msh12VersionId });
                                    } else {
                                        valid = false;
                                        validationInfo.setErrorCode(ACKErrorCodes.UnsupportedVersionID.errorCode);
                                        validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedVersionID.errorMessage);
                                        validationInfo.setValidationStatus(false);
                                        mLog.log(Level.SEVERE,
                                                "SwiftInboundMessageValidationProcessor_Invalid_VersionID",
                                                new Object[] { msh12VersionId });
                                    }

                                } else {
                                    valid = false;
                                    validationInfo.setErrorCode(ACKErrorCodes.UnsupportedVersionID.errorCode);
                                    validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedVersionID.errorMessage);
                                    validationInfo.setValidationStatus(false);
                                    mLog.log(Level.SEVERE,
                                            "SwiftInboundMessageValidationProcessor_Not_Configured_VersionID");
                                }

                            } else {
                                valid = false;
                                validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                                validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                                validationInfo.setValidationStatus(false);
                                mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Have_VersionID");
                            }

                        } else {
                            valid = false;
                            validationInfo.setErrorCode(ACKErrorCodes.UnsupportedProcessingID.errorCode);
                            validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedProcessingID.errorMessage);
                            validationInfo.setValidationStatus(false);
                            mLog.log(Level.INFO, "SwiftInboundMessageValidationProcessor_MATCH_MSH_ProcessingID",
                                    new Object[] { msh11ProcessingId });
                        }

                    } else {
                        valid = false;
                        validationInfo.setErrorCode(ACKErrorCodes.UnsupportedProcessingID.errorCode);
                        validationInfo.setErrorMessage(ACKErrorCodes.UnsupportedProcessingID.errorMessage);
                        validationInfo.setValidationStatus(false);
                        mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Configured_ProcessingID");
                    }
                } else {
                    valid = false;
                    validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                    validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                    validationInfo.setValidationStatus(false);
                    mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Have_ProcessingID");
                }

            } else {
                valid = false;
                validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
                validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
                validationInfo.setValidationStatus(false);
                mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Have_MessageControlID");
            }
			} else {
				valid = false;
				validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
				validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
				validationInfo.setValidationStatus(false);
				mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Have_EncodingCharacters");
			}
        } else {
            valid = false;
            validationInfo.setErrorCode(ACKErrorCodes.MissingRequiredField.errorCode);
            validationInfo.setErrorMessage(ACKErrorCodes.MissingRequiredField.errorMessage);
            validationInfo.setValidationStatus(false);
            mLog.log(Level.SEVERE, "SwiftInboundMessageValidationProcessor_Not_Have_FieldSeparator");
        }
*/
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
                    //For Swift versions - 2.1, 2.2
                    this.msh11ProcessingId = mshField.getFirstChild().getNodeValue();
                } else {
                    //For Swift versions - 2.3, 2.3.1, 2.4 and 2.5
                    this.msh11ProcessingId = mshField.getFirstChild().getFirstChild().getNodeValue();
                }
                continue;
            } else if (name.equals("MSH.12")) {
                if(mshField.getFirstChild().getNodeValue() != null) {
                    //For Swift versions - 2.1, 2.2
                    this.msh12VersionId = mshField.getFirstChild().getNodeValue();
                } else {
                    //For Swift versions - 2.3, 2.3.1, 2.4 and 2.5
                    this.msh12VersionId = mshField.getFirstChild().getFirstChild().getNodeValue();
                }
                continue;
            } 
        }
    }
}
