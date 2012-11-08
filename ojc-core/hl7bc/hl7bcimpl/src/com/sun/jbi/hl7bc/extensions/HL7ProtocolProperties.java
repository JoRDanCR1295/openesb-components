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
 * @(#)HL7ProtocolProperties.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 * @author S. Nageswara Rao, Raghunadh
 */
public class HL7ProtocolProperties implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    public static final String MLLPV1 = "MLLPv1";

    public static final String ORIGINAL_ACK_MODE = "original";

    public static final Byte START_BLOCk_CHAR = 11;

    public static final Byte END_DATA_CHAR = 28;

    public static final Byte END_BLOCk_CHAR = 13;

    public static final Boolean IS_HLLP_CHECKSUM_ENABLED = false;

    public static final Boolean IS_SEQ_NUM_PROTOCOL_ENABLED = false;

    public static final Boolean IS_VALIDATE_MSH_ENABLED = false;

    public static final Boolean IS_JOURNALLING_ENABLED = false;
    public static final Byte FIELD_SEPARATOR_CHAR = 124;

    public static final String ENCODIND_CHARACTERS = "^~\\&";

    public static final String PROCESSING_ID = "D";

    public static final String VERSION_ID = "2.3.1";
    
    public static final Boolean ENABLED_MLLPV1PERSISTANCE = false;

    /*
     *  Default values for SHT segment attributes 
     */
    public static final Boolean ENABLED_SFT = false;

    public static final String SOFTWARE_VENDOR_ORGANIZATION = "Sun Microsystems, Inc.";

    public static final String SOFTWARE_CERTIFIED_VERSION = "2.0";

    public static final String SOFTWARE_PRODUCT_NAME = "Sun HL7 Binding Component";

    public static final String SOFTWARE_BINARY_ID = "2.0";

    public static final String SOFTWARE_PRODUCT_INFORMATION = "It is a binding component for HL7 over TCP/IP connection";

    public static final String SOFTWARE_INSTALLED_DATE = "";

    public static final String SENDING_APPLICATION = "Sun HL7 Binding Component";

    public static final String SENDING_FACILITY = "Sun HL7 Binding Component";

    /*
     *  Default values for UAC segment attributes 
     */
    public static final Boolean ENABLED_UAC = false;

    public static final String USER_AUTENTICATION_CREDENTIAL_TYPE_CODE = "";

    public static final String USER_AUTHENTICATION_CREDENTIAL = "";

    private QName mFieldElementType = HL7Constants.QNAME_PROTOCOLPROPERTIES;

    private String mAckMode = ORIGINAL_ACK_MODE; // default

    private String mLLPType = MLLPV1; // default

    private Byte mStartBlkChar = START_BLOCk_CHAR; // default

    private Byte mEndDataChar = END_DATA_CHAR; // default

    private Byte mEndBlkChar = END_BLOCk_CHAR; // default

    private Boolean mHLLPCheckSumEnabled = IS_HLLP_CHECKSUM_ENABLED; // default

    private Boolean mSeqNumProtocolEnabled = IS_SEQ_NUM_PROTOCOL_ENABLED;

    private Boolean mValidateMSH = IS_VALIDATE_MSH_ENABLED;
    private Boolean mJournallingEnabled = IS_JOURNALLING_ENABLED;

    private Byte mFieldSeparator = FIELD_SEPARATOR_CHAR;

    private String mEncodingCharacters = ENCODIND_CHARACTERS; // default

    private String mProcessingID = PROCESSING_ID; // default

    private String mVersionID = VERSION_ID;

    private Boolean mFieldRequired = null;
    
    private Boolean mEnabledPersistance = ENABLED_MLLPV1PERSISTANCE;//default

    // SFT segments attributes

    private Boolean mEnabledSFT = ENABLED_SFT;//default

    private String mSoftwareVendorOrganization = SOFTWARE_VENDOR_ORGANIZATION;// default

    private String mSoftwareCertifiedVersionOrReleaseNumber = SOFTWARE_CERTIFIED_VERSION; //default

    private String mSoftwareProductName = SOFTWARE_PRODUCT_NAME; //default

    private String mSoftwareBinaryID = SOFTWARE_BINARY_ID; //default

    private String mSoftwareProductInformation = SOFTWARE_PRODUCT_INFORMATION; //default

    private String mSoftwareInstallDate = SOFTWARE_INSTALLED_DATE; //default

    // UAC segments attributes

    private Boolean mEnabledUAC = ENABLED_UAC;//default

    private String mUserAuthenticationCredentialTypeCode = USER_AUTENTICATION_CREDENTIAL_TYPE_CODE;// default

    private String mUserAuthenticationCredential = USER_AUTHENTICATION_CREDENTIAL; //default

    private String mSendingApplication = SENDING_APPLICATION;

    private String mSendingFacility = SENDING_FACILITY;

    private String mAcceptAckXsd = null;

    private String mApplicationAckXsd = null;
    
    private Integer mllpv2NakRetries = 0;
    private Long mllpv2RetryInterval = 0L;
    private Long mllpv2TimeToWaitForAckNak = 0L;
    
    public static final String MLLPV2_RETRIES_COUNT_ON_NAK="mllpv2RetriesOnNak";
    public static final String MLLPV2_RETRY_INTERVAL="mllpv2RetryInterval";
    public static final String MLLPV2_TIME_TO_WAIT_FOR_ACK_NAK="mllpv2TimeToWaitForAckNak";
    
    /**
     * Set the extensibility element type
     * 
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        mFieldElementType = elementType;
    }

    /**
     * Get the extensibility element type
     * 
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        mFieldRequired = required;
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }

    /**
     * Set the Acknowledgement Mode
     */
    public void setAckMode(String val) {
        mAckMode = val;
    }

    /**
     * Get the Acknowledgement Mode
     */
    public String getAckMode() {
        return mAckMode;
    }

    /**
     * Set the Lower Level Protocol Type
     */
    public void setLLPType(String val) {
        mLLPType = val;
    }

    /**
     * Get the Lower Level Protocol Type
     */
    public String getLLPType() {
        return mLLPType;
    }

    /**
     * Set the End Data Character
     */
    public void setEndDataChar(Byte val) {
        mEndDataChar = val;
    }

    /**
     * Get the End Data Character
     */
    public Byte getEndDataChar() {
        return mEndDataChar;
    }

    /**
     * Set the End Block Character
     */
    public void setEndBlockChar(Byte val) {
        mEndBlkChar = val;
    }

    /**
     * Get the End Block Character
     */
    public Byte getEndBlockChar() {
        return mEndBlkChar;
    }

    /**
     * Set the Start Block Character
     */
    public void setStartBlockChar(Byte val) {
        mStartBlkChar = val;
    }

    /**
     * Get the Start Block Character
     */
    public Byte getStartBlockChar() {
        return mStartBlkChar;
    }

    /**
     * Set HLLP Check Sum Enabled
     */
    public void setHLLPChkSumEnabled(Boolean val) {
        mHLLPCheckSumEnabled = val;
    }

    /**
     * Get HLLP Check Sum Enabled
     */
    public Boolean getHLLPChkSumEnabled() {
        return mHLLPCheckSumEnabled;
    }

    /**
     * Set sequence number protocol enabled
     */
    public void setSeqNumEnabled(Boolean val) {
        mSeqNumProtocolEnabled = val;
    }

    // Get File Name
    public Boolean getSeqNumEnabled() {
        return mSeqNumProtocolEnabled;
    }

    /**
     * Set Journalling enabled
     */
    public void setJournallingEnabled(Boolean val) {
        mJournallingEnabled = val;
    }

    // Get Journalling
    public Boolean getJournallingEnabled() {
        return mJournallingEnabled;
    }

    /**
     * Set validate MSH segment enabled
     */
    public void setValidateMSHEnabled(Boolean val) {
        mValidateMSH = val;
    }

    // Get true or false for MSH segment vaidation
    public Boolean getValidateMSHEnabled() {
        return mValidateMSH;
    }

    /**
     * Setter for the Field Separator Character
     * 
     */
    public void setFieldSeparator(Byte val) {
        mFieldSeparator = val;
    }

    /**
     * Get the Field Separator Character
     */
    public Byte getFieldSeparator() {
        return mFieldSeparator;
    }

    /**
     * Set the Encoding Chaacters
     */
    public void setEncodingCharacters(String val) {
        mEncodingCharacters = val;
    }

    /**
     * Get the Encoding Chaacters
     */
    public String getEncodingCharacters() {
        return mEncodingCharacters;
    }

    /**
     * Set the Processing ID
     */
    public void setProcessingID(String val) {
        mProcessingID = val;
    }

    /**
     * Get the Processing ID
     */
    public String getProcessingID() {
        return mProcessingID;
    }

    /**
     * Set the Version ID
     */
    public void setVersionID(String val) {
        mVersionID = val;
    }

    /**
     * Get the Version ID
     */
    public String getVersionID() {
        return mVersionID;
    }

    /**
     * Set the Validate SFT segment enabled
     */
    public void setSFTEnabled(Boolean val) {
        mEnabledSFT = val;
    }

    // Get true or false for SFT segment vaidation
    public Boolean getSFTEnabled() {
        return mEnabledSFT;
    }

    /**
     * Set the Software Vendor Organization
     */
    public void setSoftwareVendorOrganization(String val) {
        mSoftwareVendorOrganization = val;
    }

    /**
     * Get the Software Vendor Organization
     */
    public String getSoftwareVendorOrganization() {
        return mSoftwareVendorOrganization;
    }

    /**
     * Set the Software Certified Version Or ReleaseNumber
     */
    public void setSoftwareCertifiedVersionOrReleaseNumber(String val) {
        mSoftwareCertifiedVersionOrReleaseNumber = val;
    }

    /**
     * Get the Software Certified Version Or ReleaseNumber
     */
    public String getSoftwareCertifiedVersionOrReleaseNumber() {
        return mSoftwareCertifiedVersionOrReleaseNumber;
    }

    /**
     * Set the Software Product Name
     */
    public void setSoftwareProductName(String val) {
        mSoftwareProductName = val;
    }

    /**
     * Get the Software ProductName
     */
    public String getSoftwareProductName() {
        return mSoftwareProductName;
    }

    /**
     * Set the Software Binary ID
     */
    public void setSoftwareBinaryID(String val) {
        mSoftwareBinaryID = val;
    }

    /**
     * Get the Software Binary ID
     */
    public String getSoftwareBinaryID() {
        return mSoftwareBinaryID;
    }

    /**
     * Set the Software Product Information
     */
    public void setSoftwareProductInformation(String val) {
        mSoftwareProductInformation = val;
    }

    /**
     * Get the Software Product Information
     */
    public String getSoftwareProductInformation() {
        return mSoftwareProductInformation;
    }

    /**
     * Set the Software Install Date
     */
    public void setSoftwareInstallDate(String val) {
        mSoftwareInstallDate = val;
    }

    /**
     * Get the Software Install Date
     */
    public String getSoftwareInstallDate() {
        return mSoftwareInstallDate;
    }

    // Get true or false for UAC segment vaidation
    public Boolean getUACEnabled() {
        return mEnabledUAC;
    }

    /**
     * Get the User Authentication Credential Type Code
     */
    public String getUserAuthenticationCredentialTypeCode() {
        return mUserAuthenticationCredentialTypeCode;
    }

    /**
     * Get the User Authentication Credential
     */
    public String getUserAuthenticationCredential() {
        return mUserAuthenticationCredential;
    }
    
    /**
     * Setter for Sending Application attribute
     * @param val sendingApplication
     */
    public void setSendingApplication(String val) {
        mSendingApplication = val;
    }
    
    /**
     * Getter for Sending Application attribute
     * @return String
     */
    public String getSendingApplication() {
        return mSendingApplication;
    }
    
    /**
     * Setter for Sending Facility attribute
     * @param val sendingFacility
     */
    public void SetSendingFacility(String val) {
       mSendingFacility = val; 
    }

    /**
     * Getter for Sending Facility attribute
     * @return String
     */
    public String getSendingFacility() {
        return mSendingFacility;
    }
    /**
     * Set the XSD which is used for creating accept acknowledgement 
     * @param val - xsd name
     */
    public void setAcceptAckXsd(String val) {
        mAcceptAckXsd = val;
    }

    /**
     * Get the XSD which is used for creating accept acknowledgement
     * @return xsd name
     */
    public String getAcceptAckXsd() {
        return mAcceptAckXsd;
    }

    /**
     * Set the XSD which is used for creating application acknowledgement
     * @param val
     */
    public void setApplicationAckXsd(String val) {
        mApplicationAckXsd = val;
    }

    /**
     * Get the XSD which is used for creating application acknowledgement
     * @return
     */
    public String getApplicationAckXsd() {
        return mApplicationAckXsd;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nHL7 ProtocolProperties (" + mFieldElementType + "):");
        strBuf.append("\nackMode=" + mAckMode);
        strBuf.append("\nllpType=" + mLLPType);
        strBuf.append("\nendDataChar=" + mEndDataChar.toString());
        strBuf.append("\nendBlockChar=" + mEndBlkChar.toString());
        strBuf.append("\nstartBlockChar=" + mStartBlkChar.toString());
        strBuf.append("\nhllpChkSumEnabled=" + mHLLPCheckSumEnabled.toString());
        strBuf.append("\nmllpv2RetriesCountOnNak=" + mllpv2NakRetries);   
        strBuf.append("\nmllpv2RetryInterval=" + mllpv2RetryInterval);
        strBuf.append("\nmllpv2TimeToWaitForAckNak="+ mllpv2TimeToWaitForAckNak);
        strBuf.append("\nseqNumEnabled=" + mSeqNumProtocolEnabled);
        strBuf.append("\nValidateMSH=" + mValidateMSH);
        strBuf.append("\nFieldSeparator=" + mFieldSeparator);
        strBuf.append("\nEncodingCharacters=" + mEncodingCharacters);
        strBuf.append("\nProcessingID=" + mProcessingID);
        strBuf.append("\nVersionID=" + mVersionID);
        strBuf.append("\nEnabledSFT=" + mEnabledSFT);
        strBuf.append("\nSoftwareVendorOrganization=" + mSoftwareVendorOrganization);
        strBuf.append("\nSoftwareCertifiedVersionOrReleaseNumber=" + mSoftwareCertifiedVersionOrReleaseNumber);
        strBuf.append("\nSoftwareProductName=" + mSoftwareProductName);
        strBuf.append("\nSoftwareBinaryID=" + mSoftwareBinaryID);
        strBuf.append("\nSoftwareProductInformation=" + mSoftwareProductInformation);
        strBuf.append("\nSoftwareInstallDate=" + mSoftwareInstallDate);
        strBuf.append("\nSendingApplication=" + mSendingApplication);
        strBuf.append("\nSendingFacility=" + mSendingFacility);
        return strBuf.toString();
    }

	public Integer getMLLPV2RetriesCountOnNak() {
		return mllpv2NakRetries;
	}

	public void setMLLPV2RetriesCountOnNak(Integer mllpv2NakRetries) {
		this.mllpv2NakRetries = mllpv2NakRetries;
	}

	public Long getMllpv2RetryInterval() {
		return mllpv2RetryInterval;
	}

	public void setMllpv2RetryInterval(Long mllpv2RetryInterval) {
		this.mllpv2RetryInterval = mllpv2RetryInterval;
	}

	public Long getMllpv2TimeToWaitForAckNak() {
		return mllpv2TimeToWaitForAckNak;
	}

	public void setMllpv2TimeToWaitForAckNak(Long mllpv2TimeToWaitForAckNak) {
		this.mllpv2TimeToWaitForAckNak = mllpv2TimeToWaitForAckNak;
	}

    /**
     * Set the MLLPv1 persistance enabled
     */
    public void setMLLPv1PersistanceEnabled(Boolean val) {
        mEnabledPersistance = val;
    }

    // Get true or false for MLLPv1 persistance enabled
    public Boolean getMLLPv1PersistanceEnabled() {
        return mEnabledPersistance;
    }

}
