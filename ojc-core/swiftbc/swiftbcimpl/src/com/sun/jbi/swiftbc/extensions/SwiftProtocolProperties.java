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

package com.sun.jbi.swiftbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;
import com.sun.jbi.swiftbc.SAGConstants;

/**
 * @author S. Nageswara Rao
 */
public class SwiftProtocolProperties implements ExtensibilityElement, Serializable {
    
    private static final long serialVersionUID = 1L;
    
    
    public static final String ATTR_VERSION_ID = "2.1";
    
    public static final String ATTR_SOFTWARE_VENDOR_ORGANIZATION = "Sun Microsystems, Inc.";
    
    public static final String ATTR_SOFTWARE_CERTIFIED_VERSION = "6.0";
    
    public static final String ATTR_SOFTWARE_PRODUCT_NAME = "Sun Swift BC";
    
    public static final String ATTR_SOFTWARE_BINARY_ID = "6.0";
    
    public static final String ATTR_SOFTWARE_PRODUCT_INFORMATION = "It is a HL7 Binding component";
    
    public static final String ATTR_SOFTWARE_INSTALLED_DATE = "";
    
    
    private QName mFieldElementType = SAGConstants.QNAME_PROTOCOLPROPERTIES;
    
    
    private String mVersionID = ATTR_VERSION_ID;
    
    private Boolean mFieldRequired = null;
    
    // SFT segments attributes
    
    private String mSoftwareVendorOrganization = ATTR_SOFTWARE_VENDOR_ORGANIZATION;// default
    
    private String mSoftwareCertifiedVersionOrReleaseNumber = ATTR_SOFTWARE_CERTIFIED_VERSION; //default
    
    private String mSoftwareProductName = ATTR_SOFTWARE_PRODUCT_NAME; //default
    
    private String mSoftwareBinaryID = ATTR_SOFTWARE_BINARY_ID; //default
    
    private String mSoftwareProductInformation = ATTR_SOFTWARE_PRODUCT_INFORMATION; //default
    
    private String mSoftwareInstallDate = ATTR_SOFTWARE_INSTALLED_DATE; //default
    
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
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSwift ProtocolProperties (" + mFieldElementType + "):");
        strBuf.append("\nSoftwareVendorOrganization=" + mSoftwareVendorOrganization);
        strBuf.append("\nSoftwareCertifiedVersionOrReleaseNumber=" + mSoftwareCertifiedVersionOrReleaseNumber);
        strBuf.append("\nSoftwareProductName=" + mSoftwareProductName);
        strBuf.append("\nSoftwareBinaryID=" + mSoftwareBinaryID);
        strBuf.append("\nSoftwareProductInformation=" + mSoftwareProductInformation);
        strBuf.append("\nSoftwareInstallDate=" + mSoftwareInstallDate);
        return strBuf.toString();
    }
}
