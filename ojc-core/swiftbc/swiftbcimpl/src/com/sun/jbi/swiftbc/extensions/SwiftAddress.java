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
 * @(#)SwiftAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import com.sun.jbi.swiftbc.SAGConstants;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * @author S. Nageswara Rao
 */
public class SwiftAddress implements ExtensibilityElement, Serializable {

    // Attribute name - swift server location
    public static final String ATTR_SWIFT_SVR_LOCATIONURL = "location";

    // Attribute name - swift server location
    public static final String ATTR_SWIFT_SVR_LOCATION = "swiftserverLocation";

    // Attribute name - swift server port
    public static final String ATTR_SWIFT_SVR_PORT = "swiftserverPort";

    // Attribute name - swift transport protocol type
    public static final String ATTR_SWIFT_TRANS_PROTOCOL_NAME = "transportProtocolName";
    
    //  Default value for - swift transport protocol type
    public static final String ATTR_SWIFT_TRANS_PROTOCOL_NAME_VALUE = "tcp-ip";// default

    private static final long serialVersionUID = 1L;

    private Boolean mFieldRequired = null;

    private String mLocationURL = null;

    private String mServerLocation = null;

    private Integer mServerPort = null;

    private String mTransportProtocolName = ATTR_SWIFT_TRANS_PROTOCOL_NAME_VALUE;

    private QName mFieldElementType = SAGConstants.QNAME_ADDRESS;

    public SwiftAddress() {
    }

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
     * Set swift Server Location URL
     */
    public void setSwiftServerLocationURL(String val) {
        mLocationURL = val;
    }

    /**
     * Get swift Server Location
     */
    public String getSwiftServerLocationURL() {
        return mLocationURL;
    }

    /**
     * Set swift Server Location
     */
    public void setSwiftServerLocation(String val) {
        mServerLocation = val;
    }

    /**
     * Get swift Server Location
     */
    public String getSwiftServerLocation() {
        return mServerLocation;
    }

    /**
     * Set swift Server Port
     */
    public void setSwiftServerPort(Integer val) {
        mServerPort = val;
    }

    /*
     * Get swift Server Port
     */
    public Integer getSwiftServerPort() {
        return mServerPort;
    }

    /**
     * Set Transport Protocol Type
     */
    public void setTransportProtocolName(String val) {
        mTransportProtocolName = val;
    }

    /**
     * Get swift Server Port
     */
    public String getTransportProtocolName() {
        return mTransportProtocolName;
    }

 
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSwift Address (" + mFieldElementType + "):");
        strBuf.append("\nRequired=" + mFieldRequired);
        strBuf.append("\nSwiftServerLocationURL=" + mLocationURL);
        strBuf.append("\nSwiftServerLocation=" + mServerLocation);
        strBuf.append("\nSwiftServerPort=" + mServerPort.toString());
        strBuf.append("\nTransportProtocolName=" + mTransportProtocolName);

        return strBuf.toString();
    }
}
