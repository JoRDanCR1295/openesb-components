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
 * @(#)HL7Address.java 
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
 * @author S. Nageswara Rao
 */
public class HL7Address implements ExtensibilityElement, Serializable {

    public static final String TCP_ROLE_CLIENT = "client";
    
    public static final String TCP_ROLE_SERVER = "server";

    public static final String TCP_ROLE_DEFAULT = "";

	// Attribute name - HL7 server location
	public static final String ATTR_ROLE = "role";
    
	// Attribute name - HL7 server location
	public static final String ATTR_HL7_SVR_LOCATIONURL = "location";

	// Attribute name - HL7 server location
	public static final String ATTR_HL7_SVR_LOCATION = "hl7serverLocation";

	// Attribute name - HL7 server port
	public static final String ATTR_HL7_SVR_PORT = "hl7serverPort";

	// Attribute name - HL7 transport protocol type
	public static final String ATTR_HL7_TRANS_PROTOCOL_NAME = "transportProtocolName";

	//  Default value for - HL7 transport protocol type
	public static final String ATTR_HL7_TRANS_PROTOCOL_NAME_VALUE = "tcp-ip";// default

	private static final long serialVersionUID = 1L;

	private Boolean mFieldRequired = null;

	private String mLocationURL = null;

	private String mServerLocation = null;

	private Integer mServerPort = null;

	private TcpRoleEnum mTcpRole;
	
	private String mTransportProtocolName = ATTR_HL7_TRANS_PROTOCOL_NAME_VALUE;

	private QName mFieldElementType = HL7Constants.QNAME_ADDRESS;

	public HL7Address() {
		// nothing
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
	 * Set HL7 Server Location URL
	 */
	public void setHL7ServerLocationURL(String val) {
		mLocationURL = val;
	}

	/**
	 * Get HL7 Server Location
	 */
	public String getHL7ServerLocationURL() {
		return mLocationURL;
	}

	/**
	 * Set HL7 Server Location
	 */
	public void setHL7ServerLocation(String val) {
		mServerLocation = val;
	}

	/**
	 * Get HL7 Server Location
	 */
	public String getHL7ServerLocation() {
		return mServerLocation;
	}

	/**
	 * Set HL7 Server Port
	 */
	public void setHL7ServerPort(Integer val) {
		mServerPort = val;
	}

	/*
	 * Get HL7 Server Port
	 */
	public Integer getHL7ServerPort() {
		return mServerPort;
	}

	/**
	 * Set Transport Protocol Type
	 */
	public void setTransportProtocolName(String val) {
		mTransportProtocolName = val;
	}

	/**
	 * Get HL7 Server Port
	 */
	public String getTransportProtocolName() {
		return mTransportProtocolName;
	}

	public String toString() {
		StringBuffer strBuf = new StringBuffer(super.toString());
		strBuf.append("\nHL7 Address (" + mFieldElementType + "):");
		strBuf.append("\nRequired=" + mFieldRequired);
		strBuf.append("\nHL7ServerLocationURL=" + mLocationURL);
		strBuf.append("\nHL7ServerLocation=" + mServerLocation);
		strBuf.append("\nHL7ServerPort=" + mServerPort.toString());
		strBuf.append("\nRole=" + mTcpRole.name());
		strBuf.append("\nTransportProtocolName=" + mTransportProtocolName);

		return strBuf.toString();
	}
	
	public void setTcpRoleString(String theValue) {
		if (TCP_ROLE_CLIENT.equalsIgnoreCase(theValue)) {
			mTcpRole = TcpRoleEnum.CLIENT;
		} else if (TCP_ROLE_SERVER.equalsIgnoreCase(theValue)) {
			mTcpRole = TcpRoleEnum.SERVER;
		} else {
			mTcpRole = TcpRoleEnum.DEFAULT;
		}
	}
	
	public TcpRoleEnum getTcpRole() {
		return mTcpRole;
	}
	
	
}
