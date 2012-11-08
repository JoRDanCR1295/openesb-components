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
 * @(#)SMTPProxy.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author aegloff
 */
public class SMTPProxy implements ExtensibilityElement, Serializable {
    
    // Element name
    public static final String ELEM_PROXY = "proxy";

    // Attribute names
    public static final String ATTR_HOST = "host";
    public static final String ATTR_PORT = "port";
    public static final String ATTR_USERNAME = "userName";
    public static final String ATTR_PASSWORD = "password";
    public static final String ATTR_SESSION_AUTHENTICATION =
        "sessionAuthentication";

    // Qualified element name
    public static final QName QNAME_PROXY =
        new QName(SMTPConstants.NS_URI_SMTP, SMTPProxy.ELEM_PROXY);

    private static final long serialVersionUID = 1L;
    
    private Boolean mFieldRequired = null;

    private String mHost = null;
    private Integer mPort = null;
    private String mUserName = null;
    private String mPassword = null;
    private Boolean mSessionAuthentication = null;
    
    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPProxy.QNAME_PROXY;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(final QName elementType) {
    }

    /** 
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }
    
    /** 
     * Set whether required (for wsdl:required) 
     */
    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    public String getHost() {
        return mHost;
    }

    public void setHost(final String host) {
        mHost = host;
    }

    public Integer getPort() {
        return mPort;
    }

    public void setPort(final Integer port) {
        mPort = port;
    }

    public String getUserName() {
        return mUserName;
    }

    public void setUserName(final String userName) {
        mUserName = userName;
    }

    public String getPassword() {
        return mPassword;
    }

    public void setPassword(final String password) {
        mPassword = password;
    }

    public Boolean getSessionAuthentication() {
        return mSessionAuthentication;
    }

    public void setSessionAuthentication(final Boolean sessionAuthentication) {
        mSessionAuthentication = sessionAuthentication;
    }
    @Override
	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTP Element Type " + SMTPProxy.QNAME_PROXY + ":");
        strBuf.append("\nRequired=" + mFieldRequired);
        return strBuf.toString();
    }
}
