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
 * @(#)EmailAddr.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public abstract class EmailAddress implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;

    public static final String ATTR_EMAIL_HOSTNAME = "emailServer";
    public static final String ATTR_EMAIL_PORT = "port";
    public static final String ATTR_EMAIL_USERNAME = "userName";
    public static final String ATTR_EMAIL_PASSWORD = "password";
    public static final String ATTR_EMAIL_USESSL = "useSSL";

    private Boolean isRequired = Boolean.TRUE;

    private String hostName = null;
    private int port = 0;
    private String userName = null;
    private String password = null;
    private boolean useSSL = false;

    public EmailAddress() {
    }

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setElementType(javax.xml.namespace.QName)
     */
    public void setElementType(QName qName) {
        // No op
    }

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
     */
    public abstract QName getElementType();

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setRequired(java.lang.Boolean)
     */
    public void setRequired(Boolean isRequired) {
        this.isRequired = isRequired;
    }

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
     */
    public Boolean getRequired() {
        return this.isRequired;
    }

    public String getHostName() {
        return hostName;
    }

    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public boolean getUseSSL() {
        return useSSL;
    }

    public void setUseSSL(boolean useSSL) {
        this.useSSL = useSSL;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n EmailAddr " + getElementType() + ":");
        strBuf.append("\n Required = " + isRequired);
        strBuf.append("\n Hostname = " + hostName);
        strBuf.append("\n userName = " + userName);
        strBuf.append("\n password = " + ((password == null) ? "(empty)" : "(password is set)")) ;
        return strBuf.toString();
    }
}
