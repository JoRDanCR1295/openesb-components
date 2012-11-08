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
 * @(#)SMTPAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.Serializable;
import javax.crypto.NullCipher;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author aegloff
 */
public class SMTPAddress implements ExtensibilityElement, Serializable {
    
    // Local element name
    public static final String ELEM_ADDRESS = "address";

    // Attribute names
    public static final String ATTR_LOCATION = "location";
    
    // Attribute names
    public static final String ATTR_SMTPSERVER = "smtpserver";
    
    public static final String  ATTR_SMTPPORT = "smtpport";
    
    public static final String ATTR_USERNAME = "username";
    
    public static final String ATTR_PASSWORD = "password";
    
   // public static final String ATTR_USESSL = "useSSL";
    
    public static final String ATTR_USESSL = "useSSL";
    
    // QName representing this Extensibility Element
    public static final QName QNAME_ADDRESS =
        new QName(SMTPConstants.NS_URI_SMTP, SMTPAddress.ELEM_ADDRESS);

    private static final long serialVersionUID = 1L;

    private Boolean mFieldRequired = null;

    private MailTo mLocation = null;
    
    private String mSMTPServer = null;
    
    private int mSMTPPort = 25;
    
    private String mUsername = null;
    
    private String mPassword = null;
    
    private boolean mUseSSL = false; //default

    public SMTPAddress() {}

    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPAddress.QNAME_ADDRESS;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(final QName elementType) {
        // No operation
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

    public MailTo getLocation() {
        return mLocation;
    }

    public void setLocation(final MailTo val) {
        mLocation = val;
    }    
    
    public String getSMTPServer() {
        return mSMTPServer;
    }

    public void setSMTPServer(final String val) {
        mSMTPServer = val;
    }    
    
    public void setSMTPPort(final int val){
        mSMTPPort = val;
    }
    
    public int getSMTPPort() {
        return mSMTPPort;
    }
    public void setUserName(final String val){
        mUsername = val;
    }
    
    public void setPassWord(final String val){
        mPassword = val;
    }
    
    public String getUserName(){
        return mUsername;
    }
    
    public String getPassword(){
        return mPassword;
    }
    
    public void setUseSSL(boolean val){
        this.mUseSSL= val;
    }
    
    public boolean getUseSSL(){
        return mUseSSL;
    }
    
    
    @Override
	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTP address (" + SMTPAddress.QNAME_ADDRESS + "):");
        strBuf.append("\nRequired=" + mFieldRequired);
        strBuf.append("\nLocation=" + mLocation);
         strBuf.append("\nSmtpserver=" + mSMTPServer);
         strBuf.append("\nSmtpserver="+ mSMTPPort);
         strBuf.append("\nUserName=" + mUsername);
         strBuf.append("\nPassword=" + mPassword);
         strBuf.append("\nUseSSL=" + mUseSSL);
        return strBuf.toString();
    }
}
