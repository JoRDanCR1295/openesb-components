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
 */

/*
 * @(#)MQBCAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

public final class MQBCAddress
        implements ExtensibilityElement, Serializable, Deflatable {

    // Local element name
    public static final String ELEM_ADDRESS = "address";
    public static final String ATTR_HOST_NAME = "hostName";
    public static final String ATTR_PORT_NUMBER = "portNumber";
    public static final String ATTR_QUEUE_MANAGER_NAME = "queueManagerName";
    public static final String ATTR_CHANNEL_NAME = "channelName";
    public static final String ATTR_CODED_CHARACTER_SET_ID = "codedCharacterSetID";
    public static final String ATTR_USER_ID = "userName";
    public static final String ATTR_PASSWORD = "password";
    public static final String ATTR_CIPHERSUITE = "cipherSuite";
    public static final String ATTR_SSLPEERNAME = "sslPeerName";
    
    
    
    private static final long serialVersionUID = 2L;
    
    
    private volatile boolean fieldRequired = Boolean.FALSE;
    private volatile String hostName;
    private volatile int portNumber = 1414;
    private volatile String queueManagerName;
    private volatile String channelName;
    private volatile String codedCharacterSetID;
    private volatile String userID;
    private volatile String password;
    private volatile String sslPeerName;
    private volatile String cipherSuite;
    
    /** Creates a new instance of MQAddress */
    public MQBCAddress() {
    }
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return MQConstants.QNAME_ADDRESS;
    }
    
    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        // No operation
    }
    
    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = clean(required);
    }
    
    public String getHostName() {
        return clean(hostName);
    }
    
    public void setHostName(String val) {
        hostName= clean(val);
    }
    
    public int getPortNumber() {
        return clean(portNumber);
    }
    
    public void setPortNumber(int val) {
        portNumber= clean(val);
    }
    
    public String getQueueManagerName() {
        return clean(queueManagerName);
    }
    
    public void setQueueManagerName(String val) {
        queueManagerName= clean(val);
    }
    
    public String getChannelName() {
        return clean(channelName);
    }
    
    public void setChannelName(String val) {
        channelName= clean(val);
    }
    
    public String getCodedCharacterSetID() {
        return clean(codedCharacterSetID);
    }
    
    public void setCodedCharacterSetID(String val) {
        codedCharacterSetID = clean(val);
    }
    
    public String getPassword() {
        return clean(password);
    }
    
    public void setPassword(String val) {
        password = clean(val);
    }
    
    public String getUserName() {
        return clean(userID);
    }
    
    public void setUserName(String val) {
        userID = clean(val);
    }

    public String getCipherSuite() {
        return clean(cipherSuite);
    }
    
    public void setCipherSuite(String suite) {
        cipherSuite = clean(suite);
    }
    
    public String getSslPeerName() {
        return clean(sslPeerName);
    }
    
    public void setSslPeerName(String name) {
        sslPeerName = clean(name);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        return obj instanceof MQBCAddress && isSameMQBCAddress(
                (MQBCAddress) obj);
    }

    @Override
    public int hashCode() {
        return toString().toLowerCase().hashCode();
    }

    /**
     * Serialize to the PrintWriter.
     *
     * @param pw The serialization outlet.
     * @param context User-defined context.
     *
     * @throws com.sun.jbi.mqbc.extensions.Deflatable.DeflateException if any
     * problems occur during the serialization process.
     */
    public void deflate(PrintWriter pw, Context context)
            throws DeflateException {
        Object namespace = context.getContext("namespace");
        Object prefix = context.getContext("prefix");
        
        final boolean printNamespace = (namespace != null);
        
        pw.print("<");
        pw.print(prefix.toString());
        pw.print(":");
        pw.print(ELEM_ADDRESS);
        pw.print(" ");
        
        if (printNamespace) {
            pw.print("xml:");
            pw.print(prefix.toString());
            pw.print("=");
            pw.print("\"");
            pw.print(namespace.toString());
            pw.print("\" ");
        }

        if (fieldRequired) {
            Object defContext = context.getContext(Definition.class);
            if (defContext != null) {
                Definition def = (Definition) defContext;
                try {
                    DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        String.valueOf(fieldRequired),
                        def,
                        pw);
                } catch (WSDLException e) {
                    throw new Deflatable.DeflateException(e);
                }
            }
        }

        pw.print(ATTR_QUEUE_MANAGER_NAME + "=" + "\"" + queueManagerName + "\"");
        
        if (!hostName.equals("")) {
            pw.print(ATTR_HOST_NAME + "=" + "\"" + hostName + "\"");
            pw.print(ATTR_PORT_NUMBER + "=" + "\"" + portNumber + "\"");
        }
        if (!channelName.equals("")) {
            pw.print(ATTR_CHANNEL_NAME + "=" + "\"" + channelName + "\"");
        }
        if (!codedCharacterSetID.equals("")) {
            pw.print(ATTR_CODED_CHARACTER_SET_ID + "=" + "\"" + codedCharacterSetID + "\"");
        }
        if (!userID.equals("")) {
            pw.print(ATTR_USER_ID + "=" + "\"" + userID + "\"");
        }
        if (!password.equals("")) {
            pw.print(ATTR_PASSWORD + "=" + "\"" + password + "\"");
        }
        if (!cipherSuite.equals("")) {
            pw.print(ATTR_CIPHERSUITE + "=" + "\"" + cipherSuite + "\"");
        }
        if (!sslPeerName.equals("")) {
            pw.print(ATTR_SSLPEERNAME + "=" + "\"" + sslPeerName + "\"");
        }
        
        pw.println("/>");
    }

    private boolean isSameMQBCAddress(MQBCAddress thatmqAddress) {

        boolean isSame = getQueueManagerName().equalsIgnoreCase(
                thatmqAddress.getQueueManagerName());

        isSame &= getHostName().equalsIgnoreCase(thatmqAddress.getHostName());

        isSame &= getChannelName().equalsIgnoreCase(
                thatmqAddress.getChannelName());

        isSame &= getPortNumber() == thatmqAddress.getPortNumber();
        
        return isSame;
    }
    
    private String clean(String value) {
        if (value == null) {
            value = "";
        }
        return value.trim();
    }
    
    private Boolean clean(Boolean value) {
        return (value == null ? false : value);
    }
    
    private int clean(int value) {
        return Math.max(0, value);
    }
}
