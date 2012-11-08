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
 * @(#)MQBCBody.java 
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

/**
 *
 */
public class MQBCBody
        implements ExtensibilityElement, Serializable, Deflatable {


    public static final String ATTR_MQ_MESSAGE_TYPE = "messageType";
    public static final String ATTR_SYNC_POINT = "syncpoint";
    public static final String ATTR_MQ_MESSAGE_BODY = "messageBody";
    public static final String ATTR_MQ_MESSAGE_USE = "use";
    public static final String ATTR_MQ_ENCODING_STYLE = "encodingStyle";
    
    public static final String MESSAGE_TYPE_TEXT_MESSAGE = "TextMessage";
    public static final String MESSAGE_TYPE_BYTE_MESSAGE = "ByteMessage";
    
    public static final String MESSAGE_USE_TYPE_LITERAL = "literal";
    public static final String MESSAGE_USE_TYPE_ENCODED = "encoded";
   
    private static final long serialVersionUID = 2L;
   
    private volatile boolean fieldRequired;
    
    private volatile String mqMessageType = MESSAGE_TYPE_TEXT_MESSAGE;
    private volatile boolean mqSyncPoint;
    private volatile String mqMessageBody = "";
    private volatile String mqMessageUse = MESSAGE_USE_TYPE_LITERAL;
    private volatile String mqEncodingStyle = "";
    
    /** Creates a new instance of MQInput */
    public MQBCBody() {
    }
    
    /**
     * Get the extensibility element type
     * @return The extensibility element's type
     */
    public QName getElementType() {
        return MQConstants.QNAME_MQMESSAGE;
    }

    /**
     * Sets the extensibility element type
     * @param elementType The extensibility element's type
     */
    public void setElementType(QName elementType) {
         //no op
    }

    /**
     * Get whether required (for wsdl:required)
     * @return The element's required attribute value
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Sets whether required (for wsdl:required)
     * @param required The element's required attribute value
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Gets the messageType attribute
     * @return The messsage part for messageType
     */
    public String getMQMessageType() {
        return mqMessageType;
    }

    /**
     * Sets the messageType attribute
     * @param val The messsage part for messageType
     */
    public void setMQMessageType(String val) {
        val = clean(val);
        if (val.equalsIgnoreCase(MESSAGE_TYPE_TEXT_MESSAGE)) {
            mqMessageType = MESSAGE_TYPE_TEXT_MESSAGE;
        } else if (val.equalsIgnoreCase(MESSAGE_TYPE_BYTE_MESSAGE)) {
            mqMessageType = MESSAGE_TYPE_BYTE_MESSAGE;
        }
    }
    
    /**
     * Gets the messageUse attribute
     * @return The use part
     */
    public String getMQMessageUse() {
        return mqMessageUse;
    }

    /**
     * Sets the messageUse attribute
     * @param val The messsage part for messageType
     */
    public void setMQMessageUse(String val) {
        val = clean(val);
        if (val.equalsIgnoreCase(MESSAGE_USE_TYPE_ENCODED)) {
            mqMessageUse = MESSAGE_USE_TYPE_ENCODED;
        } else if (val.equalsIgnoreCase(MESSAGE_USE_TYPE_LITERAL)) {
            mqMessageUse = MESSAGE_USE_TYPE_LITERAL;
        }
    }
   
    
     public String getMQMessageBody() {
        return mqMessageBody;
    }

    /**
     * Sets the messageBody attribute
     * @param val The messsage part for messageBody
     */
    public void setMQMessageBody(String val) {
        mqMessageBody = clean(val);
    }
    
    public boolean getMQSyncPoint() {
        return mqSyncPoint;
    }

    public void setMQSyncPoint(boolean val) {
        mqSyncPoint = val;
    }
    
    public void setMQMsgBodyEncodingStyle(String val) {
        mqEncodingStyle = clean(val);
    }    

    public String getMQMsgBodyEncodingStyle() {
        return mqEncodingStyle;
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
        pw.print(":body ");
        
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

        pw.print(ATTR_MQ_MESSAGE_BODY + "=" + "\"" + mqMessageBody + "\"");
        pw.print(ATTR_MQ_MESSAGE_TYPE + "=" + "\"" + mqMessageType + "\"");
        pw.print(ATTR_MQ_MESSAGE_USE + "=" + "\"" + mqMessageUse + "\"");
        pw.print(ATTR_SYNC_POINT + "=" + "\"" + String.valueOf(mqSyncPoint) + "\"");
        if (!mqEncodingStyle.equals("")) {
            pw.print(ATTR_MQ_ENCODING_STYLE + "=" + "\"" + mqEncodingStyle + "\"");
        }
        
        pw.println("/>");
    }
    
    private String clean(String value) {
        if (value == null) {
            value = "";
        }
        return value.trim();
    }
}
