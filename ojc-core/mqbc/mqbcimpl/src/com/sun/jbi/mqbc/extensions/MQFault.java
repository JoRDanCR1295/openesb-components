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
 * @(#)$Id: MQFault.java,v 1.1 2008/12/10 21:53:45 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
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
 * Represents a MQ binding Fault WSDL element.
 *
 * @author Noel.Ang@sun.com
 */
public class MQFault implements ExtensibilityElement, Serializable, Deflatable {
    public static final String ATTR_REASON_CODE = "reasonCodePart";
    public static final String ATTR_REASON_TEXT = "reasonTextPart";

    private static final long serialVersionUID = 1L;
    private volatile boolean fieldRequired;
    private volatile String reasonCodePart;
    private volatile String reasonTextPart;
   
    /** Creates a new instance of MQFault */
    public MQFault() {
    }

    public String getReasonCodePart() {
        return clean(reasonCodePart);
    }
    
    public void setReasonCodePart(String partName) {
        reasonCodePart = clean(partName);
    }
    
    public String getReasonTextPart() {
        return clean(reasonTextPart);
    }
    
    public void setReasonTextPart(String partName) {
        reasonTextPart = clean(partName);
    }
    
    /**
     * Get the extensibility element type
     * @return The extensibility element's type
     */
    public QName getElementType() {
        return MQConstants.QNAME_MQFAULT;
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
        fieldRequired = clean(required);
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
        pw.print(":fault ");
        
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

        pw.print(ATTR_REASON_CODE + "=" + "\"" + clean(reasonCodePart) + "\"");
        pw.print(ATTR_REASON_TEXT + "=" + "\"" + clean(reasonTextPart) + "\"");
        pw.println("/>");
    }
    
    private String clean(String value) {
        if (value == null) {
            value = "";
        }
        return value.trim();
    }
    
    private boolean clean(Boolean value) {
        return (value == null ? Boolean.FALSE : value);
    }
}
