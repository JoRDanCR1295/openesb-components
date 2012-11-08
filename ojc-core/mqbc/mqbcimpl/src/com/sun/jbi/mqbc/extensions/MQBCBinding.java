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
 * @(#)MQBCBinding.java 
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
 * Represents the Component's binding element extension.
 *
 * @author Noel.Ang@sun.com
 */
public class MQBCBinding
        implements ExtensibilityElement, Serializable, Deflatable {

    private static final long serialVersionUID = 2L;
    
    private volatile QName fieldElementType = MQConstants.QNAME_BINDING;
    private volatile boolean fieldRequired;

    /** Creates a new instance of MQBinding */
    public MQBCBinding() {
    }

    /**
     * Get the extensibility element type
     *
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     *
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
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
        fieldRequired = required;
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
    public void deflate(PrintWriter pw, Deflatable.Context context)
            throws Deflatable.DeflateException {

        Object namespace = context.getContext("namespace");
        Object prefix = context.getContext("prefix");
        
        final boolean printNamespace = (namespace != null);
        
        pw.print("<");
        pw.print(prefix.toString());
        pw.print(":binding ");
        
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
        
        pw.println("/>");
    }
}
