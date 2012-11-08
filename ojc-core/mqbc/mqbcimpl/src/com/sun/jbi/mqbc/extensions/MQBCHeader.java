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
 * @(#)MQBCHeader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 */
public final class MQBCHeader implements ExtensibilityElement, Serializable {

    public static final String ATTR_DESCRIPTOR = "descriptor";
    public static final String ATTR_PART = "part";
    
    private static final long serialVersionUID = 2L;
    private volatile QName fieldElementType = MQConstants.QNAME_MQHEADER;
    private volatile boolean fieldRequired;
    private final MessageDescriptors descriptor;
    private final String partName;

    /**
     * Creates a new instance of MQHeader
     *
     * @param descriptor The subject that this object represents
     * @param partName Name of the message part assigned to this header.
     *
     * @throws NullPointerException if descriptor or partName is null.
     * @throws IllegalArgumentException if partName is blank.
     */
    public MQBCHeader(MessageDescriptors descriptor, String partName) {
        if (descriptor == null) {
            throw new NullPointerException("descriptor");
        }
        if (partName == null) {
            throw new NullPointerException("partName");
        }
        if (partName.trim().equals("")) {
            throw new IllegalArgumentException("blank partName");
        }
        this.descriptor = descriptor;
        this.partName = partName;
    }

    public MessageDescriptors getDescriptor() {
        return descriptor;
    }
    
    public String getPartName() {
        return partName;
    }
    
    /**
     * Get the extensibility element type
     * @return The extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }
    
    /**
     * Sets the extensibility element type
     * @param elementType The extensibility element's type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
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
}
