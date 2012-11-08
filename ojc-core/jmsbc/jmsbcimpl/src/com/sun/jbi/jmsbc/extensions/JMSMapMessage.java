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
 * @(#)JMSMapMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import java.util.Iterator;
import java.util.Map;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * Represents the message parts mapping to a jms mapmessage
 */
public class JMSMapMessage implements ExtensibilityElement, Serializable {

    private static final long serialVersionUID = 1L;

    QName fieldElementType = JMSConstants.QNAME_MAPMESSAGE;

    Boolean fieldRequired = null;

    Map parts = null;  // <mapEntryName, JMSMapMessagePart>

    public JMSMapMessage() {
    }

    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
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
     * Gets the mapmessage parts map
     * @return Map
     */
    public Map getMapMessageParts() {
        return parts;
    }

    /**
     * Sets the mapmessage parts map
     * @param val Map
     */
    public void setMapMessageParts(Map val) {
        parts = val;
    }

    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nMapMessage(" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nMap Part List=[[" + partsMapToString() + "]]");
        strBuf.append("\n");
        return strBuf.toString();
    }
    
    private String partsMapToString() {
        StringBuffer strBuff = new StringBuffer();
        if (parts != null) {
            Iterator mapIter = parts.keySet().iterator();
            while (mapIter.hasNext()) {
                JMSMapMessagePart part = 
                        (JMSMapMessagePart)parts.get(mapIter.next());
                strBuff.append(part.toString());
            }
        }
        return strBuff.toString();
    }
    
}
