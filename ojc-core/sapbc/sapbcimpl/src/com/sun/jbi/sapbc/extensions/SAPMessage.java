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
 * @(#)SAPMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLExtensibleElementImpl;
import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPMessage extends WSDLExtensibleElementImpl implements Serializable {

    public SAPMessage(final ExtensibilityElement el) throws WSDLException {
        fromXML(el);
    }
    
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    public QName getElementType() {
        return fieldElementType;
    }
    
    public void setRequired(Boolean required) {
        isFieldRequired = required;
    }
    
    public Boolean getRequired() {
        return isFieldRequired;
    }
    
     public String toString() {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("SAPMessage " + fieldElementType + ":");
        strBuf.append("\nRequired: " + getRequired());
        return strBuf.toString();
    }


    public void toXML(final PrintWriter pw) {
        pw.write("<sap:message");
        pw.write("/>");
    }
    
    public void fromXML(final ExtensibilityElement el) throws WSDLException {
    }
    
    public boolean isInstance(ExtensibilityElement e){
        return e.getElementType().equals(TYPE);        
    }
    
    private static final long serialVersionUID = 1L;
    
    private QName fieldElementType = TYPE;
    private Boolean isFieldRequired = Boolean.FALSE;
    
    public static final QName TYPE =
             new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, "message", "sap"); // NO I8N
}
