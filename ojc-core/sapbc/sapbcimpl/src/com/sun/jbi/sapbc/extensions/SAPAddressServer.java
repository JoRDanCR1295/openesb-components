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
 * @(#)SAPAddressServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLExtensibleElementImpl;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPAddressServer extends WSDLExtensibleElementImpl implements Serializable {

    public SAPAddressServer(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
        fromXML(el, sapEnvVar);
    }
    
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    public QName getElementType() {
        return fieldElementType;
    }

    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nsap serverparams " + fieldElementType + ":");
        strBuf.append("\nRequired: " + fieldRequired);
        strBuf.append("\n" + SAPAddressServer.ATTR_USE_PROGRAM_ID + ": " + programID);
        return strBuf.toString();
    }

    public String programID() {
        return programID;
    }
        
    public void toXML(final PrintWriter pw) {
        
        pw.write("            <sap:serverparams");
        
        DOMUtils.printAttribute(
                SAPAddressServer.ATTR_USE_PROGRAM_ID,
                programID(),
                pw);
        
        pw.write("/>\n");
    }
    
    public void fromXML(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) 
            throws WSDLException {
        String val;
        Map attrMap = el.getOtherAttributes();
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPAddressServer.ATTR_USE_PROGRAM_ID);
        if (val != null) {
            programID = val;
        } else {
            String msg = mMessages.getString(
                    "SAPAddressServer.Missing_required_attr",
                    new Object[] { SAPAddressServer.ATTR_USE_PROGRAM_ID, val });
            throw new WSDLException("", msg);
        }
        
    }
    
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SAPAddressServer.class);
    private static final Logger mLogger = Messages.getLogger(SAPAddressServer.class); 
    protected static final String ATTR_USE_PROGRAM_ID = "programID"; // NO I8N
    
    private QName fieldElementType = TYPE;
    private Boolean fieldRequired = Boolean.FALSE;
    private String programID = "";
    
    public static final QName TYPE =
            new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, "serverparams", "sap"); // NO I8N    
}
