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
 * @(#)SAPFmOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import java.io.PrintWriter;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPFmOperation extends SAPOperation {
    
    public SAPFmOperation(final ExtensibilityElement el) throws WSDLException {
        setElementType(TYPE);
        setRequired(Boolean.FALSE);
        fromXML(el);
    }
    
    public String functionName() {
        return functionName;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("SAPFmOperation " + getElementType() + ":");
        strBuf.append("\nRequired: " + getRequired());
        strBuf.append("\n" + ATTR_FUNCTION_NAME + ": " + functionName);
        return strBuf.toString();
    }

    public void toXML(final PrintWriter pw) {
        pw.print("        <sap:fmoperation");
        
        DOMUtils.printAttribute(
                SAPFmOperation.ATTR_FUNCTION_NAME,
                functionName(),
                pw);
        
        pw.print("/>\n");
    }

    public void fromXML(final ExtensibilityElement el) throws WSDLException {
        String val;
        Map attrMap = el.getOtherAttributes();
        
        val = (String) attrMap.get(SAPFmOperation.ATTR_FUNCTION_NAME);
        if (val != null) {
            functionName = val;
        } else {
            String msg = mMessages.getString(
                    "SAPOperation.Missing_required_attr",
                    SAPFmOperation.ATTR_FUNCTION_NAME);
            throw new WSDLException("", msg);
        }
    }

    public static final String ATTR_FUNCTION_NAME = "functionName";
    private String functionName = "";

    private static final Messages mMessages = Messages.getMessages(SAPFmOperation.class);
    private static final Logger mLogger = Messages.getLogger(SAPFmOperation.class); 
    
    public static final QName TYPE =
             new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, SAPWsdlConstants.FM_OPERATION, "sap");
}
