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
 * @(#)SAPIDocOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import java.io.PrintWriter;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPIDocOperation extends SAPOperation {
    
    public SAPIDocOperation(final ExtensibilityElement el) throws WSDLException {
       setElementType(TYPE);
       setRequired(Boolean.FALSE);
       fromXML(el);
    }
    
     public String toString() {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("SAPIDocOperation " + getElementType() + ":");
        strBuf.append("\nRequired: " + getRequired());
        return strBuf.toString();
    }

    public void toXML(final PrintWriter pw) {
        pw.print("        <sap:idocoperation");        
        pw.print("/>\n");
    }

    public void fromXML(final ExtensibilityElement el) throws WSDLException {
    }

    //private static final String ATTR_FUNCTION_NAME = "functionName";
    private static final Messages mMessages = Messages.getMessages(SAPIDocOperation.class);
    private static final Logger mLogger = Messages.getLogger(SAPIDocOperation.class); 
    
    public static final QName TYPE =
             new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, SAPWsdlConstants.IDOC_OPERATION, "sap");
}
