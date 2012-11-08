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
 * @(#)SAPWsdlConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 * @author Julie Knight
 */
public class SAPWsdlConstants {
    
    private SAPWsdlConstants() {
    }
    
    // Namespace for SAP extensibility elements
    public static final String EXT_NAMESPACE_URI = "http://schemas.sun.com/jbi/wsdl-extensions/sap/";
    
    // Namespace for SAP extensibility elements
    public static final String FM_OPERATION = "fmoperation";
    
    // Namespace for SAP extensibility elements
    public static final String IDOC_OPERATION = "idocoperation";

    // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_MESSAGE = "message";

    // Qualified element names
    public static final QName QNAME_BINDING = new QName(EXT_NAMESPACE_URI, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(EXT_NAMESPACE_URI, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(EXT_NAMESPACE_URI, ELEM_ADDRESS);
    public static final QName QNAME_MESSAGE = new QName(EXT_NAMESPACE_URI, ELEM_MESSAGE);
    
}
