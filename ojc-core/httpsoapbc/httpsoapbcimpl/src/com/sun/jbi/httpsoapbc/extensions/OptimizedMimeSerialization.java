/*******************************************************************************
 * The contents of this file are subject to the terms of the Common Development and
 * Distribution License(theLicense).You may not use this file except in compliance
 * with the License.
 * 
 * You can obtain a copy of the License at http://opensource.org/licenses/CDDL-1.0or
 * http://opensource.org/licenses/cddl1.txt
 * 
 * When distributing Covered Code,include this CDDL Header Notice in each file and
 * include the License file at http://opensource.org/licenses/cddl1.txt. If applicable, add
 * the following below the CDDL Header, with the fields enclosed by brackets []
 * replaced by your own identifying information:
 * 
 * "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * Portions Copyrighted 2013 OpenESB Community
 ******************************************************************************/

package com.sun.jbi.httpsoapbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author David BRASSELY (brasseld at gmail.com)
 * @author OpenESB Community
 */
public class OptimizedMimeSerialization implements ExtensibilityElement, Serializable {

    public static String NS_URI_HTTPBC_MTOM_EXTENSION = "http://schemas.xmlsoap.org/ws/2004/09/policy/optimizedmimeserialization";
    
    // Local element name
    public static final String ELEM_OptimizedMimeSerialization = "OptimizedMimeSerialization";
    
    // QName representing this Extensibility Element
    private QName QNAME_OptimizedMimeSerialization =
        new QName(NS_URI_HTTPBC_MTOM_EXTENSION, ELEM_OptimizedMimeSerialization);
    
    private Boolean mFieldRequired = false;
    
    public void setElementType(QName elementType) {
        QNAME_OptimizedMimeSerialization = elementType;
    }

    public QName getElementType() {
        return QNAME_OptimizedMimeSerialization;
    }

    public void setRequired(Boolean arg0) {
        mFieldRequired = arg0;
    }

    public Boolean getRequired() {
        return mFieldRequired;
    }
}
