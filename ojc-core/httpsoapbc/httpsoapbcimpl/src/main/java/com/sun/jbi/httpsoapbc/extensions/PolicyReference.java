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
 * @(#)PolicyReference.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;


public class PolicyReference implements ExtensibilityElement, Serializable {

        public static String NS_URI_POLICY = "http://schemas.xmlsoap.org/ws/2004/09/policy";
    
    // Local element name
    public static final String ELEM_ADDRESS = "PolicyReference";

    // QName representing this Extensibility Element
    public static final QName QNAME_ADDRESS =
        new QName(NS_URI_POLICY, ELEM_ADDRESS);
    
    private Boolean mFieldRequired = false;
    private String mURI;

    public PolicyReference() {
    }

    public void setElementType(QName elementType) {
    }

    public QName getElementType() {
        return PolicyReference.QNAME_ADDRESS;
    }

    public void setRequired(Boolean required) {
        this.mFieldRequired = required;
    }

    public Boolean getRequired() {
        return this.mFieldRequired;
    }

    public String getURI() {
        return mURI;
    }
    
    public void setURI(String uri) {
        mURI = uri;
    }
}
