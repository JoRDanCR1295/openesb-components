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
 * @(#)LDAPOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LDAPOperation implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    private QName mFieldElementType = LDAPConstants.QNAME_OPERATION;
    private Boolean mFieldRequired = null;
    public static final String OPERATION_TYPE_SEARCH = "searchRequest";
    public static final String OPERATION_TYPE_UPDATE = "updateRequest";
    public static final String OPERATION_TYPE_ADD = "insertRequest";
    public static final String OPERATION_TYPE_DELETE = "deleteRequest";
    public static final String ATTR_OPERATION_TYPE = "type";
    private String mOperationType = null;

    public LDAPOperation() {
    }

    /**
     * @return mFieldElementType
     */
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
     * @return mFieldRequired
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }

    public void setElementType(final QName elementType) {
        mFieldElementType = elementType;
    }

    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }
    
     public void setOperationType(final String operationType) {
        mOperationType = operationType;
    }
    
    /**
     *
     * @return mOperationType
     */
    public String getOperationType() {
        return mOperationType;
    }
}
