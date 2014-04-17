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
 * @(#)StringCompareValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/*
 * Class holding information pertaining to authenticating credentials against username and password
 * Strings.
 */
public class PropertiesFileValidation extends ValidationBaseType
        implements ExtensibilityElement, Serializable {
    
    // Local element name
    public static final String ELEM_PROPERTIES_Compare = "PropertesFileAuthentication";
     public static final String PROPERTIES_FILE_PATH="path";
     public static final String PATH_NOT_DEFINED="NO "+PROPERTIES_FILE_PATH +" DEFINED";

    // QName representing this Extensibility Element
    private QName QNAME_StringCompare =
        new QName(NS_URI_HTTPBC_SEC_EXTENSION, ELEM_PROPERTIES_Compare);
    
    private Boolean mFieldRequired = false;

    private String propertiesFileLocation="";
    
    public void setElementType(QName arg0) {
        QNAME_StringCompare = arg0;
    }

    public QName getElementType() {
        return QNAME_StringCompare;
    }

    public void setRequired(Boolean arg0) {
        mFieldRequired = arg0;
    }

    public Boolean getRequired() {
        return mFieldRequired;
    }


    public String getPropertiesFileLocation() {
        return propertiesFileLocation;
    }

    public void setPropertiesFileLocation(String propertiesFileLOcation) {
        this.propertiesFileLocation = propertiesFileLOcation;
    }
    
    
    
}
