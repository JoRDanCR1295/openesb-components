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
 * @(#)Policy.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Administrator
 */
public class Policy implements ExtensibilityElement, Serializable {

    public static String NS_URI_POLICY = "http://schemas.xmlsoap.org/ws/2004/09/policy";
    public static String NS_URI_SECURITY_POLICY = "http://schemas.xmlsoap.org/ws/2005/07/securitypolicy";
    public static String NS_URI_BASIC_AUTHENTICATION_SECURITY_POLICY = "http://sun.com/ws/httpbc/security/BasicauthSecurityPolicy";
    // Local element name
    public static final String ELEM_ADDRESS = "Policy";

    // QName representing this Extensibility Element
    public static final QName QNAME_ADDRESS =
        new QName(NS_URI_POLICY, ELEM_ADDRESS);
    
    private Boolean mFieldRequired = false;
    private String policyID;
    private String username;
    private String password;
    
    private MustSupportBasicAuthentication msba;
    private BasicAuthenticationDetail bad;
        
    public Policy() {
    }

    public void setElementType(QName elementType) {
    }

    public QName getElementType() {
        return Policy.QNAME_ADDRESS;
    }

    public void setRequired(Boolean required) {
        mFieldRequired = required;
    }

    public Boolean getRequired() {
        return mFieldRequired;
    }

    void setID(String id) {
      this.policyID = id;  
    }
    
    public String getID() {
        return this.policyID;
    }
    
    public String getUserName() {
      return this.username;  
    }
    
    public void setUserName(String user) {
        this.username = user;
    }
    
    public void setPassword(String passwd) {
        this.password = passwd;
    }
    
    public String getPassword() {
        return this.password;
    }
    
    
    public MustSupportBasicAuthentication getMustSupportBasicAuthentication() {
        return msba;
    }
    
    public void setMustSupportBasicAuthentication (MustSupportBasicAuthentication val) {
        msba = val;
    }
    
    public BasicAuthenticationDetail getBasicAuthenticationDetail() {
        return bad;
    }
    
    public void setBasicAuthenticationDetail (BasicAuthenticationDetail val) {
        bad = val;
    }
        
}
