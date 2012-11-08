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
 * @(#)LDAPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;


/**
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LDAPConstants {
    //Namespaces
    public static final String NS_URI_LDAP = "http://schemas.sun.com/jbi/wsdl-extensions/ldap/";

    //Local element names
    public static final String ELEM_ADDRESS = "address";

    //Qualified element names
    public static final QName QNAME_BINDING = new QName(LDAPConstants.NS_URI_LDAP,
            Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(LDAPConstants.NS_URI_LDAP,
            Constants.ELEM_OPERATION);
    public static final QName QNAME_OPERATION_INPUT = new QName(LDAPConstants.NS_URI_LDAP,
            Constants.ELEM_INPUT);
    public static final QName QNAME_OPERATION_OUTPUT = new QName(LDAPConstants.NS_URI_LDAP,
            Constants.ELEM_OUTPUT);
    public static final QName QNAME_ADDRESS = new QName(LDAPConstants.NS_URI_LDAP,
            LDAPConstants.ELEM_ADDRESS);

	public static final String APP_VAR_TYPE_NUMBER = "NUMBER";
    public static final String APP_VAR_TYPE_BOOLEAN = "BOOLEAN";
    public static final String APP_VAR_TYPE_STRING = "STRING";
    public static final String APP_VAR_TYPE_PASSWORD = "PASSWORD";
}