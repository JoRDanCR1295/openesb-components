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
 * @(#)JDBCConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;


/**
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCConstants {
    //Namespaces
    public static final String NS_URI_JDBC = "http://schemas.sun.com/jbi/wsdl-extensions/jdbc/";

    //Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_SQL = "sql";

    //Qualified element names
    public static final QName QNAME_BINDING = new QName(JDBCConstants.NS_URI_JDBC,
            Constants.ELEM_BINDING);
    public static final QName QNAME_SQL = new QName(JDBCConstants.NS_URI_JDBC, JDBCConstants.ELEM_SQL);
    public static final QName QNAME_OPERATION = new QName(JDBCConstants.NS_URI_JDBC,
            Constants.ELEM_OPERATION);
    public static final QName QNAME_OPERATION_INPUT = new QName(JDBCConstants.NS_URI_JDBC,
            Constants.ELEM_INPUT);
    public static final QName QNAME_OPERATION_OUTPUT = new QName(JDBCConstants.NS_URI_JDBC,
            Constants.ELEM_OUTPUT);
    public static final QName QNAME_ADDRESS = new QName(JDBCConstants.NS_URI_JDBC,
            JDBCConstants.ELEM_ADDRESS);
}
