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
 * @(#)SMTPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

/**
 * Defines the constants for use in parsing the Service Description WSDL
 *
 * @author Alexander Fung
 */
public class SMTPConstants {
    
    // Namespaces
    public static final String NS_URI_SMTP = "http://schemas.sun.com/jbi/wsdl-extensions/smtp/";
    
    //Use types
    public static final String SMTP_USE_TYPE_LITERAL = "literal";
    public static final String SMTP_USE_TYPE_ENCODED = "encoded";
    public static final String SMTP_USE_SSL ="ssl Based";
    public static final String SMTP_USE_NON_SSL="plain";

}
