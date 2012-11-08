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
 * @(#)BPELExtConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import javax.xml.namespace.QName;

/**
 * Constants used in BPEL WSDL extension support.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public abstract class BPELExtConstants {
    
    public static String PROPERTY_URL =
        "http://docs.oasis-open.org/wsbpel/2.0/varprop";
    
    public static String PARTNER_LINK_URL =
        "http://docs.oasis-open.org/wsbpel/2.0/plnktype";

    public static String PROPERTY_NCNAME = "property";
    
    public static QName PROPERTY_ELEM =
        new QName(PROPERTY_URL, PROPERTY_NCNAME);
    
    public static String PROPERTY_ALIAS_NCNAME = "propertyAlias";
    
    public static QName PROPERTY_ALIAS_ELEM =
        new QName(PROPERTY_URL, PROPERTY_ALIAS_NCNAME);
    
    public static String QUERY_NCNAME = "query";
    
    public static QName QUERY_ELEM =
        new QName(PROPERTY_URL, QUERY_NCNAME);
    
    public static String PARTNER_LINK_TYPE_NCNAME = "partnerLinkType";
    
    public static QName PARTNER_LINK_TYPE_ELEM =
        new QName(PARTNER_LINK_URL, PARTNER_LINK_TYPE_NCNAME);
    
    public static String ROLE_NCNAME = "role";
    
    public static QName PARTNER_LINK_ROLE_ELEM =
        new QName(PARTNER_LINK_URL, ROLE_NCNAME);
}
