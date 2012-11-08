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
 * @(#)PortMapEntry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import javax.xml.namespace.QName;


/** Used by SQLSE. By SQLSEServiceUnitManager to hold the service metadata
 * after parsing the ServiceUnitDescriptor.
 */
public class PortMapEntry {
    private QName mServiceName;
    private QName mEndPoint;
    private String mRole;
    private QName mPartnerLink;

    /**
     * Constructor
     * @param service
     * @param endpoint
     * @param role
     * @param partnerlink
     */
    PortMapEntry(final QName service, final QName endpoint, final String role,
        final QName partnerlink) {
        mServiceName = service;
        mEndPoint = endpoint;
        mRole = role;
        mPartnerLink = partnerlink;
    }

    /**
     * returns the service name.
     * @return QName
     */
    QName getServiceName() {
        return mServiceName;
    }

    /**
     * Returns the endpoint.
     * @return QName
     */
    QName getEndPoint() {
        return mEndPoint;
    }

    /**
     * Returns the role.
     * @return role
     */
    protected String getRole() {
        return mRole;
    }

    /**
     * Returns the partnerlink.
     * @return QName
     */
    QName getPartnerLink() {
        return mPartnerLink;
    }
}
