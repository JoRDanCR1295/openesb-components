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

package com.sun.jbi.engine.bpel;

import javax.xml.namespace.QName;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class PortMapEntry {
    private QName mServiceName;
    private QName mEndPoint;
    private String mRole;
    private QName mPartnerLink;
    private String mRoleName;
    private QName mPartnerLinkType;
    
    /**
     * Creates a new instance of PortMapEntry
     *
     * @param service Service QName
     * @param endpoint Endpoint QName
     * @param role Role
     * @param partnerlink PartnerLink QName
     * @param roleName Role name
     * @param partnerLinkType partner link type
     */
    public PortMapEntry(QName service, QName endpoint, String role,
        QName partnerlink, QName partnerLinkType, String roleName) {
        mServiceName = service;
        mEndPoint = endpoint;
        mRole = role;
        mPartnerLink = partnerlink;
        mPartnerLinkType = partnerLinkType;
        mRoleName = roleName;
    }

    /**
     * get Service
     *
     * @return QName Service
     */
    public QName getServiceName() {
        return mServiceName;
    }

    /**
     * get Endpoint
     *
     * @return QName Endpoint
     */
    public QName getEndPoint() {
        return mEndPoint;
    }

    /**
     * get Role
     *
     * @return String role
     */
    public String getRole() {
        return mRole;
    }

    /**
     * get PartnerLink QName
     *
     * @return QName PartnerLink
     */
    public QName getPartnerLink() {
        return mPartnerLink;
    }

    /**
     * get PartnerLinkType QName
     *
     * @return QName PartnerLinkType
     */
    public QName getPartnerLinkType() {
        return mPartnerLinkType;
    }

    /**
     * get Role Name
     *
     * @return String Role name
     */
    public String getRoleName() {
        return mRoleName;
    }
}
