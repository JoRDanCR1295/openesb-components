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
 * @(#)RequestReplyKeyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;


import java.util.HashMap;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationSet;


/**
 * Request/Reply key implementation
 *
 * @author Sun Microsystems
 */
public class RequestReplyKeyImpl {
    private PartnerLink mPartnerLink;
    private QName mPortType;
    private String mOper;
    private String mMEID;
    private HashMap mCorrSetProperties;
    private String mBPInstanceID;
    private int mHashCode;

    /** PartnerLink ID */
//    QName mPartnerIdentifier;

    /**
     * Creates a new RequestReplyKeyImpl object.
     *
     * @param partnerLink partner link
     * @param pType port type
     * @param oper operation
     * @param corrSetProperties map of correlationsets properties
     * @param messageExchange message exchange ID
     * @param bpInstanceID BP process instance thread ID
     */
    public RequestReplyKeyImpl(PartnerLink partnerLink, QName pType,
            String oper, HashMap corrSetProperties, String messageExchange,
            String bpInstanceID) {
        mPartnerLink = partnerLink;
        mPortType = pType;
        mOper = oper;
        mMEID = messageExchange;
        mCorrSetProperties = corrSetProperties;
        mBPInstanceID = bpInstanceID;
        mHashCode = (mPartnerLink.getPartnerLinkType().toString() + mPortType + mOper).hashCode();
    }

    /**
     * hash code
     * 
     * @return int hash code
     */
    public int hashCode() {
        return mHashCode;
    }

    /**
     * equals
     *
     * @param obj object
     *
     * @return boolean: if objects are equal, returns true; otherwise, returns false
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof RequestReplyKeyImpl)) {
            return false;
        }

        if (obj == this) {
            return true;
        }

        RequestReplyKeyImpl key = (RequestReplyKeyImpl) obj;

        if ((mMEID != null) && !(mMEID.trim().equals(""))) { //$NON-NLS-1$
            if (mMEID.equals(key.mMEID)) {
                return true;
            }
        }

        boolean retFlag = checkEquals(mPartnerLink, key.mPartnerLink) &&
            checkEquals(mPortType, key.mPortType) &&
            checkEquals(mOper, key.mOper);

        // TODO Need to do correlation check here.
        if (retFlag && (key.mCorrSetProperties != null)) {
            retFlag = CorrelationSet.match(key.mCorrSetProperties,
                    mCorrSetProperties);
        }

        return retFlag;
    }

    private boolean checkEquals(Object o1, Object o2) {
        return (o1 == null) ? (o2 == null) : o1.equals(o2);
    }
}
