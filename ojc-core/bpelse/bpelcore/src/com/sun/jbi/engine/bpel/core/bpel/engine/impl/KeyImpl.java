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
 * @(#)KeyImpl.java 
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
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * Key implementation
 *
 * @author Sun Microsystems
 */
public class KeyImpl {
	
    private PartnerLink mPartnerLink;
    private QName mPortType;
    private String mOper;
    private Object mMEID;
    private int mType;
    private HashMap mCorrSetProperties;

    /**
     * Creates a new KeyImpl object.
     *
     * @param partnerLink partner link
     * @param pType port type
     * @param oper operation
     * @param meID message exchange ID
     * @param type event type
     * @param corrSetProperties map of correlationsets properties
     */
    public KeyImpl(PartnerLink partnerLink, QName pType, String oper, Object meID,
        int type, HashMap corrSetProperties) {
        mPartnerLink = partnerLink;
        mPortType = pType;
        mOper = oper;
        mMEID = meID;
        mType = type;
        mCorrSetProperties = corrSetProperties;
    }

    /**
     * hashcode
     *
     * @return int hash code
     */
    public int hashCode() {
        return mMEID.hashCode();
    }

    /**
     * equals
     *
     * @param obj Object
     *
     * @return boolean: if objects are equals, returns true; otherwise, returns false
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof KeyImpl)) {
            return false;
        }

        if (obj == this) {
            return true;
        }

        KeyImpl key = (KeyImpl) obj;

        boolean retFlag = (mType == key.mType) &&
            checkEquals(mPartnerLink, key.mPartnerLink) &&
            checkEquals(mPortType, key.mPortType) &&
            checkEquals(mOper, key.mOper) && checkEquals(mMEID, key.mMEID);

        // TODO Need to do correlation check here.
        if (retFlag && (key.mCorrSetProperties != null)) {
            retFlag = CorrelationSet.match(key.mCorrSetProperties,
                    mCorrSetProperties);
        }

        return retFlag;
    }

    /**
     * gets properties
     *
     * @return Returns the mCorrelationSet.
     */
    public HashMap getProperties() {
        return mCorrSetProperties;
    }

    /**
     * gets message exchange key
     *
     * @return Returns the mMEID.
     */
    public Object getMessageExchangeKey() {
        return mMEID;
    }

    /**
     * gets operation
     *
     * @return Returns the mOper.
     */
    public String getOperation() {
        return mOper;
    }

    /**
     * gets partner link
     *
     * @return Returns the mPartnerLink.
     */
    public PartnerLink getPartnerLink() {
        return mPartnerLink;
    }

    /**
     * gets portname
     *
     * @return Returns the mPortType.
     */
    public QName getPort() {
        return mPortType;
    }

    /**
     * gets event type
     *
     * @return Returns the mType.
     */
    public int getType() {
        return mType;
    }

    private boolean checkEquals(Object o1, Object o2) {
        return (o1 == null) ? (o2 == null) : o1.equals(o2);
    }

    /**
     * toString
     *
     * @return String String
     */
    public String toString() {
        String str1 = I18n.loc("BPCOR-3013: type =");
        String str2 = I18n.loc("BPCOR-3014: PartnerLink =");
        String str3 = I18n.loc("BPCOR-3015: PortType =");
        String str4 = I18n.loc("BPCOR-3016: Operation =");
        String str5 = I18n.loc("BPCOR-3017: MessageExhangeId =");
        return  str1 + mType + str2 + mPartnerLink + //$NON-NLS-1$ //$NON-NLS-2$
        str3 + mPortType + str4 + mOper + //$NON-NLS-1$ //$NON-NLS-2$
        str5 + mMEID; //$NON-NLS-1$
    }
}
