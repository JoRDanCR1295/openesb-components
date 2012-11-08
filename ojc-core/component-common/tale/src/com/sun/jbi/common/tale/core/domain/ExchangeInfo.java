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
 * @(#)ExchangeInfo.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * Domain Object used for storing Exchange related information in the 
 * table EXCHANGE_INFO
 * @author Sun Microsystems
 */
public class ExchangeInfo {
    private String mExchangeID;
    private String mEndPointName;
    private String mServiceName;
    private String mOperationName;
    
    public static ExchangeInfo copy(ExchangeInfo info) {
        ExchangeInfo copy = null;
        if (info != null) {
            copy = new ExchangeInfo();
            copy.setExchangeID(info.getExchangeID());
            copy.setEndPointName(info.getEndPointName());
            copy.setServiceName(info.getServiceName());
            copy.setOperationName(info.getOperationName());
        }
        return copy;
    }

    /**
     * @return the mEndPointNameName
     */
    public String getEndPointName() {
        return mEndPointName;
    }

    /**
     * @param endPointNameName the mEndPointName to set
     */
    public void setEndPointName(String endPointName) {
        mEndPointName = endPointName;
    }

    /**
     * @return the mExchangeID
     */
    public String getExchangeID() {
        return mExchangeID;
    }

    /**
     * @param exchangeID the mExchangeID to set
     */
    public void setExchangeID(String exchangeID) {
        mExchangeID = exchangeID;
    }

    /**
     * @return the mOperationName
     */
    public String getOperationName() {
        return mOperationName;
    }

    /**
     * @param operationName the mOperationName to set
     */
    public void setOperationName(String operationName) {
        mOperationName = operationName;
    }

    /**
     * @return the mServiceName
     */
    public String getServiceName() {
        return mServiceName;
    }

    /**
     * @param serviceName the mServiceName to set
     */
    public void setServiceName(String serviceName) {
        mServiceName = serviceName;
    }

}
