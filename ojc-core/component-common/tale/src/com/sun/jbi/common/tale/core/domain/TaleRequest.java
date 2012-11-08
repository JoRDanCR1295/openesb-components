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
 * @(#)TaleRequest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

import java.util.ArrayList;
import java.util.Collection;

/**
 * 
 * @author Kevan Simpson
 */
public class TaleRequest {
    private SourceInfo mSourceInfo;
    private int mCode;
    private String mDetails;        // optional
    private String mDisplayMessage; // optional
    private Payload mPayload;       // optional
    private EnvironmentInfo mEnvironmentInfo;
    private ExchangeInfo mExchangeInfo;
    private Collection<MessageTracking> mMessageTracking;
    
    public static TaleRequest copy(TaleRequest req) {
        TaleRequest copy = null;
        if (req != null) {
            copy = new TaleRequest();
            copy.setSourceInfo(SourceInfo.copy(req.getSourceInfo()));
            copy.setPayload(Payload.copy(req.getPayload()));
            copy.setCode(req.getCode());
            copy.setDetails(req.getDetails());
            copy.setDisplayMessage(req.getDisplayMessage());
            copy.setEnvironmentInfo(req.getEnvironmentInfo());
            copy.setExchangeInfo(req.getExchangeInfo());
            copy.setMsgTrackingList(req.getMsgTrackingList());
        }
        return copy;
    }
    
    public TaleRequest() {
        mSourceInfo = new SourceInfo();
        mEnvironmentInfo = new EnvironmentInfo();
        mExchangeInfo = new ExchangeInfo();
        mMessageTracking = new ArrayList<MessageTracking>();
    }
    
    public TaleRequest(SourceInfo info, int code, String details, String displayMsg, Payload payload) {
        mSourceInfo = info;
        mCode = code;
        mDetails = details;
        mDisplayMessage = displayMsg;
        mPayload = payload;
    }

    /**
     * @return the sourceInfo
     */
    public SourceInfo getSourceInfo() {
        return mSourceInfo;
    }

    /**
     * @param sourceInfo the sourceInfo to set
     */
    public void setSourceInfo(SourceInfo sourceInfo) {
        mSourceInfo = sourceInfo;
    }

    /**
     * @return the code
     */
    public int getCode() {
        return mCode;
    }

    /**
     * @param code the code to set
     */
    public void setCode(int code) {
        mCode = code;
    }

    /**
     * @return the details
     */
    public String getDetails() {
        return mDetails;
    }

    /**
     * @param details the details to set
     */
    public void setDetails(String details) {
        mDetails = details;
    }

    /**
     * @return the displayMessage
     */
    public String getDisplayMessage() {
        return mDisplayMessage;
    }

    /**
     * @param displayMessage the displayMessage to set
     */
    public void setDisplayMessage(String displayMessage) {
        mDisplayMessage = displayMessage;
    }

    /**
     * @return the payload
     */
    public Payload getPayload() {
        return mPayload;
    }

    /**
     * @param payload the payload to set
     */
    public void setPayload(Payload payload) {
        mPayload = payload;
    }
    
    /**
     * @return the mEnvironmentInfo
     */
    public EnvironmentInfo getEnvironmentInfo() {
        return mEnvironmentInfo;
    }

    /**
     * @param environmentInfo the mEnvironmentInfo to set
     */
    public void setEnvironmentInfo(EnvironmentInfo environmentInfo) {
        mEnvironmentInfo = environmentInfo;
    }

    /**
     * @return the mExchangeInfo
     */
    public ExchangeInfo getExchangeInfo() {
        return mExchangeInfo;
    }

    /**
     * @param exchangeInfo the mExchangeInfo to set
     */
    public void setExchangeInfo(ExchangeInfo exchangeInfo) {
        mExchangeInfo = exchangeInfo;
    }
    
    /**
     * @return the mMessageTracking
     */
    public Collection<MessageTracking> getMsgTrackingList() {
        return mMessageTracking;
    }
    /**
     * @param attachments the mMessageTracking to set
     */
    public void setMsgTrackingList(Collection<MessageTracking> msgTracks) {
        mMessageTracking = msgTracks;
    }
    
    public void addMsgTracking(String trackId) {
        MessageTracking msgTrack = new MessageTracking(trackId);
        mMessageTracking.add(msgTrack);
    }

}
