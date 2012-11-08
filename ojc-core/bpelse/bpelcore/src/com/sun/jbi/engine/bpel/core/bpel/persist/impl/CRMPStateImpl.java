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
 * @(#)CRMPStateImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import com.sun.jbi.engine.bpel.core.bpel.persist.State;


class CRMPStateImpl implements State.CRMPState {
    private String mCrmpInvokeId;
    private String mPLink;
    private String mOper;
    private String mBPELME;
    private long mReplyVarId;
    
    char[] mRespObj;
    
    boolean mIsUpdateForReply;
    
    CRMPStateImpl(String crmpInvokeId, String partnerLink, String operation, 
            String bpelMesgExchange) {
        mCrmpInvokeId = crmpInvokeId;
        mPLink = partnerLink;
        mOper = operation;
        mBPELME = bpelMesgExchange;
    }
    
    CRMPStateImpl(String partnerLink, String operation, String bpelMesgExchange, 
            long replyVarId, char[] responseObj) {
        mPLink = partnerLink;
        mOper = operation;
        mBPELME = bpelMesgExchange;
        mReplyVarId = replyVarId;
        mRespObj = responseObj;
        mIsUpdateForReply = true;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getBPELME()
     */
    public String getBPELME() {
        return mBPELME;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getCrmpInvokeId()
     */
    public String getCrmpInvokeId() {
        return mCrmpInvokeId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getOper()
     */
    public String getOper() {
        return mOper;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getPLink()
     */
    public String getPLink() {
        return mPLink;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getReplyVarId()
     */
    public long getReplyVarId() {
        return mReplyVarId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#getRespObj()
     */
    public char[] getRespObj() {
        return mRespObj;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State.CRMPState#isUpdateForReply()
     */
    public boolean isUpdateForReply() {
        return mIsUpdateForReply;
    }
}
