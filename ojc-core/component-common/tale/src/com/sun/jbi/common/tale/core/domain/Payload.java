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
 * @(#)Payload.java
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
public class Payload {
    //TODO: To support backward compatibility we need to store the exact same value for these codes 
    public enum Encode {
        Y, N
    };

    public enum EncodeMode {
        NOENCODING, ASCII, BASE64
    };

    public enum PayloadType {
        ORIGINAL_MSG, TRANSFORMED_MSG, BOTH_MSGS
    };

    public static final int VERSION = 1; 

    // we're ignoring PersistFlag and PersistMode for now...
    private Encode mEncodeFlag;
    private EncodeMode mEncodeMode;     // optional
    private PayloadType mPayloadType;
    private String mPayloadMessage;
    //private String mOriginalMessage;    // optional
    //private String mTransformedMessage; // optional
    private Collection<ExternalizedPart> mExternalizedParts = new ArrayList<ExternalizedPart>();
    private Collection<PayloadAttachment> mAttachments = new ArrayList<PayloadAttachment>();
    
    public static Payload copy(Payload p) {
        Payload copy = null;
        if (p != null) {
            copy = new Payload();
            copy.setEncodeFlag(p.getEncodeFlag());
            copy.setEncodeMode(p.getEncodeMode());
            //copy.setOriginalMessage(p.getOriginalMessage());
            copy.setPayloadType(p.getPayloadType());
            copy.setPayloadMessage(p.getPayloadMessage());
            //copy.setTransformedMessage(p.getTransformedMessage());
            copy.setExternalizedParts(p.getExternalizedParts());
            copy.setAttachments(p.getAttachments());
        }
        return copy;
    }
    /**
     * @return the encodeFlag
     */
    public Encode getEncodeFlag() {
        return mEncodeFlag;
    }
    /**
     * @param encodeFlag the encodeFlag to set
     */
    public void setEncodeFlag(Encode encodeFlag) {
        mEncodeFlag = encodeFlag;
    }
    /**
     * @return the encodeMode
     */
    public EncodeMode getEncodeMode() {
        return mEncodeMode;
    }
    /**
     * @param encodeMode the encodeMode to set
     */
    public void setEncodeMode(EncodeMode encodeMode) {
        mEncodeMode = encodeMode;
    }
    /**
     * @return the payloadType
     */
    public PayloadType getPayloadType() {
        return mPayloadType;
    }
    /**
     * @param payloadType the payloadType to set
     */
    public void setPayloadType(PayloadType payloadType) {
        mPayloadType = payloadType;
    }
    /**
     * @return the originalMessage
     *//*
    public String getOriginalMessage() {
        return mOriginalMessage;
    }
    *//**
     * @param originalMessage the originalMessage to set
     *//*
    public void setOriginalMessage(String originalMessage) {
        mOriginalMessage = originalMessage;
    }
    *//**
     * @return the transformedMessage
     *//*
    public String getTransformedMessage() {
        return mTransformedMessage;
    }
    *//**
     * @param transformedMessage the transformedMessage to set
     *//*
    public void setTransformedMessage(String transformedMessage) {
        mTransformedMessage = transformedMessage;
    }*/
    /**
     * @return the mPayloadMessage
     */
    public String getPayloadMessage() {
        return mPayloadMessage;
    }
    /**
     * @param payloadMessage the mPayloadMessage to set
     */
    public void setPayloadMessage(String payloadMessage) {
        mPayloadMessage = payloadMessage;
    }
    /**
     * @return the mAttachments
     */
    public Collection<PayloadAttachment> getAttachments() {
        return mAttachments;
    }
    /**
     * @param attachments the mAttachments to set
     */
    public void setAttachments(Collection<PayloadAttachment> attachments) {
        mAttachments = attachments;
    }
    
    public void addPayloadAttachment(String seqId, Object blob) {
        PayloadAttachment attchment = new PayloadAttachment();
        attchment.setSequenceID(seqId);
        attchment.setAttachment(blob);
        mAttachments.add(attchment);
    }
    /**
     * @return the mExternalizedParts
     */
    public Collection<ExternalizedPart> getExternalizedParts() {
        return mExternalizedParts;
    }
    
    /**
     * @param externalizedParts the mExternalizedParts to set
     */
    public void setExternalizedParts(Collection<ExternalizedPart> externalizedParts) {
        mExternalizedParts = externalizedParts;
    }
    
}
