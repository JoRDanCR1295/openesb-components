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
 * @(#)Notification.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.util.ArrayList;
import java.util.List;

/**
 * Notification.java
 *
 * Created on February 7, 2007, 11:15 AM
 *
 * @author Bing Lu
 */
public class Notification {
    private String mId;
    private Object mEvent;
    private List<String> mSeqIds;
    private List<String> mTags;
    private boolean error = false;
    private Object transaction = null; 

    /** Creates a new instance of Notification */
    public Notification(String id, Object event, String seqId) {
        this(id, event, seqId, "+");
    }

    public Notification(String id, Object event, String seqId, String tag) {
        mId = id;
        mEvent = event;
        mSeqIds = new ArrayList<String>();
        mSeqIds.add(seqId);
        mTags = new ArrayList<String>();
        mTags.add(tag);
    }

    public Notification(String id, Object event, List<String> seqIds) {
        mId = id;
        mEvent = event;
        mSeqIds = seqIds;
        mTags = new ArrayList<String>();
        for (int i = 0; i < seqIds.size(); i++) {
            mTags.add("+");
        }
    }

    public Notification(String id, Object event, List<String> seqIds, List<String> tags) {
        mId = id;
        mEvent = event;
        mSeqIds = seqIds;
        mTags = tags;
    }
    
    public String getId() {
        return mId;
    }

    public int getBatchSize() {
        return mSeqIds.size();
    }
    
    public Object getEvent() {
        return mEvent;
    }
    
    public String getSeqId(int i) {
        return mSeqIds.get(i);
    }
    
    public String getTag(int i) {
        return mTags.get(i);
    }

    public boolean isError() {
        return error;
    }

    public void setError(boolean error) {
        this.error = error;
    }

    public Object getTransaction() {
        return transaction;
    }

    public void setTransaction(Object transaction) {
        this.transaction = transaction;
    }
    public String toString(){
        return "Notification with Id" + mId;
    }
    
}
