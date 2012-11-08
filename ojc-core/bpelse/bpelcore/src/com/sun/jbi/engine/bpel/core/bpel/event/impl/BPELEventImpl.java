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
 * @(#)$Id: BPELEventImpl.java,v 1.6 2008/02/06 21:40:38 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.sql.Timestamp;

import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;

public abstract class BPELEventImpl implements BPELEvent {
	
//	private QName mBPELName;
//	private String mBPId;
	private String mEventId;
	private EventType mType;
	private Timestamp mDate;
	private String mEngineId;
    private Integer mInstanceHash;

	BPELEventImpl(String engineId, String eventId, EventType type) {		
		mEngineId = engineId;
//		mBPELName = name;
//		mBPId = id;
		mEventId = eventId;
		mType = type;
		mDate = new Timestamp (System.currentTimeMillis());
	}

//	public QName getBPELName() {
//		// TODO Auto-generated method stub
//		return mBPELName;
//	}
//
//	public String getBPID() {
//		// TODO Auto-generated method stub
//		return mBPId;
//	}

	public String getEventId() {
		// TODO Auto-generated method stub
		return mEventId;
	}

	public EventType getEventType() {
		// TODO Auto-generated method stub
		return mType;
	}

	public Timestamp getTimeStamp() {
		// TODO Auto-generated method stub
		return mDate;
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		StringBuffer buffer = new StringBuffer ();
		buffer.append("<<<<<<<<<<<BPELEvent:");
		buffer.append(getEventType());
		buffer.append("<<<<<<<<<<<\n");		
		return buffer.toString();
	}

	public String getEngineId() {
		// TODO Auto-generated method stub
		return mEngineId;
	}
    
    public  int getInstanceHash () {
        if (mInstanceHash != null) {
            return mInstanceHash;
        }
       if (getInstanceId() == null) {
           mInstanceHash = 0;
       }else {
           mInstanceHash = getInstanceId().hashCode();           
       }
       return mInstanceHash;
   }
	
}
