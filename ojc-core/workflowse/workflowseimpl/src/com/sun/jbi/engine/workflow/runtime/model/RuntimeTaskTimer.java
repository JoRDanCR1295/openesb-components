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
 * @(#)RuntimeTaskTimer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;


import java.util.Date;
import java.util.concurrent.ScheduledFuture;

import com.sun.jbi.workflow.model.DeadlineOrDuration;

public interface RuntimeTaskTimer {

	Long getId();
    
	DeadlineOrDuration getTimerMetaData();
	
	RuntimeTask getTask();
	
        void setTimerTask( ScheduledFuture<?>  tt);
        
        ScheduledFuture<?>  getTimerTask();
        
        boolean cancel();
        
        void setDueDate(Date dueDate);
        
        Date getDueDate();
        
//	RuntimeTaskTimer.TimerState getState();
//	
//	static enum TimerState {
//        
//        ACTIVE (0, "Active"),
//        EXPIRED (1, "Expired"),
//        CANCELLED (2, "Cancelled");
//        
//        
//        private int mState;
//        private String mDescription;        
//        
//        public String toString() {
//            // TODO Auto-generated method stub
//            return mDescription;
//        }
//        private TimerState(int state, String description) {
//            mState = state;
//            mDescription = description;
//        }
//         public String getDescription() {
//            return mDescription;
//        }
//        public int getState() {
//            return mState;
//        }        
//        
//        
//    }

}
