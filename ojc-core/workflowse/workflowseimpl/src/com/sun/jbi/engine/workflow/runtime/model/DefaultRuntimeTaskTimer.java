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
 * @(#)DefaultRuntimeTaskTimer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;



import com.sun.jbi.workflow.model.DeadlineOrDuration;
import java.util.Date;
import java.util.TimerTask;
import java.util.concurrent.ScheduledFuture;

public class DefaultRuntimeTaskTimer implements RuntimeTaskTimer {

	private Long mTimerId;
	
	private DeadlineOrDuration mDeadlineOrDuration;
	
	private Long mRuntimeTaskId;
    
    private TaskManager mTaskManager;
	
     private ScheduledFuture<?>    mTT;
        
//	private TimerState mState = TimerState.ACTIVE;
	private Date dueDate;
        
	public DefaultRuntimeTaskTimer(Long timerId, DeadlineOrDuration deadlineOrDuration,TaskManager taskManager,  Long taskId) {
		this.mTimerId = timerId;
		this.mDeadlineOrDuration = deadlineOrDuration;
		this.mTaskManager = taskManager;
        mRuntimeTaskId = taskId;
	}
	
	public Long getId() {
		return mTimerId;
	}
	
	public DeadlineOrDuration getTimerMetaData() {
		return mDeadlineOrDuration;
	}
	
	public RuntimeTask getTask() {
		return  mTaskManager.getTask(mRuntimeTaskId);
	}

        public void setTimerTask(ScheduledFuture<?> tt) {
            this.mTT = tt;
        }
        
        public ScheduledFuture<?> getTimerTask() {
           return this.mTT;
        }
        
        public boolean cancel() {
            boolean result = false;
            if(this.mTT != null) {
                result = this.mTT.cancel(true);
            }
            
            return result;
        }

    public void setDueDate(Date dueDate)
    {
        this.dueDate = dueDate;
    }

    public Date getDueDate()
    {
        return this.dueDate;
    }

        
//	public TimerState getState() {
//		return mState;
//	}


    

    
}
