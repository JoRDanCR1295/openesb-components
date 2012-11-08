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
 * @(#)TaskItem.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.sql.ResultSet;
import java.util.Calendar;
import java.util.Date;

import com.sun.jbi.engine.workflow.db.hibernate.TaskInstance;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.TaskManagerHelper;

/**
 *
 */
public class TaskItem {
    
    private Long  mId;
    private String mDescription;
    private int mPriority;
    private RuntimeTask.TaskState mState;
    private String mAssignedTo;
    private String mClaimedBy;
    private Calendar mCreateDate;
    private String mKeywords;
    private String mTaskDefId;
    private Calendar mDeadline;
    private String mCompletedBy;
    
    public TaskItem(TaskInstance taskInstance) {
       mId = taskInstance.getId();
       mDescription = taskInstance.getTitle();
       mPriority = taskInstance
               .getPriority();
       mState = RuntimeTask.TaskState.getTaskState(taskInstance
               .getStatus());
       mAssignedTo = taskInstance.getAssignedTo();
       mClaimedBy = taskInstance.getOwner();
       Calendar cal = Calendar.getInstance();
       cal.setTime(taskInstance.getCreateDate());
       mCreateDate = cal;
       
       Date deadline = taskInstance.getDeadline();
       if (deadline != null) {
           Calendar deadlineCal = Calendar.getInstance();
           deadlineCal.setTime(deadline);
           mDeadline = deadlineCal;           
       }
       mTaskDefId = taskInstance.getTaskDefId();
       mCompletedBy = taskInstance.getCompletedBy();      
       mKeywords = taskInstance.getKeyword();
    }
    
    public TaskItem(Object []  taskInstanceInfo) {
        mId = (Long) taskInstanceInfo [0];
        mDescription =(String)  taskInstanceInfo [1];
        mPriority = ((Integer) taskInstanceInfo [3]).intValue();

        mState = RuntimeTask.TaskState.getTaskState((Integer) taskInstanceInfo [2]);
        mAssignedTo = (String) taskInstanceInfo [5];
        mClaimedBy =(String) taskInstanceInfo [4];
        Calendar cal = Calendar.getInstance();
        cal.setTime((Date) taskInstanceInfo [9]);
        mCreateDate = cal;
        mKeywords = (String) taskInstanceInfo [10];
        
        Date deadline = (Date)  taskInstanceInfo [11];
        if (deadline != null) {
            Calendar deadlineCal = Calendar.getInstance();
            deadlineCal.setTime(deadline);
            mDeadline = deadlineCal;           
        }
        mCompletedBy = (String)  taskInstanceInfo [12];
        mTaskDefId =  (String) taskInstanceInfo [13];        
     }
    
    public TaskItem(ResultSet  tiRs) throws Exception{
        mId =  tiRs.getLong(1);
        mDescription =(String)  tiRs.getString(2);
        mPriority = tiRs.getInt(4);

        mState = RuntimeTask.TaskState.getTaskState( tiRs.getInt(3));
        mAssignedTo =  tiRs.getString(6);
        mClaimedBy = tiRs.getString(5);
        Calendar cal = Calendar.getInstance();
        cal.setTime(tiRs.getDate(10));
        mKeywords = tiRs.getString (11);
        mCreateDate = cal;
     }    
    
    public TaskItem(RuntimeTask rTask) {
        mId = rTask.getId();
        mDescription = rTask.getDescription();
        mPriority =rTask.getPriority();
        mState = rTask.getState();
        mAssignedTo = TaskManagerHelper.makeString(rTask.getAssignedTo());
        mClaimedBy = TaskManagerHelper.makeString(rTask.getClaimedBy());  
        mCreateDate = rTask.getCreateDate();
        mKeywords = rTask.getKeywords();  
        mDeadline = rTask.getDeadline();      
        mCompletedBy = rTask.getCompletedBy() == null ? "" :  rTask.getCompletedBy() .getName();
        mTaskDefId =  rTask.getTaskMeta().getQName().toString();
    }    
    public Long getId() {
        return mId;
    }
    
    public String getDescription() {
        return mDescription;
    }
    
    public int getPriority() {
        return mPriority;
    }
    
    public RuntimeTask.TaskState getState() {
        return mState;
    }
    
    public String getAssignedTo() {
        return mAssignedTo;
    }
    
    public String getClaimedBy() {
        return mClaimedBy;
    }
    
    public Calendar getCreatedDate () {
        return mCreateDate;
    }
    
    public String getKeywords () {
        return mKeywords;
    }
  
    public String getCompletedBy() {
        return mCompletedBy;
    }

    public Calendar getDeadline() {
        return mDeadline;
    }

    public String getTaskDefId() {
        return mTaskDefId;
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((mId == null) ? 0 : mId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final TaskItem other = (TaskItem) obj;
        if (mId == null) {
            if (other.mId != null)
                return false;
        } else if (!mId.equals(other.mId)) {
            return false;
        } else if (mId.equals(other.mId)) {
            return true;
        }
        return false;
    }
    
}
