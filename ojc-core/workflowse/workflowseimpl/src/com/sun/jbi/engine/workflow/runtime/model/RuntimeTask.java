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
 * @(#)RuntimeTask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model;

import java.util.Calendar;
import java.util.Collection;
import java.util.Set;

import org.apache.commons.jxpath.JXPathContext;

import com.sun.jbi.engine.workflow.EngineContext;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import com.sun.jbi.engine.workflow.process.LDAPConfig;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;

/**
 *
 * 
 */
public interface RuntimeTask {	
    
    public static final int DEFAULT_PRIORITY = 5;
   
    Long getId();
    
    void setId (Long id);
     
    String getDescription();
    
    void setDescription (String desc);
    
    Calendar getCreateDate();
    
    Calendar getEndDate ();
    
    void setCreateDate (Calendar date);
    
    void setEndDate (Calendar date);
    
    Collection<TaskPrincipal> getAssignedTo();
    
    void setAssignedTo(Collection<TaskPrincipal> users);
    
    void addAssignedTo(TaskPrincipal user);
    
    TaskPrincipal getClaimedBy();
    
    void setClaimedBy(TaskPrincipal user);
    
    void execute() throws TaskException;
    
    TaskInput getInput();
    
    TaskOutput getOutput();
    
    void setOutput(TaskOutput output);
    
    TaskState getState();
    
    void setState(TaskState state);
    
    int getPriority();
    
    void setPriority(int priority);

    void setCompletedBy (TaskPrincipal user);
    
    TaskPrincipal getCompletedBy ();
    
    Task getTaskMeta ();
    
    String getExchangeId ();
    
    void setExchangeId (String exchangeId);
    
    boolean isNew ();
    
    void setNew (boolean isNew);    
   
    RuntimeVariables getRuntimeVariables ();
    
    void setRuntimeVariables (RuntimeVariables runtimeVars);
    
    void setJXpathContext(JXPathContext jxpathContext);
    
    JXPathContext getJXpathContext ();
    
    LDAPConfig getLDAPConfig();    
    
    String getKeywords ();
    
    void setKeywords (String keywords);
    
    Calendar getDeadline() ;

   void setDeadline(Calendar deadline) ;
    
    
    /**
     * Initialize the task for runtime variables and input payload
     * @param context
     * @param inputMsg
     */
    void init (EngineContext context) throws TaskException;
    

    
    static enum TaskState {
        
        UNASSIGNED (0, "UnAssigned"),
        ASSIGNED (1, "Assigned"),
        CLAIMED (2, "Claimed"),
        COMPLETED (3, "Completed"),
        EXPIRED (4, "Expired"),
        ESCALATED (5, "Escalated"),
        ABORTED (6, "Aborted"),
        FAILED (7, "Failed");
        
        
        private int mState;
        private String mDescription;        
        
        public String toString() {
            return mDescription;
        }
        private TaskState(int state, String description) {
            mState = state;
            mDescription = description;
        }
         public String getDescription() {
            return mDescription;
        }
        public int getState() {
            return mState;
        }        
        
        public static TaskState getTaskState(Integer status) {
        	int statusInt = status.intValue();
            RuntimeTask.TaskState taskStatus = TaskState.UNASSIGNED;
            switch (statusInt) {
            case 0:
                taskStatus = TaskState.UNASSIGNED;
                break;
            case 1:
                taskStatus = TaskState.ASSIGNED;
                break;
            case 2:
                taskStatus = TaskState.CLAIMED;
                break;
            case 3:
                taskStatus = TaskState.COMPLETED;
                break;
            case 4:
                taskStatus = TaskState.EXPIRED;
                break;
            case 5:
                taskStatus = TaskState.ESCALATED;
                break;
            case 6:
                taskStatus = TaskState.ABORTED;
                break;
            case 7:
                taskStatus = TaskState.FAILED;
                break;           
                
            }
            
            return taskStatus;
        }
        
        public static boolean isTerminalStatus (Integer status) {
            int statusInt = status.intValue();
            boolean result = false;
            switch (statusInt) {
            case 3:
                result = true;
                break;
            case 4:
                result = true;
                break;
            case 6:
                result = true;
                break;
            case 7:
                result = true;
                break; 
              default : 
                  result = false;
            }
              
              return result;             
              
        }
        
        public static TaskState getTaskState(String status) {
        	RuntimeTask.TaskState taskStatus = TaskState.UNASSIGNED;
        	if(status != null) {
	        	if(status.equalsIgnoreCase(TaskState.ASSIGNED.toString())) {
	        		taskStatus =  TaskState.ASSIGNED;
	        	} else if(status.equalsIgnoreCase(TaskState.CLAIMED.toString())) {
	        		taskStatus =  TaskState.CLAIMED;
	        	} else if(status.equalsIgnoreCase(TaskState.COMPLETED.toString())) {
	        		taskStatus =  TaskState.COMPLETED;
	        	} else if(status.equalsIgnoreCase(TaskState.EXPIRED.toString())) {
	        		taskStatus =  TaskState.EXPIRED;
	        	} else if(status.equalsIgnoreCase(TaskState.ESCALATED.toString())) {
	        		taskStatus =  TaskState.ESCALATED;
	        	} else if(status.equalsIgnoreCase(TaskState.ABORTED.toString())) {
	        		taskStatus =  TaskState.ABORTED;
	        	} else if(status.equalsIgnoreCase(TaskState.FAILED.toString())) {
	        		taskStatus =  TaskState.FAILED;
	        	}
        	}
        	
        	return taskStatus;
        }
        
    }


    

    

}
