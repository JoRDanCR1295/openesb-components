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
 * @(#)TaskAssignee.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.hibernate;

import java.io.Serializable;
import java.util.Date;

public class TaskAssignee implements Serializable {

    private Long id;

    private String assignee;

    private Integer assignedStatus;

    private Date updateDate;
    
    private Date startDate;

    private Boolean activeAssignee;

    private TaskInstance taskInstance;
    
    public Integer getAssignedStatus() {
        return assignedStatus;
    }

    public void setAssignedStatus(Integer assignedStatus) {
        this.assignedStatus = assignedStatus;
    }

    public String getAssignee() {
        return assignee;
    }

    public void setAssignee(String assignee) {
        this.assignee = assignee;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public Date getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(Date updateDate) {
        this.updateDate = updateDate;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date updateDate) {
        this.startDate = updateDate;
    }
    
    public TaskInstance getTaskInstance() {
        return taskInstance;
    }

    public void setTaskInstance(TaskInstance taskInstance) {
        this.taskInstance = taskInstance;
    }

    public Boolean getActiveAssignee()
    {
        return activeAssignee;
    }

    public void setActiveAssignee(Boolean activeAssignee)
    {
        this.activeAssignee = activeAssignee;
    }

    @Override
    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((assignedStatus == null) ? 0 : assignedStatus.hashCode());
        result = PRIME * result + ((assignee == null) ? 0 : assignee.hashCode());
        result = PRIME * result + ((id == null) ? 0 : id.hashCode());
        result = PRIME * result + ((updateDate == null) ? 0 : updateDate.hashCode());
        result = PRIME * result + ((activeAssignee == null) ? 0 : activeAssignee.hashCode());
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
        final TaskAssignee other = (TaskAssignee) obj;
        if (id == null) {
            if (other.id != null)
                return false;
        } else if (!id.equals(other.id)) {
            return false;
        } else if (id.equals(other.id)) {
            return true;
        }
        if (assignedStatus == null) {
            if (other.assignedStatus != null)
                return false;
        } else if (!assignedStatus.equals(other.assignedStatus))
            return false;
        if (assignee == null) {
            if (other.assignee != null)
                return false;
        } else if (!assignee.equals(other.assignee))
            return false;

        if (updateDate == null) {
            if (other.updateDate != null)
                return false;
        } else if (!updateDate.equals(other.updateDate))
            return false;
         
        if (activeAssignee == null) {
            if (other.activeAssignee != null)
                return false;
        } else if (!activeAssignee.equals(other.activeAssignee))
            return false;
        return true;
    } 
 
}
