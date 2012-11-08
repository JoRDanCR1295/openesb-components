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
 * @(#)$Id: TasklistResult.java,v 1.2 2010/02/15 19:22:54 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi.operations;

import java.util.List;

import com.sun.jbi.engine.workflow.clientapi.TaskItem;

public class TasklistResult {
    
    private List<TaskItem> mTaskList;
    private int mTotalRecords;
    
    
    public List<TaskItem> getTaskList () {
        return mTaskList;
        
    }

    public int getTotalRecords () {
        return mTotalRecords;
    }
    
    public int getReturnedRecords () {
        return mTaskList ==null ? 0 : mTaskList.size();
    }
    
    public void setTaskList (List<TaskItem>  taskItems) {
        mTaskList = taskItems;
    }
    
    public void setTotalRecords (int total) {
        mTotalRecords = total;
    }
}
