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
 * @(#)TaskEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.process;

import java.util.EventObject;

import org.w3c.dom.Element;

public class TaskEvent extends EventObject {

	private long mTaskId;
	
	private Element mElement;
    
    private String mTaskRelId;
	
	public TaskEvent(Object source, long taskId, String taskRelId, Element element) {
		super(source);
		this.mTaskId = taskId;
		this.mElement = element;
        this.mTaskRelId = taskRelId;
	}
	
	public long getTaskId() {
		return this.mTaskId;
	}
	
	public Element getElement() {
		return this.mElement;
	}
    
    public String getTaskRelId()  {
        return mTaskRelId;
    }
}
