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
 * @(#)JbiTaskResult.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;

/**
 * Used to report the results of the implementation's execution of the task, 
 * and, optionally, the result of the component's part of executing the task. 
 * This latter feature will appear only if applicable (the task involves 
 * interaction with a component, and the task proceeds to the point where that 
 * interaction was started.)
 * 
 * @author Kevan Simpson
 */
public class JbiTaskResult implements TaskElement {
    /** The element name: <code>component-task-result</code>. */
    public static final String ELEMENT_NAME = "jbi-task-result";

    private TaskResult mFrmwkTaskResult = null;
    private TaskResult[] mComponentTaskResults = null;
    
    public JbiTaskResult(TaskResult frmwkResult, TaskResult... componentTaskResults) {
        mFrmwkTaskResult = frmwkResult;
        mComponentTaskResults = componentTaskResults;
    }
    
    public TaskResult getFrmwkTaskResult() { return mFrmwkTaskResult; }
    public TaskResult[] getComponentTaskResults() { return mComponentTaskResults; }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskElement#accept(com.sun.jbi.component.toolkit.util.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }
}
