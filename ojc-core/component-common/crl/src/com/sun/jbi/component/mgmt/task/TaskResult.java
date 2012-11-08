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
 * @(#)TaskResult.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.mgmt.task;

import javax.jbi.component.ServiceUnitManager;

/**
 * Used to report either the results of the JBI implementation’s execution 
 * of the task (a &quot;framework&quot; task result) OR the results of 
 * interaction(s) with a component (a &quot;component&quot; task result).
 * <p>
 * A framework task result includes an optional element, 
 * <code>is-cause-framework</code>, which the implementation MUST set to “YES” 
 * in cases where the JBI implementation is the cause of the task failing to execute.
 * <p>
 * A component task result includes the component’s unique name and details 
 * about the component task results. For example, this is used by the 
 * component’s {@link ServiceUnitManager} implementation to return the results 
 * of {@link ServiceUnitManager#deploy(String, String)} and 
 * {@link ServiceUnitManager#undeploy(String, String)}.
 *  
 * @author Kevan Simpson
 */
public class TaskResult implements TaskElement {
    /** The component element name: <code>component-task-result</code>. */
    public static final String COMPONENT_ELEMENT_NAME = "component-task-result";
    /** The framework element name: <code>frmwk-task-result</code>. */
    public static final String FRMWK_ELEMENT_NAME = "frmwk-task-result";
    /** The is cause framework element name: <code>is-cause-framework</code>. */
    public static final String IS_CAUSE_FRAMEWORK_ELEMENT_NAME = "is-cause-framework";
    /** The component name element name: <code>component-name</code>. */
    public static final String COMPONENT_NAME_ELEMENT_NAME = "component-name";
    

    private Boolean mIsCauseFramework = null;   // unset
    private String mComponentName = null;
    private TaskResultDetails mDetails = null;
    private boolean mFrameworkResult = true;
    
    public TaskResult(TaskResultDetails details) {
        mDetails = details;
    }
    
    public TaskResult(TaskResultDetails details, boolean isCauseFramework) {
        this(details);
        mIsCauseFramework = Boolean.valueOf(isCauseFramework);
    }

    public TaskResult(String componentName, TaskResultDetails details) {
        this(details);
        mComponentName = componentName;
        mFrameworkResult = false;
    }

    public boolean isFrameworkResult() { return mFrameworkResult; }

    public Boolean isCauseFramework() { return mIsCauseFramework; }
    public String getComponentName() { return mComponentName; }
    public TaskResultDetails getDetails() { return mDetails; }

    /** @see com.sun.jbi.component.mgmt.task.TaskElement#accept(com.sun.jbi.component.mgmt.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }
}
