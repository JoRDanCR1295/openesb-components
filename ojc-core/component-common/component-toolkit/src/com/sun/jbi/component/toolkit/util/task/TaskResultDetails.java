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
 * @(#)TaskResultDetails.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;

/**
 * Used to report the task ID, result (SUCCESS or FAILED), and, optionally, 
 * a typed (ERROR, WARNING, or INFO) message. Zero or more task status 
 * messages can be included (for dealing with multiple interactions with 
 * components). Finally, optional exception information can be provided.
 * 
 * @author Kevan Simpson
 */
public class TaskResultDetails implements TaskElement {
    public enum MessageType { ERROR, WARNING, INFO };
    
    /** The component element name: <code>component-task-result-details</code>. */
    public static final String COMPONENT_ELEMENT_NAME = "component-task-result-details";
    /** The framework element name: <code>frmwk-task-result-details</code>. */
    public static final String FRMWK_ELEMENT_NAME = "frmwk-task-result-details";
    /** The element name: <code>task-result-details</code>. */
    public static final String ELEMENT_NAME = "task-result-details";
    /** The task id element name: <code>task-id</code>. */
    public static final String TASK_ID_ELEMENT_NAME = "task-id";
    /** The task result element name: <code>task-result</code>. */
    public static final String TASK_RESULT_ELEMENT_NAME = "task-result";
    /** The message type element name: <code>message-type</code>. */
    public static final String MESSAGE_TYPE_ELEMENT_NAME = "message-type";
    /** The task status message element name: <code>task-status-msg</code>. */
    public static final String TASK_STATUS_MSG_ELEMENT_NAME = "task-status-msg";
    /** The locale element name: <code>locale</code>. */
    public static final String LOCALE_ELEMENT_NAME = "locale";
    

    private String mTaskId;
    private boolean mSuccess = false;
    private MessageType mMessageType = null;
    private MsgLocInfo[] mTaskStatusMsgs = null;
    private ExceptionInfo[] mExceptionInfos = null;
    private String mLocale = null;
    private boolean mFrameworkDetails = false;
    
    public TaskResultDetails(String taskId, 
                             boolean success, 
                             MessageType messageType,
                             MsgLocInfo[] taskStatusMsgs,
                             ExceptionInfo[] infos) {
        this(taskId, success, messageType, taskStatusMsgs, infos, false);
    }

    public TaskResultDetails(String taskId, 
                             boolean success, 
                             MessageType messageType,
                             MsgLocInfo[] taskStatusMsgs,
                             ExceptionInfo[] infos,
                             String locale) {
        this(taskId, success, messageType, taskStatusMsgs, infos, true);
        mLocale = locale;
}

    private TaskResultDetails(String taskId, 
                              boolean success, 
                              MessageType messageType,
                              MsgLocInfo[] taskStatusMsgs,
                              ExceptionInfo[] infos,
                              boolean isFrmwk) {
        mTaskId = taskId;
        mSuccess = success;
        mMessageType = messageType;
        mTaskStatusMsgs = taskStatusMsgs;
        mExceptionInfos = infos;
        mFrameworkDetails = isFrmwk;
    }

    public String getLocale() { return mLocale; }
    public boolean isFrameworkDetails() { return mFrameworkDetails; }
    
    public ExceptionInfo[] getExceptionInfos() { return mExceptionInfos; }
    public MessageType getMessageType() { return mMessageType; }
    public boolean isSuccess() { return mSuccess; }
    public String getTaskId() { return mTaskId; }
    public MsgLocInfo[] getTaskStatusMsgs() { return mTaskStatusMsgs; }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskElement#accept(com.sun.jbi.component.toolkit.util.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }

}
