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
 * @(#)TaskXmlWriter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;

import com.sun.jbi.common.util.Util;

/**
 * Generates the XML content of JBI task messages.
 * 
 * @author Kevan Simpson
 */
public class TaskXmlWriter implements TaskVisitor {
    private StringBuffer mXml = new StringBuffer();
    
    public static String toXml(TaskElement elem) {
        // TODO do we overload this method to provide formatted version of xml?
        if (elem != null) {
            TaskXmlWriter writer = new TaskXmlWriter();
            elem.accept(writer);
            return writer.getXml();
        }
        return null;
    }
    
    public String getXml() {
        return mXml.toString();
    }
    
    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.ExceptionInfo) */
    public void visit(ExceptionInfo info) {
        mXml.append("<").append(ExceptionInfo.ELEMENT_NAME)
            .append("><").append(ExceptionInfo.NESTING_LEVEL_ELEMENT_NAME)
            .append(">").append(info.getNestingLevel()).append("</")
            .append(ExceptionInfo.NESTING_LEVEL_ELEMENT_NAME).append(">");
        if (info.getMsgLocInfo() != null) {
            info.getMsgLocInfo().accept(this);
        }
        mXml.append("<").append(ExceptionInfo.STACK_TRACE_ELEMENT_NAME).append(">");
        // TODO should we be adding all chained exceptions?
        for (Throwable t = info.getException(); t != null; t = t.getCause()) {
            mXml.append('\n').append(Util.escape(Util.toString(info.getException())));
        }
        mXml.append("</").append(ExceptionInfo.STACK_TRACE_ELEMENT_NAME).append("></")
            .append(ExceptionInfo.ELEMENT_NAME).append(">");
    }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.JbiTask) */
    public void visit(JbiTask task) {
        /*
         * TODO Generate a properly localized encoding. The spec does not
         *       address this issue so we're defaulting to UTF-8 for now.
         */
        mXml.append("<?xml version='1.0' encoding='UTF-8' standalone='yes'?>")
            .append("<").append(JbiTask.ELEMENT_NAME).append(" version='")
            .append(task.getVersion()).append("' xmlns='")
            .append(JbiTask.JBI_TASK_NAMESPACE_URI).append("'>");
        if (task.getJbiTaskResult() != null) {
            task.getJbiTaskResult().accept(this);
        }
        mXml.append("</").append(JbiTask.ELEMENT_NAME).append(">");
    }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.JbiTaskResult) */
    public void visit(JbiTaskResult result) {
        mXml.append("<").append(JbiTaskResult.ELEMENT_NAME).append(">");
        if (result.getFrmwkTaskResult() != null) {
            result.getFrmwkTaskResult().accept(this);
        }
        TaskResult[] compResults = result.getComponentTaskResults();
        if (compResults != null) {
            for (TaskResult tr : compResults) {
                if (tr != null) tr.accept(this);
            }
        }
        mXml.append("</").append(JbiTaskResult.ELEMENT_NAME).append(">");
    }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.MsgLocInfo) */
    public void visit(MsgLocInfo info) {
        mXml.append("<").append(MsgLocInfo.ELEMENT_NAME).append("><")
            .append(MsgLocInfo.LOC_TOKEN_ELEMENT_NAME).append(">")
            .append(info.getToken()).append("</")
            .append(MsgLocInfo.LOC_TOKEN_ELEMENT_NAME).append("><")
            .append(MsgLocInfo.LOC_MESSAGE_ELEMENT_NAME).append(">")
            .append(info.getMessage()).append("</")
            .append(MsgLocInfo.LOC_MESSAGE_ELEMENT_NAME).append(">");
        Object[] params = info.getParams();
        if (params != null) {
            for (Object p : params) {
                // null param will show up as "null" via String.valueOf()
                mXml.append("<").append(MsgLocInfo.LOC_PARAM_ELEMENT_NAME)
                    .append(">").append(String.valueOf(p)).append("</")
                    .append(MsgLocInfo.LOC_PARAM_ELEMENT_NAME).append(">");
            }
        }
        mXml.append("</").append(MsgLocInfo.ELEMENT_NAME).append(">");
    }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.TaskResult) */
    public void visit(TaskResult result) {
        if (result.isFrameworkResult()) {
            mXml.append("<").append(TaskResult.FRMWK_ELEMENT_NAME).append(">");
            if (result.getDetails() != null) {
                result.getDetails().accept(this);
            }
            if (result.isCauseFramework() != null) {    // null if unset
                String causeFlag = 
                        (result.isCauseFramework().booleanValue()) ? "YES" : "NO";
                mXml.append("<").append(TaskResult.IS_CAUSE_FRAMEWORK_ELEMENT_NAME)
                    .append(">").append(causeFlag).append("</")
                    .append(TaskResult.IS_CAUSE_FRAMEWORK_ELEMENT_NAME).append(">");
            }
            mXml.append("</").append(TaskResult.FRMWK_ELEMENT_NAME).append(">");
        }
        else {
            mXml.append("<").append(TaskResult.COMPONENT_ELEMENT_NAME).append("><")
                .append(TaskResult.COMPONENT_NAME_ELEMENT_NAME).append(">")
                .append(result.getComponentName()).append("</")
                .append(TaskResult.COMPONENT_NAME_ELEMENT_NAME).append(">");
            if (result.getDetails() != null) {
                result.getDetails().accept(this);
            }
            mXml.append("</").append(TaskResult.COMPONENT_ELEMENT_NAME).append(">");
        }
    }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskVisitor#visit(com.sun.jbi.component.toolkit.util.task.TaskResultDetails) */
    public void visit(TaskResultDetails details) {
        String results = (details.isSuccess()) ? "SUCCESS" : "FAILED";
        if (details.isFrameworkDetails()) {
            mXml.append("<").append(TaskResultDetails.FRMWK_ELEMENT_NAME);
        }
        else {
            mXml.append("<").append(TaskResultDetails.COMPONENT_ELEMENT_NAME);
        }
        // closing parent element tag here
        mXml.append("><").append(TaskResultDetails.ELEMENT_NAME).append("><")
            .append(TaskResultDetails.TASK_ID_ELEMENT_NAME).append(">")
            .append(details.getTaskId()).append("</")
            .append(TaskResultDetails.TASK_ID_ELEMENT_NAME).append("><")
            .append(TaskResultDetails.TASK_RESULT_ELEMENT_NAME).append(">")
            .append(results).append("</")
            .append(TaskResultDetails.TASK_RESULT_ELEMENT_NAME).append(">");
        if (details.getMessageType() != null) {
            mXml.append("<").append(TaskResultDetails.MESSAGE_TYPE_ELEMENT_NAME)
                .append(">").append(details.getMessageType()).append("</")
                .append(TaskResultDetails.MESSAGE_TYPE_ELEMENT_NAME).append(">");
        }
        MsgLocInfo[] infos = details.getTaskStatusMsgs();
        if (infos != null && infos.length > 0) {
            for (MsgLocInfo loc : infos) {
                if (loc != null) {
                    mXml.append("<")
                        .append(TaskResultDetails.TASK_STATUS_MSG_ELEMENT_NAME)
                        .append(">");
                    loc.accept(this);
                    mXml.append("</")
                        .append(TaskResultDetails.TASK_STATUS_MSG_ELEMENT_NAME)
                        .append(">");
                }
            }
        }
        ExceptionInfo[] errors = details.getExceptionInfos();
        if (errors != null) {
            for (ExceptionInfo ei : errors) {
                if (ei != null) ei.accept(this);
            }
        }
        mXml.append("</").append(TaskResultDetails.ELEMENT_NAME).append(">");
        
        if (details.isFrameworkDetails()) {
            mXml.append("<").append(TaskResultDetails.LOCALE_ELEMENT_NAME).append(">")
                .append(details.getLocale()).append("</")
                .append(TaskResultDetails.LOCALE_ELEMENT_NAME).append("></")
                .append(TaskResultDetails.FRMWK_ELEMENT_NAME).append(">");
        }
        else {
            mXml.append("</").append(TaskResultDetails.COMPONENT_ELEMENT_NAME)
                .append(">");
        }
    }
}
