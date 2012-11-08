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
 * @(#)TaskMessageValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.mgmt.task;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.component.mgmt.task.TaskResultDetails.MessageType;
import com.sun.jbi.crl.util.Util;

/**
 * Traverses task messages and performs validation.
 * 
 * @author Kevan Simpson
 */
public class TaskMessageValidator implements TaskVisitor {
    private static final String MISSING = " is missing!";
    private static final String REQUIRED = " is required!";
    
    private List<String> mMessages = null;
    
    public void validate(TaskElement elem) {
        reset();
        if (elem != null) {
            elem.accept(this);
        }
    }
    
    public boolean hasValidationErrors() {
        return (mMessages != null && !mMessages.isEmpty());
    }
    
    public String[] getValidationErrors() {
        if (mMessages == null) return new String[0];
        
        String[] msgs = new String[mMessages.size()];
        mMessages.toArray(msgs);
        return msgs;
    }
    
    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.ExceptionInfo) */
    public void visit(ExceptionInfo info) {
        if (info == null) return;
        
        if (info.getException() == null) {
            add(ExceptionInfo.STACK_TRACE_ELEMENT_NAME, " in ",
                ExceptionInfo.ELEMENT_NAME, REQUIRED);
        }
        if (info.getMsgLocInfo() == null) {
            add(MsgLocInfo.ELEMENT_NAME, " in ", 
                ExceptionInfo.ELEMENT_NAME, REQUIRED);
        }
        else {
            info.getMsgLocInfo().accept(this);
        }
    }

    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.JbiTask) */
    public void visit(JbiTask task) {
        if (task == null) return;
        
        String version = task.getVersion();
        if (Util.isEmpty(version)) {
            add(JbiTask.VERSION_ATTRIBUTE_NAME, " attribute in ", 
                JbiTask.ELEMENT_NAME, REQUIRED);
        }
        else {
            try {
                double d = Double.parseDouble(version);
                if (d <= 0d) {
                    add(JbiTask.VERSION_ATTRIBUTE_NAME, " attribute in ",
                        JbiTask.ELEMENT_NAME, " cannot be zero or negative: ",
                        version);
                }
            } 
            catch (NumberFormatException nbe) {
                add(JbiTask.VERSION_ATTRIBUTE_NAME, " attribute in ", 
                    JbiTask.ELEMENT_NAME, " must be a decimal value: ", version);
            }

            if (!version.equals(new Double(version).toString())) {
                add(JbiTask.VERSION_ATTRIBUTE_NAME, " attribute in ", 
                    JbiTask.ELEMENT_NAME, " must be a valid decimal representation: ", 
                    version);
            }
        }
        
        if (task.getJbiTaskResult() == null) {
            add(JbiTaskResult.ELEMENT_NAME, " in ", JbiTask.ELEMENT_NAME, REQUIRED);
        }
        else {
            task.getJbiTaskResult().accept(this);
        }
    }

    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.JbiTaskResult) */
    public void visit(JbiTaskResult result) {
        if (result == null) return;
        
        if (result.getFrmwkTaskResult() == null) {
            add(TaskResult.FRMWK_ELEMENT_NAME, " in ", 
                JbiTaskResult.ELEMENT_NAME, REQUIRED);
        }
        else {
            result.getFrmwkTaskResult().accept(this);
        }
        
        TaskResult[] compTaskResults = result.getComponentTaskResults();
        if (compTaskResults != null) {
            for (TaskResult tr : compTaskResults) {
                if (tr == null) {
                    add(TaskResult.COMPONENT_ELEMENT_NAME, " in ",
                        JbiTaskResult.ELEMENT_NAME, MISSING);
                }
                else {
                    if (tr.isFrameworkResult()) {
                        add(JbiTaskResult.ELEMENT_NAME, " cannot contain multiple ",
                            TaskResult.FRMWK_ELEMENT_NAME, " elements!");
                    }
                    tr.accept(this);
                }
            }
        }
    }

    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.MsgLocInfo) */
    public void visit(MsgLocInfo info) {
        if (info == null) return;
        
        if (info.getToken() == null) {
            add(MsgLocInfo.LOC_TOKEN_ELEMENT_NAME, " in ", MsgLocInfo.ELEMENT_NAME, REQUIRED);
        }
        if (info.getMessage() == null) {
            add(MsgLocInfo.LOC_MESSAGE_ELEMENT_NAME, " in ", MsgLocInfo.ELEMENT_NAME, REQUIRED);
        }
        else {
            int paramCount = -1;
            try { 
                MessageFormat mf = new MessageFormat(info.getMessage());
                paramCount = mf.getFormatsByArgumentIndex().length;
            }
            catch (IllegalArgumentException iae) {
                add(MsgLocInfo.LOC_MESSAGE_ELEMENT_NAME, 
                    " is an invalid pattern!  Make sure all special characters (e.g. ',\",{) are properly escaped!");
            }
            
            Object[] params = info.getParams();
            if (params == null) {
                if (paramCount > 0) {
                    add(MsgLocInfo.LOC_PARAM_ELEMENT_NAME, REQUIRED, 
                        " Expected ", String.valueOf(paramCount), " arguments!");
                }
            }
            else if (paramCount != params.length) {
                add("Wrong number of arguments! Expected ", String.valueOf(paramCount),
                    ", actual ", String.valueOf(String.valueOf(params.length)));
            }
        }
    }

    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.TaskResult) */
    public void visit(TaskResult result) {
        if (result == null) return;
        
        if (result.isFrameworkResult()) {
            if (!Util.isEmpty(result.getComponentName())) {
                add(TaskResult.FRMWK_ELEMENT_NAME, " cannot contain a ",
                    TaskResult.COMPONENT_NAME_ELEMENT_NAME, " element!");
            }
            
            if (result.getDetails() == null) {
                add(TaskResultDetails.FRMWK_ELEMENT_NAME, " in ",
                    TaskResult.FRMWK_ELEMENT_NAME, REQUIRED);
            }
            else {
                if (!result.getDetails().isFrameworkDetails()) {
                    add(TaskResult.FRMWK_ELEMENT_NAME, " cannot contain ",
                        TaskResultDetails.COMPONENT_ELEMENT_NAME, " elements!");
                }
                result.getDetails().accept(this);
            }
        }
        else {
            if (Util.isEmpty(result.getComponentName())) {
                add(TaskResult.COMPONENT_NAME_ELEMENT_NAME, " in ",
                    TaskResult.COMPONENT_ELEMENT_NAME, REQUIRED);
            }
            
            if (result.getDetails() == null) {
                add(TaskResultDetails.COMPONENT_ELEMENT_NAME, " in ",
                    TaskResult.COMPONENT_ELEMENT_NAME, REQUIRED);
            }
            else {
                if (result.getDetails().isFrameworkDetails()) {
                    add(TaskResult.COMPONENT_ELEMENT_NAME, " cannot contain ",
                        TaskResultDetails.FRMWK_ELEMENT_NAME, " elements!");
                }
                result.getDetails().accept(this);
            }
            
            if (result.isCauseFramework() != null) {
                add(TaskResult.COMPONENT_ELEMENT_NAME, " cannot contain an ",
                    TaskResult.IS_CAUSE_FRAMEWORK_ELEMENT_NAME, " element!");
            }
        }
    }

    /** @see com.sun.jbi.component.mgmt.task.TaskVisitor#visit(com.sun.jbi.component.mgmt.task.TaskResultDetails) */
    public void visit(TaskResultDetails details) {
        if (details == null) return;
        
        if (Util.isEmpty(details.getTaskId())) {
            add(TaskResultDetails.TASK_ID_ELEMENT_NAME, " in ",
                TaskResultDetails.ELEMENT_NAME, REQUIRED);
        }
        
        MessageType msgType = details.getMessageType();
        if (details.isSuccess()) {
            if (msgType == MessageType.ERROR) {
                add("Successful tasks cannot report ERROR status!");
            }
        }
        else if (msgType != MessageType.WARNING || msgType != MessageType.ERROR) {
            add("Failed tasks must report WARNING or ERROR status!");
        }
        
        MsgLocInfo[] msgs = details.getTaskStatusMsgs();
        if (msgs != null && msgs.length > 0) {
            for (MsgLocInfo mli : msgs) {
                if (mli == null) {
                    add("NULL ", TaskResultDetails.TASK_STATUS_MSG_ELEMENT_NAME, 
                        " in ", TaskResultDetails.ELEMENT_NAME);
                }
                else {
                    mli.accept(this);
                }
            }
        }
        
        ExceptionInfo[] infos = details.getExceptionInfos();
        if (infos != null && infos.length > 0) {
            for (ExceptionInfo ei : infos) {
                if (ei == null) {
                    add("NULL ", ExceptionInfo.ELEMENT_NAME, " in ",
                        TaskResultDetails.ELEMENT_NAME);
                }
                else {
                    ei.accept(this);
                }
            }
        }
        
        if (details.isFrameworkDetails() && Util.isEmpty(details.getLocale())) {
            add(TaskResultDetails.LOCALE_ELEMENT_NAME, " in ",
                TaskResultDetails.FRMWK_ELEMENT_NAME, REQUIRED);
        }
        else if (!details.isFrameworkDetails() && !Util.isEmpty(details.getLocale())) {
            add(TaskResultDetails.COMPONENT_ELEMENT_NAME, " cannot contain a ",
                TaskResultDetails.LOCALE_ELEMENT_NAME, " element!");
        }
    }

    private void reset() {
        mMessages = new ArrayList<String>();
    }
    private void add(String... msg) {
        if (msg != null) {
            if (msg.length == 1) {
                mMessages.add(msg[0]);
            }
            else if (msg.length > 1) {
                StringBuffer buff = new StringBuffer();
                for (String str : msg) {
                    buff.append(str);
                }
                mMessages.add(buff.toString());
            }
        }
    }
}
