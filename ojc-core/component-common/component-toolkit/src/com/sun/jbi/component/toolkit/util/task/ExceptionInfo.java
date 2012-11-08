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
 * @(#)ExceptionInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;


/**
 * Exception information used to report exceptions.
 * 
 * @author Kevan Simpson
 */
public class ExceptionInfo implements TaskElement {
    /** The element name: <code>exception-info</code>. */
    public static final String ELEMENT_NAME = "exception-info";
    /** The nesting level element name: <code>nesting-level</code>. */
    public static final String NESTING_LEVEL_ELEMENT_NAME = "nesting-level";
    /** The stacktrace element name: <code>stack-trace</code>. */
    public static final String STACK_TRACE_ELEMENT_NAME = "stack-trace";

    /** The read-only throwable nesting level. Defaults to -1 (unset). */
    private int mNestingLevel = -1;
    /** The throwable's message localization info. */
    private MsgLocInfo mMsgLocInfo;
    /** The underlying throwable. */
    private Throwable mException;

    public ExceptionInfo(int nestingLevel, MsgLocInfo info, Throwable exception) {
        mNestingLevel = nestingLevel;
        mMsgLocInfo = info;
        mException = exception;
    }
    
    public ExceptionInfo(MsgLocInfo info, Throwable exception) {
        this(-1, info, exception);
        mNestingLevel = calcNestingLevel(exception);
    }
    
    public Throwable getException() { return mException; }
    public MsgLocInfo getMsgLocInfo() { return mMsgLocInfo; }
    public int getNestingLevel() { return mNestingLevel; }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskElement#accept(com.sun.jbi.component.toolkit.util.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }
    
    protected int calcNestingLevel(Throwable exception) {
        if (getNestingLevel() == -1 && exception != null) {
            // TODO copied from componentsl....is this correct?
            Throwable t = exception.getCause();
            for (mNestingLevel = 0; t != null; mNestingLevel++) {
                t = t.getCause();
            }
        }
        return getNestingLevel();
    }
}
