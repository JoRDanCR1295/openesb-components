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
 * @(#)MsgLocInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;

/**
 * Message location information used to report a message in the form
 * of a format string and zero or more text parameters.
 * 
 * @author Kevan Simpson
 */
public class MsgLocInfo implements TaskElement {
    /** The element name: <code>msg-loc-info</code>. */
    public static final String ELEMENT_NAME = "msg-loc-info";
    /** The localization token element name: <code>loc-token</code>. */
    public static final String LOC_TOKEN_ELEMENT_NAME = "loc-token";
    /** The localization message element name: <code>loc-message</code>. */
    public static final String LOC_MESSAGE_ELEMENT_NAME = "loc-message";
    /** The localization parameter element name: <code>loc-param</code>. */
    public static final String LOC_PARAM_ELEMENT_NAME = "loc-param";

    private String mToken, mMessage;
    private Object[] mParams;
    
    public MsgLocInfo(String token, String message, Object... params) {
        mToken = token;
        mMessage = message;
        mParams = params;
    }
    
    public String getToken() { return mToken; }
    public String getMessage() { return mMessage; }
    public Object[] getParams() { return mParams; }

    /** @see com.sun.jbi.component.toolkit.util.task.TaskElement#accept(com.sun.jbi.component.toolkit.util.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }
}
