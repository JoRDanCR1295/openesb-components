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
 * @(#)JbiTask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.mgmt.task;

/**
 * The document element, which, along with the document’s namespace, identifies 
 * this as a JBI management task result/status report.
 * 
 * @author Kevan Simpson
 */
public class JbiTask implements TaskElement {
    /** The element name: <code>jbi-task</code>. */
    public static final String ELEMENT_NAME = "jbi-task";
    /** The version attribute name: <code>version</code>. */
    public static final String VERSION_ATTRIBUTE_NAME = "version";
    /** The namespace URI dictated by the JBI spec. */
    public static final String JBI_TASK_NAMESPACE_URI = "http://java.sun.com/xml/ns/jbi/management-message";

    private String mVersion = "1.0";
    private JbiTaskResult mJbiTaskResult = null;
    
    public JbiTask(JbiTaskResult taskResult) {
        mJbiTaskResult = taskResult;
    }
    public JbiTask(String version, JbiTaskResult taskResult) {
        this(taskResult);
        mVersion = version;
    }
    
    public JbiTaskResult getJbiTaskResult() { return mJbiTaskResult; }
    public String getVersion() { return mVersion; }
    
    /** @see com.sun.jbi.component.mgmt.task.TaskElement#accept(com.sun.jbi.component.mgmt.task.TaskVisitor) */
    public void accept(TaskVisitor visitor) {
        visitor.visit(this);
    }
}
