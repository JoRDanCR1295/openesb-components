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
 * @(#)JBITask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.logging.Logger;



/**
 * This class represents the top-level <code>jbi-task</code> element of the
 * status/result XML schema defined by the JBI specification as used to
 * convey the result of management tasks.<br/>
 *
 * According to the spec, documents in this XML schema are to be serialized
 * as <code>String</code>s in order to avoid the need for synchronized class
 * definitions. Consistently, classes in this package are not marked as
 * <code>Serializable</code>.
 *
 * @author Sun Microsystems
 *
 */
public class JBITask extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(JBITask.class.getName());
    /**
     * The name of the top element: <code>jbi-task</code>.
     */
    public static final String ELEMENT_NAME = "jbi-task";

    /**
     * The namespace URI dictated by the JBI spec.
     */
    public static final String JBI_TASK_NAMESPACE_URI = "http://java.sun.com/xml/ns/jbi/management-message";

    /*
     * The message schema version. Defaults to the current value: 1.0
     */
    private String version = "1.0";

    /*
     * The task result (<code>jbi-task-result</code>).
     */
    private JBITaskResult taskResult;

    /**
     * Get a string serialization of the status/result message conveyed by
     * this instance as per the JBI 1.0 specification. The current
     * implementation uses a fixed <code>UTF-8</code> XML encoding.
     *
     * @return The <code>String</code> serialization of the XML document
     *         representing this message.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        /*
         * TODO Generate a properly localized encoding. The spec does not
         *       address this issue so we're defaulting to UTF-8 for now.
         */
        buffer.append("<?xml version='1.0' encoding='UTF-8' standalone='yes'?>\n");

        // Root element
        buffer.append("<");
        buffer.append(ELEMENT_NAME);
        buffer.append(" version='");
        buffer.append(version);
        buffer.append("'");
        buffer.append(" xmlns='");
        buffer.append(JBI_TASK_NAMESPACE_URI);
        buffer.append("'");
        buffer.append(">\n");
        buffer.append(getTaskResult().getString());
        buffer.append("</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the task result has been properly set.
=     * @throws JBIMessageException If the task result has not been set.
     */
    public void validate() throws JBIMessageException {
        if (taskResult == null) {
            logger.warning("Task result not set");
        }
    }

    /**
     * Get the not-null task result subelement.

     * @return The <code>TaskResult</code> subelement.
     */
    public JBITaskResult getTaskResult() {
        if (taskResult == null) {
            taskResult = new JBITaskResult();
        }
        return taskResult;
    }

    /**
     * Set the task result to a previously built message value.
     *
     * @param taskResult The new task result
     * @throws JBIMessageException If the new task result is null
     */
    public void setTaskResult(JBITaskResult taskResult) throws JBIMessageException {
        if (taskResult == null) {
            throw new JBIMessageException("Task result cannot be null");
        }
        this.taskResult = taskResult;
    }

    /**
     * Get the message version. This defaults to the current implemented
     * version (1.0).

     * @return The message version.
     */
    public String getVersion() {
        return version;
    }

    /**
     * Set the message version.
     *
     * @param version The new version.
     * @throws JBIMessageException If the new version is null, not a valid
     *                             decimal number or has a zero or negative
     *                             numeric value.
     */
    public void setVersion(String version) throws JBIMessageException {
        if (version == null) {
            throw new JBIMessageException("Version cannot be null");
        }

        try {
            double d = Double.parseDouble(version);
            if (d <= 0d) {
                throw new JBIMessageException("Version cannot be zero nor negative: " + version);
            }
        } catch (NumberFormatException nbe) {
            throw new JBIMessageException("Invalid version decimal number: " + version);
        }

        if (!version.equals(new Double(version).toString())) {
            throw new JBIMessageException("Invalid decimal representation: " + version);
        }

        this.version = version;
    }
}
