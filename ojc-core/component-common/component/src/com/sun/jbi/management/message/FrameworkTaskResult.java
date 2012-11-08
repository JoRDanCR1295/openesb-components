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
 * @(#)FrameworkTaskResult.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.logging.Logger;

/**
 * This class embodies the <code>frmwk-task-result</code> element of a JBI
 * management message.

 * @author Sun Microsystems
 *
 */
public class FrameworkTaskResult extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(FrameworkTaskResult.class.getName());
    /**
     * The element name: <code>frmwk-task-result</code>.
     */
    public static final String ELEMENT_NAME = "frmwk-task-result";

    /**
     * The element name for framework-caused results: <code>is-cause-framework</code>.
     */
    public static final String CAUSE_FRAMEWORK_NAME = "is-cause-framework";

    /**
     * The framework task result details sub-element.
     */
    private FrameworkTaskResultDetails details;

    /**
     * Flag indicating whether the result was caused by the JBI framework.
     */
    private boolean causeFramework = false;

    /**
     * Get a string serialization of the framework task result message conveyed
     * by this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this framework task result.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append(getDetails().getString());

        buffer.append("\t\t\t<");
        buffer.append(CAUSE_FRAMEWORK_NAME);
        buffer.append(">");
        buffer.append(causeFramework ? "YES" : "NO");
        buffer.append("</");
        buffer.append(CAUSE_FRAMEWORK_NAME);
        buffer.append(">\n");

        buffer.append("\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the task result details have been properly set.
     */
    public void validate() {}

    /**
     * Determine whether this task result was caused by the JBI framework.
     *
     * @return <core>true</code> if this task result was caused by the JBI
     *         framework itself, <code>false</code> otherwise.
     */
    public boolean isCauseFramework() {
        return causeFramework;
    }

    /**
     * State whether this task result was caused by the JBI framework.
     *
     * @param causeFramework Whether this task result was caused by the JBI
     *                       framework itself.
     */
    public void setCauseFramework(boolean causeFramework) {
        this.causeFramework = causeFramework;
    }

    /**
     * Get the (not null) framework task details.
     *
     * @return The framework task details associated with this instance.
     */
    public FrameworkTaskResultDetails getDetails() {
        if (details == null) {
            details = new FrameworkTaskResultDetails();
        }
        return details;
    }

    /**
     * Set the framework task details for this instance.
     *
     * @param details The framework task details associated with this instance.
     * @throws JBIMessageException If the given details are null.
     */
    public void setDetails(FrameworkTaskResultDetails details) throws JBIMessageException {
        if (details == null) {
            throw new JBIMessageException("Framework task result details cannot be null");
        }
        this.details = details;
    }
}
