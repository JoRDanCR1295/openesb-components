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
 * @(#)ComponentTaskResultDetails.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.logging.Logger;

/**
 * This class embodies the <code>component-task-result-details</code> element of
 * a JBI management message.

 * @author Sun Microsystems
 *
 */
public class ComponentTaskResultDetails extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(ComponentTaskResultDetails.class.getName());
    /**
     * The element name: <code>component-task-result-details</code>.
     */
    public static final String ELEMENT_NAME = "component-task-result-details";

    /**
     * The task result details as such.
     */
    private TaskResultDetails details;

    /**
     * The empty constructor.
     *
     */
    public ComponentTaskResultDetails() {}

    /**
     * The convenience constructor.
     */
    public ComponentTaskResultDetails(TaskResultDetails details) {
        setDetails(details);
    }

    /**
     * Get a string serialization of the component task result details conveyed
     * by this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this component task result detail.
     */
    protected String buildString() throws JBIMessageException {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">");

        buffer.append(details.getString());

        buffer.append("\t\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">");

        return buffer.toString();
    }

    /**
     * Validate that the task result details have been properly set.

     * @throws JBIMessageException If the task id has not been set.
     */
    public void validate() throws JBIMessageException {
        if (details == null) {
            logger.warning("Details not set in task result details");
        }
    }

    /**
     * Get the (not null) task result details.
     * @return the task result details.
     */
    public TaskResultDetails getDetails() {
        if (details == null) {
            details = new TaskResultDetails();
        }
        return details;
    }

    /**
     * Set the task result details.
     *
     * @param details the task result details.
     * @throws JBIMessageException If the given detail is null.
     */
    public void setDetails(TaskResultDetails details) throws JBIMessageException {
        if (details == null) {
            throw new JBIMessageException("Task result details cannot be null");
        }
        this.details = details;
    }
}
