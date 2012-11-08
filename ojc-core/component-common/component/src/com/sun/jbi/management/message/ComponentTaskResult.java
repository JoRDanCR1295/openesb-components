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
 * @(#)ComponentTaskResult.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.logging.Logger;

/**
 * This class embodies the <code>component-task-result</code> element of a
 * JBI management message.
 *
 * @author Sun Microsystems
 *
 */
public class ComponentTaskResult extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(ComponentTaskResult.class.getName());
    /**
     * The element name: <code>component-task-result</code>.
     */
    public static final String ELEMENT_NAME = "component-task-result";
    /**
     * The component name element name: <code>component-name</code>.
     */
    public static final String COMPONENT_NAME_NAME = "component-name";

    /**
     * The component's name.
     */
    private String componentName;

    /**
     * The component task result details.
     */
    private ComponentTaskResultDetails details;

    public ComponentTaskResult() {}

    public ComponentTaskResult(String componentName, ComponentTaskResultDetails details) {
        setComponentName(componentName);
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

        buffer.append("\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t<");
        buffer.append(COMPONENT_NAME_NAME);
        buffer.append(">");
        buffer.append(componentName);
        buffer.append("</");
        buffer.append(COMPONENT_NAME_NAME);
        buffer.append(">\n");

        buffer.append(details.getString());

        buffer.append("\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the task result details have been properly set.
     * @throws JBIMessageException If the component name of the details
     *                             have not been set.
     */
    public void validate() throws JBIMessageException {
        if (componentName == null) {
            logger.warning("Component name not set in component task result");
        }
        if (details == null) {
            logger.warning("Details not set in component task result");
        }
    }

    /**
     * Get the component's name.
     * @return the component's name.
     */
    public String getComponentName() {
        return componentName;
    }

    /**
     * Set the component's name.
     *
     * @param componentName The name to be set.
     * @throws JBIMessageException If the given name is null.
     */
    public void setComponentName(String componentName) throws JBIMessageException {
        if (componentName == null) {
            throw new JBIMessageException("Component name cannot be null");
        }
        this.componentName = componentName;
    }

    /**
     * Get the (not null) component task result details.
     * @return the component task result details.
     */
    public ComponentTaskResultDetails getDetails() {
        if (details == null) {
            details = new ComponentTaskResultDetails();
        }
        return details;
    }

    /**
     * Set the component task result details.
     * @param details the component task result details.
     * @throws JBIMessageException if the given detail is null.
     */
    public void setDetails(ComponentTaskResultDetails details) throws JBIMessageException {
        if (details == null) {
            throw new JBIMessageException("Component task result details cannot be null");
        }
        this.details = details;
    }
}
