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
 * @(#)FrameworkTaskResultDetails.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.Locale;
import java.util.logging.Logger;

/**
 * This class embodies the <code>frmwk-task-result-details</code> element of a
 * JBI management message.

 * @author Sun Microsystems
 *
 */
public class FrameworkTaskResultDetails extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(FrameworkTaskResultDetails.class.getName());
    /**
     * The element name: <code>frmwk-task-result-details</code>.
     */
    public static final String ELEMENT_NAME = "frmwk-task-result-details";

    /**
     * The element name for the result locale: <code>locale</code>.
     */
    public static final String LOCALE_NAME = "locale";

    /**
     * The task result details sub-element.
     */
    private TaskResultDetails details;

    /**
     * The message locale. Defaults to the current <code>Locale.getDefault()</code>.
     */
    private Locale locale = Locale.getDefault();

    /**
     * Get a string serialization of the framework task result message conveyed
     * by this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing these framework task result details.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append(getDetails().getString());

        buffer.append("\t\t\t\t<");
        buffer.append(LOCALE_NAME);
        buffer.append(">");
        buffer.append(getLocale().toString());
        buffer.append("</");
        buffer.append(LOCALE_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the task result details have been properly set.
     */
    public void validate() {}

    /**
     * Get the not null task result details sub-element.
     *
     * @return The task result details sub-element associated with this instance.
     */
    public TaskResultDetails getDetails() {
        if (details == null) {
            details = new TaskResultDetails();
        }
        return details;
    }

    /**
     * Explicitly set the task result details associated with this instance.
     *
     * @param details The new task result details.
     * @throws JBIMessageException If the new task result detail is null.
     */
    public void setDetails(TaskResultDetails details) throws JBIMessageException {
        if (details == null) {
            throw new JBIMessageException("Task result details cannot be null");
        }
        this.details = details;
    }

    /**
     * Get this instance locale.
     *
     * @return The instance message locale.
     */
    public Locale getLocale() {
        return locale;
    }

    /**
     * Set this instance message locale.
     *
     * @param locale The new message locale.
     * @throws JBIMessageException If the new locale is null.
     */
    public void setLocale(Locale locale) throws JBIMessageException {
        if (locale == null) {
            throw new JBIMessageException("Locale cannot be null");
        }
        this.locale = locale;
    }
}
