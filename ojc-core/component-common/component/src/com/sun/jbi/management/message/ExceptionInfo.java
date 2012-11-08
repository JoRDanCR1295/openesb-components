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

package com.sun.jbi.management.message;

import java.util.logging.Logger;

import com.sun.jbi.util.JBIUtils;

// TODO The spec doesn't specifiy the base to be used in nesting. Zero is assumed.

/**
 * This class embodies the <code>throwable-info</code> element of a
 * JBI management message.

 * @author Sun Microsystems
 *
 */
public class ExceptionInfo extends JBIMessageElement {
    private static final Logger logger = Logger.getLogger(ExceptionInfo.class.getName());
    /**
     * The element name: <code>msg-loc-info</code>.
     */
    public static final String ELEMENT_NAME = "exception-info";

    /**
     * The element name: <code>msg-loc-info</code>.
     */
    public static final String NESTING_LEVEL_NAME = "nesting-level";

    /**
     * The element name: <code>msg-loc-info</code>.
     */
    public static final String STACK_TRACE_NAME = "stack-trace";

    /**
     * The underlying throwable.
     */
    private Throwable throwable;

    /**
     * The read-only throwable nesting level. Defaults to -1 (unset).
     */
    private int nestingLevel = -1;

    /**
     * The throwable's message localization info.
     */
    private MessageLocalizationInfo messageLocalization;

    /**
     * The empty constructor.
     */
    public ExceptionInfo(){}

    /**
     * The convenience constructor.
     *
     * @param throwable The unerlying throwable.
     * @param level The throwable nesting level.
     * @throws JBIMessageException If the given throwable is null or the nesting
     *                             level is invalid.
     */
    public ExceptionInfo(Throwable throwable, MessageLocalizationInfo messageLocalization) throws JBIMessageException {
        setException(throwable);
        setMessageLocalization(messageLocalization);
    }

    /**
     * Get a string serialization of the throwable info conveyed by
     * this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this throwable info.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t\t\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t\t<");
        buffer.append(NESTING_LEVEL_NAME);
        buffer.append(">");
        buffer.append(getNestingLevel());
        buffer.append("</");
        buffer.append(NESTING_LEVEL_NAME);
        buffer.append(">\n");

        buffer.append("\t");
        buffer.append(messageLocalization.getString());

        buffer.append("\t\t\t\t\t\t<");
        buffer.append(STACK_TRACE_NAME);
        buffer.append(">");
        for (Throwable t = throwable; t != null; t = t.getCause()) {
            buffer.append('\n');
            buffer.append(JBIUtils.escape(JBIUtils.stringStackTrace(t)));
        }
        buffer.append("</");
        buffer.append(STACK_TRACE_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the message localization info has been properly set.

     * @throws JBIMessageException If the throwable or message localization have
     *                             not been set.
     */
    public void validate() throws JBIMessageException {
        if (throwable == null) {
            logger.warning("Exception not set");
        }

        if (messageLocalization == null) {
            logger.warning("Missing message localization");
        }
    }

    /**
     * Get this element's underlying throwable.
     * @return
     */
    public Throwable getException() {
        return throwable;
    }

    /**
     * Set the underlying throwable.
     *
     * @param throwable The underlying throwable.
     * @throws JBIMessageException if the given throwable is null.
     */
    public void setException(Throwable throwable) throws JBIMessageException {
        if (throwable == null) {
            throw new JBIMessageException("Exception cannot be null");
        }
        this.nestingLevel = -1;
        this.throwable = throwable;
    }

    /**
     * Get the throwable's nesting level.
     * @return the nesting level.
     */
    public int getNestingLevel() {
        if (nestingLevel == -1 && throwable != null) {
            Throwable t = throwable.getCause();
            for (nestingLevel = 0; t != null; nestingLevel++) {
                t = t.getCause();
            }
        }
        return nestingLevel;
    }

    /**
     * Get the message localization info.
     *
     * @return The throwable's message localization info.
     */
    public MessageLocalizationInfo getMessageLocalization() {
        if (messageLocalization == null) {
            messageLocalization = new MessageLocalizationInfo();
        }
        return messageLocalization;
    }

    /**
     * Set the throwable's message localization info.
     *
     * @param messageLocalization the throwable's message localization info.
     */
    public void setMessageLocalization(MessageLocalizationInfo messageLocalization) throws JBIMessageException {
        if (messageLocalization == null) {
            throw new JBIMessageException("Message localization info cannot be null");
        }
        this.messageLocalization = messageLocalization;
    }
}
