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
 * @(#)MessageLocalizationInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.text.MessageFormat;
import java.util.logging.Logger;



/**
 * This class embodies the <code>msg-loc-info</code> element of a
 * JBI management message.

 * @author Sun Microsystems
 *
 */
public class MessageLocalizationInfo extends JBIMessageElement {
    private static final Logger logger = Logger.getLogger(MessageLocalizationInfo.class.getName());
    /**
     * The element name: <code>msg-loc-info</code>.
     */
    public static final String ELEMENT_NAME = "msg-loc-info";

    /**
     * The localization token element name: <code>loc-token</code>.
     */
    public static final String LOCALIZATION_TOKEN_NAME = "loc-token";

    /**
     * The localization message element name: <code>loc-message</code>.
     */
    public static final String LOCALIZATION_MESSAGE_NAME = "loc-message";

    /**
     * The localization parameter element name: <code>loc-param</code>.
     */
    public static final String LOCALIZATION_PARAMETER_NAME = "loc-param";

    /**
     * The message key for looking up localized text for the message.
     */
    private String token;

    /**
     * The localized message. Messages use <code>java.text.MessageFormat</code> patterns.
     */
    private String message;

    /**
     * Zero or more parameters for the message.
     */
    private Object[] parameters;

    /**
     * The empty constructor.
     */
    public MessageLocalizationInfo() {}

    /**
     * The convenience constructor.
     *
     * @param tkn The message key for looking up localized text for the message.
     * @param msg The localized message.
     * @param params Zero or more parameters for the message.
     *
     * @throws JBIMessageException If any of the token, message or parameters is
     *                             null or malformed.
     */
    public MessageLocalizationInfo(String tkn, String msg, Object[] params) throws JBIMessageException {
        setToken(tkn);
        setMessage(msg);
        setParameters(params);
    }

    /**
     * Get a string serialization of the message localization info conveyed by
     * this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this message localization info.
     */
    protected String buildString() {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t\t\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t\t<");
        buffer.append(LOCALIZATION_TOKEN_NAME);
        buffer.append(">");
        buffer.append(token);
        buffer.append("</");
        buffer.append(LOCALIZATION_TOKEN_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t\t<");
        buffer.append(LOCALIZATION_MESSAGE_NAME);
        buffer.append(">");
        buffer.append(message);
        buffer.append("</");
        buffer.append(LOCALIZATION_MESSAGE_NAME);
        buffer.append(">\n");

        if (parameters != null) {
            for (int i = 0; i < parameters.length; i++) {
                String parameter = "null";
                if (parameters[i] != null) {
                    parameter = parameters[i].toString();
                }
                buffer.append("\t\t\t\t\t\t<");
                buffer.append(LOCALIZATION_PARAMETER_NAME);
                buffer.append(">");
                buffer.append(parameter);
                buffer.append("</");
                buffer.append(LOCALIZATION_PARAMETER_NAME);
                buffer.append(">\n");
            }
        }

        buffer.append("\t\t\t\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }
    /**
     * Validate that the message localization info has been properly set.

     * @throws JBIMessageException If the token or message have not been set,
     *                             the message format is invalid or the
     *                             parameter count does not match the message
     *                             format.
     */
    /* (non-Javadoc)
     * @see com.sun.jbi.management.message.JBIMessageElement#validate()
     */
    public void validate() throws JBIMessageException {
       if (token == null) {
    	   logger.warning("Token not set in message localization");
        }

        if (message == null) {
            logger.warning("Message not set in message localization");
        }

        int actualParameterCount = 0;
        if (parameters != null) {
            actualParameterCount = parameters.length;
        }
        MessageFormat messageFormat = null;
        try {
            messageFormat = new MessageFormat(message);
        } catch (Exception e) {
            logger.warning("Error in pattern '" + message + "': " + e);
        }
        int expectedParameterCount = -1;
        try {
            expectedParameterCount = messageFormat.getFormatsByArgumentIndex().length;
        }
        catch (Exception e) {
            logger.warning("validate() failed to tally message formats: " + e);
        }
        if (expectedParameterCount != actualParameterCount &&
            expectedParameterCount > -1) {
            logger.warning("Parameter count mismatch, expected " + 
                                          expectedParameterCount + ", actual " + 
                                          actualParameterCount);
        }
    }

    /**
     * Get the localized message text.
     * @return
     */
    public String getMessage() {
        return message;
    }

    /**
     * Set the localized message text.
     *
     * @param msg The message text.
     * @throws JBIMessageException If the given message text is null.
     */
    public void setMessage(String msg) throws JBIMessageException {
        if (msg == null) {
            throw new JBIMessageException("Localized message text cannot be null");
        }
        this.message = msg;
    }

    /**
     * Get the message parameters.
     *
     * @return The message parameters.
     */
    public Object[] getParameters() {
        return copy(parameters);
    }

    /**
     * Set the message parameters.
     *
     * @param params The message parameters.
     */
    public void setParameters(Object[] params) {
        this.parameters = copy(params);
    }

    /** Utility method to copy array and avoid exposing internal representation. */
    private Object[] copy(Object[] src) {
        if (src == null) return null;
        else {
            int len = src.length;
            Object[] dest = new Object[len];
            System.arraycopy(src, 0, dest, 0, len);
            return dest;
        }
    }
    
    /**
     * Get the message localization token.
     * @return The message localization token.
     */
    public String getToken() {
        return token;
    }

    /**
     * Set the localized message token.
     *
     * @param tkn The key to look up the localized message.
     * @throws JBIMessageException If the given token is null.
     */
    public void setToken(String tkn) throws JBIMessageException {
        if (tkn == null) {
            throw new JBIMessageException("Localization token cannot be null");
        }
        this.token = tkn;
    }
}
