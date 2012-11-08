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
 * @(#)TaskResultDetails.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import com.sun.jbi.util.JBIUtils;

// TODO The spec doesn't specify whether any combination of success/failure
//       flags and message types is valid. We assume SUCCESS excludes ERROR
//       and that ERROR requires exception info.
/**
 * This class embodies the <code>task-result-details</code> element of a
 * JBI management message.

 * @author Sun Microsystems
 *
 */
public class TaskResultDetails extends JBIMessageElement {
	private static final Logger logger = Logger.getLogger(TaskResultDetails.class.getName());
	
    /**
     * The element name: <code>task-result-details</code>.
     */
    public static final String ELEMENT_NAME = "task-result-details";

    /**
     * The task id sub-element name: <code>task-id</code>/.
     */
    public static final String TASK_ID_NAME = "task-id";

    /**
     * The task result sub-element name: <code>task-result</code>/.
     */
    public static final String TASK_RESULT_NAME = "task-result";

    /**
     * The message type sub-element name: <code>message-type</code>/.
     */
    public static final String MESSAGE_TYPE_NAME = "message-type";

    /**
     * The status message sub-element name: <code>task-status-msg</code>/.
     */
    public static final String TASK_STATUS_MESSAGE_NAME = "task-status-msg";

    /**
     * The task id.
     */
    private String taskId;

    /**
     * The success flag. Defaults to <code>true</code>.
     */
    private boolean successfulResult = true;

    /**
     * The message type. Defaults to <code>INFO</code>.
     */
    private int messageType = JBITaskMessageBuilder.NONE;

    /**
     * The optional, ordered collection of message localizations.
     */
    private List messages;

    /**
     * The optional, ordered collection of exceptions represented by this message.
     */
    private List exceptions;

    /**
     * Get a string serialization of the task result details conveyed by
     * this instance as per the JBI 1.0 specification.
     *
     * @return The <code>String</code> serialization of the XML element
     *         representing this task result detail.
     */
    protected String buildString() throws JBIMessageException {
        StringBuffer buffer = new StringBuffer();

        buffer.append("\t\t\t\t<");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t<");
        buffer.append(TASK_ID_NAME);
        buffer.append(">");
        buffer.append(taskId);
        buffer.append("</");
        buffer.append(TASK_ID_NAME);
        buffer.append(">\n");

        buffer.append("\t\t\t\t\t<");
        buffer.append(TASK_RESULT_NAME);
        buffer.append(">");
        buffer.append(successfulResult ? "SUCCESS" : "FAILED");
        buffer.append("</");
        buffer.append(TASK_RESULT_NAME);
        buffer.append(">\n");

        if (messageType != JBITaskMessageBuilder.NONE) {
            buffer.append("\t\t\t\t\t<");
            buffer.append(MESSAGE_TYPE_NAME);
            buffer.append(">");
            buffer.append(getMessageTypeString());
            buffer.append("</");
            buffer.append(MESSAGE_TYPE_NAME);
            buffer.append(">\n");
        }

        if (messages != null && messages.size() != 0) {
            buffer.append("\t\t\t\t\t<");
            buffer.append(TASK_STATUS_MESSAGE_NAME);
            buffer.append(">\n");
            Iterator i = messages.iterator();
            while (i.hasNext()) {
                MessageLocalizationInfo messageLocalization = (MessageLocalizationInfo) i.next();
                buffer.append(messageLocalization.getString());
            }
            buffer.append("\t\t\t\t\t</");
            buffer.append(TASK_STATUS_MESSAGE_NAME);
            buffer.append(">\n");
        }

        if (exceptions != null && exceptions.size() != 0) {
            Iterator i = exceptions.iterator();
            while (i.hasNext()) {
                ExceptionInfo exceptionInfo = (ExceptionInfo ) i.next();
                buffer.append(exceptionInfo.getString());
            }
        }

        buffer.append("\t\t\t\t</");
        buffer.append(ELEMENT_NAME);
        buffer.append(">\n");

        return buffer.toString();
    }

    /**
     * Validate that the task result details have been properly set.

     * @throws JBIMessageException If the task id has not been set.
     */
    public void validate() throws JBIMessageException {
        if (taskId == null) {
        	logger.warning("Task id not set in task result details");
        }

        if (successfulResult && messageType == JBITaskMessageBuilder.ERROR) {
        	logger.warning("Inconsistent message type: cannot use error message for successful result");
        }
    }

    /**
     * Yield a displayable representation of the message type.
     *
     * @return The message type string: INFO, WARNING or ERROR
     * @throws IllegalStateException if the type is any other than the above.
     */
    public String getMessageTypeString() {
        switch (messageType) {
            case JBITaskMessageBuilder.NONE: return "NONE";
            case JBITaskMessageBuilder.INFO: return "INFO";
            case JBITaskMessageBuilder.WARNING: return "WARNING";
            case JBITaskMessageBuilder.ERROR: return "ERROR";
            default:
                throw new IllegalStateException("Invalid message type: " + messageType);
        }
    }

    /**
     * Return the list of exceptions
     *
     * @return The possibly empty (but not null) list of exception
     */
    public List getExceptions() {
        if (exceptions == null) {
            exceptions = JBIUtils.createSafeList(ExceptionInfo.class, false, false);
        }
        return exceptions;
    }

    /**
     * Construct a new <code>ExceptionInfo</code> instance and add it to the
     * exception list.
     *
     * @param throwable The exception being formatted.
     * @param level The level of the exception being formatted.
     *
     * @return A new, empty component task result.
     * @throws JBIMessageException if an error occurs during exception info creation.
     */
    public ExceptionInfo newExceptionInfo(Throwable throwable, MessageLocalizationInfo messageLocalization) throws JBIMessageException {
        ExceptionInfo exceptionInfo = new ExceptionInfo(throwable, messageLocalization);
        getExceptions().add(exceptionInfo);
        return exceptionInfo;
    }

    /**
     * Add an existing <code>ExceptionInfo</code> instance the exception list.
     *
     * @param exceptionInfo The existing exception info instance.
     *
     * @throws JBIMessageException If the given instance is null.
     */
    public void addExceptionInfo(ExceptionInfo exceptionInfo) throws JBIMessageException {
        if (exceptionInfo == null) {
            throw new JBIMessageException("Exception info cannot be null");
        }
        getExceptions().add(exceptionInfo);
    }

    /**
     * Remove an exception info from the exception list.
     *
     * @param exceptionInfo The exception info element to be removed.
     * @throws JBIMessageException If the given exception info instance is null,
     *                             the exception list is empty or the given
     *                             instance was not present in the list.
     */
    public void removeExceptionInfo(ExceptionInfo exceptionInfo) throws JBIMessageException {
        if (exceptionInfo == null) {
            throw new JBIMessageException("Cannot remove null exception");
        }

        if (exceptions == null) {
            throw new JBIMessageException("Empty exception list, cannot remove");
        }

        if (!exceptions.remove(exceptionInfo)) {
            throw new JBIMessageException("Exception not in list, cannot remove");
        }
    }

    /**
     * Explicitly set the exception list.
     *
     * @param exceptions The list of exceptions.
     *
     * @throws JBIMessageException If the list is null, contains null elements
     *                             or contains elements of type other than
     *                             <code>ExceptionInfo</code>.
     */
    public void setExceptions(List exceptions) throws JBIMessageException {
        JBIUtils.validateCollection(exceptions, ExceptionInfo.class);
        this.exceptions = exceptions;
    }

    /**
     * Return the list of message localizations.
     *
     * @return The list of message localization info.
     */
    public List getMessages() {
        if (messages == null) {
            messages = JBIUtils.createSafeList(MessageLocalizationInfo.class, false, false);
        }
        return messages;
    }

    /**
     * Create a new message localization info.
     *
     * @param token The key for looking up localized text for the message
     * @param message The message text in <code>java.text.MessageFormat</code> format.
     * @param parameters The message parameters.

     * @return A newly initialized instance of <code>MessageLocalizationInfo</code>.
     */
    public MessageLocalizationInfo newMessageLocalizationInfo(String token, String message, Object[] parameters) throws JBIMessageException {
        MessageLocalizationInfo messageLocalization = new MessageLocalizationInfo(token, message, parameters);
        getMessages().add(messageLocalization);
        return messageLocalization;
    }

    /**
     * Add a previously built instance to the message localization list.
     *
     * @param messageLocalization The existing message localization info.
     * @throws JBIMessageException If the given instance is null.
     */
    public void addMessageLocalizationInfo(MessageLocalizationInfo messageLocalization) throws JBIMessageException {
        if (messageLocalization == null) {
            throw new JBIMessageException("Message localization info cannot be null");
        }
        getMessages().add(messageLocalization);
    }

    /**
     * Remove an instance from the message localization list.
     *
     * @param messageLocalization The message localization info to be removed.
     * @throws JBIMessageException If the govien instance is null, the message
     *                             localization list is empty or the given
     *                             instance was not present in the list.
     */
    public void removeMessageLocalizationInfo(MessageLocalizationInfo messageLocalization) throws JBIMessageException {
        if (messageLocalization == null) {
            throw new JBIMessageException("Cannot remove null exception");
        }

        if (messages == null) {
            throw new JBIMessageException("Empty message localization list, cannot remove");
        }

        if (!messages.remove(messageLocalization)) {
            throw new JBIMessageException("Message localization not in list, cannot remove");
        }
    }

    /**
     * Explicitly set the list of message localizations.
     *
     * @param messages The message localization list to be used.
     * @throws JBIMessageException If the given list is null or contains elements
     *                             of type other than <code>MessageLocalizationInfo</code>.
     */
    public void setMessages(List messages) throws JBIMessageException {
        JBIUtils.validateCollection(messages, MessageLocalizationInfo.class);
        this.messages = messages;
    }

    /**
     * Return the message type.
     *
     * @return The message type.
     */
    public int getMessageType() {
        return messageType;
    }

    /**
     * Set the message type.
     *
     * @param messageType The new message type.
     * @throws JBIMessageException if the given type is not one of INFO,
     *                             WARNING or ERROR.
     */
    public void setMessageType(int messageType) throws JBIMessageException {
        switch (messageType) {
            case JBITaskMessageBuilder.NONE:
            case JBITaskMessageBuilder.INFO:
            case JBITaskMessageBuilder.WARNING:
            case JBITaskMessageBuilder.ERROR:
                this.messageType = messageType;
                break;
            default:
                throw new JBIMessageException("Invalid message type: " + messageType);
        }
    }

    /**
     * Inquiry whether the task result embodied by this detail was succesful or not.
     * @return whether the task result embodied by this detail was succesful or not.
     */
    public boolean isSuccessfulResult() {
        return successfulResult;
    }

    /**
     * State whether the task result embodied by this detail was succesful or not.
     * @param successfulResult whether the task result embodied by this detail was succesful or not.
     */
    public void setSuccessfulResult(boolean successfulResult) {
        this.successfulResult = successfulResult;
    }

    /**
     * Get this detail's task id.
     *
     * @return The task id.
     */
    public String getTaskId() {
        return taskId;
    }

    /**
     * Set this detail's task id.
     *
     * @param taskId The task id string.
     * @throws JBIMessageException if the given task id is null.
     */
    public void setTaskId(String taskId) throws JBIMessageException {
        if (taskId == null) {
            throw new JBIMessageException("Task id cannot be null");
        }
        this.taskId = taskId;
    }
}
