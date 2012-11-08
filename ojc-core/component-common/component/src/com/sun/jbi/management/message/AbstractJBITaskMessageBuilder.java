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
 * @(#)AbstractJBITaskMessageBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import javax.jbi.management.DeploymentException;

public abstract class AbstractJBITaskMessageBuilder implements JBITaskMessageBuilder {
    private String componentName;

    public final void setComponentName(String componentName) {
        if (componentName == null) {
            throw new NullPointerException("Component name cannot be null");
        }
        if (componentName.trim().length() == 0) {
            throw new IllegalArgumentException("Component name cannot be blank");
        }
        this.componentName = componentName;
    }

    public String getComponentName() {
        return componentName;
    }

    public final String createSuccessMessage(String taskId) {
        return createComponentMessage(
                taskId, // taskId
                true, // success
                NONE, // messageType
                null, // locToken
                null, // locMessage
                null, // locParam
                null); // throwable
    }

    public final String createSuccessMessage(String    taskId,
                                             String    locToken,
                                             String    locMessage,
                                             Object[]  locParam)
    {
        return createComponentMessage(
                taskId, // taskId
                true, // success
                NONE, // messageType
                locToken, // locToken
                locMessage, // locMessage
                locParam, // locParam
                null); // throwable
    }

    public final String createSuccessMessage(String    taskId,
                                             String    locToken,
                                             String    locMessage,
                                             Object    locParam)
    {
    	Object[] param = null;
    	if (locParam != null) {
    		param = new Object[] { locParam };
    	}
    	
        return createSuccessMessage(
                taskId, // taskId
                locToken, // locToken
                locMessage, // locMessage
                param); // locParam
    }

    public final String createExceptionMessage(
            String    taskId,
            String    locToken,
            String    locMessage,
            Object    locParam)
    {
        return createExceptionMessage(
                taskId,
                locToken,
                locMessage,
                locParam,
                null);
    }

    public final String createExceptionMessage(
            String    taskId,
            String    locToken,
            String    locMessage,
            Object    locParam,
            Throwable throwable)
    {
    	Object[] param = null;
    	if (locParam != null) {
    		param = new Object[] { locParam };
    	}
    	
        return createComponentMessage(
                taskId,
                false,
                ERROR,
                locToken,
                locMessage,
                param,
                throwable);
    }

    public final String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object[]  locParam,
                                         Throwable throwable)
    {
        return createComponentMessage(
                taskId,
                false,
                ERROR,
                locToken,
                locMessage,
                locParam,
                throwable);
    }

    public final String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object[]  locParam)
    {
        return createExceptionMessage(
                taskId,
                locToken,
                locMessage,
                locParam,
                null);
    }


    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object    locParam,
                               Throwable throwable)
        throws DeploymentException
    {
        String extMsg = createExceptionMessage(taskId,
                                               locToken,
                                               locMessage,
                                               locParam,
                                               throwable);
        throw new DeploymentException(extMsg, throwable);
    }

    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object    locParam)
        throws DeploymentException
    {
        String extMsg = createExceptionMessage(taskId,
                                               locToken,
                                               locMessage,
                                               locParam);
        throw new DeploymentException(extMsg);
    }
    
    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object[]  locParam,
                               Throwable throwable)
        throws DeploymentException
    {
        String extMsg = createExceptionMessage(taskId,
                                               locToken,
                                               locMessage,
                                               locParam,
                                               throwable);
        throw new DeploymentException(extMsg, throwable);
    }
    
    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object[]  locParam)
        throws DeploymentException
    {
        String extMsg = createExceptionMessage(taskId,
                                               locToken,
                                               locMessage,
                                               locParam);
        throw new DeploymentException(extMsg);
    }
    
    public final String createComponentMessage(
            String    taskId,
            boolean   success,
            int       messageType,
            String    locToken,
            String    locMessage,
            Object[]  locParam,
            Throwable throwable)
        throws JBIMessageException
    {
        return createComponentMessage(
                createComponentTaskResult(
                        taskId,
                        success,
                        messageType,
                        locToken,
                        locMessage,
                        locParam,
                        throwable));
    }

    public final ComponentTaskResult createComponentTaskResult(
                String    taskId,
                boolean   success,
                int       messageType,
                String    locToken,
                String    locMessage,
                Object[]  locParam,
                Throwable throwable)
            throws JBIMessageException
    {
        String componentName = getComponentName();
        if (componentName == null) {
            throw new IllegalStateException("Component name has not been set");
        }


        if (taskId == null) {
            throw new IllegalArgumentException("Task id cannot be null");
        }

        switch (messageType) {
            case NONE:
            case INFO:
            case WARNING:
            case ERROR:
                break;
            default:
                throw new IllegalArgumentException("Invalid message type (" + messageType + "). Must be one of INFO, WARNING or ERROR");
        }

        if (success) {
            if (messageType != NONE && messageType != INFO && messageType != WARNING) {
                throw new IllegalArgumentException("Successful operation can only report INFO or WARNING");
            }
        } else {
            if (messageType != WARNING && messageType != ERROR) {
                throw new IllegalArgumentException("Unsuccessful operation can only report WARNING or ERROR");
            }
        }

        TaskResultDetails trd = new TaskResultDetails();
        trd.setTaskId(taskId);
        trd.setSuccessfulResult(success);
        trd.setMessageType(messageType);

        locMessage = escapeChars(locMessage);
        if (throwable != null) {
            if (locToken == null) {
                throw new IllegalArgumentException("Localization token cannot be null");
            }
            if (locMessage == null) {
                throw new IllegalArgumentException("Localization message cannot be null");
            }
            MessageLocalizationInfo messageLocalizationInfo = new MessageLocalizationInfo(locToken, locMessage, locParam);
            trd.newExceptionInfo(throwable, messageLocalizationInfo);
        } else  if (locMessage != null) {
            if (locToken == null) {
                throw new IllegalArgumentException("Localization token cannot be null");
            }
            trd.newMessageLocalizationInfo(locToken, locMessage, locParam);
        }

        ComponentTaskResultDetails ctrd = new ComponentTaskResultDetails(trd);

        return new ComponentTaskResult(getComponentName(), ctrd);
    }

    private String escapeChars(String text) {
        if (text == null) {
            return null;
        }

        StringBuffer buffer = new StringBuffer();
        char[] chars = text.toCharArray();
        for (int i = 0, I = chars.length; i < I; i++) {
            switch (chars[i]) {
                case '{':
                    // do not escape left  brace if next character is a digit
                    // this is a shortcut, we should be verifying all characters
                    // preceeding closing brace can be parsed to a numeral - KPS
                    if ((i + 1) < I && Character.isDigit(chars[i + 1])) {
                        buffer.append("{");  
                    }
                    else buffer.append("'{'");
                    break;
                case '\'':
                    buffer.append("''");
                    break;
                default:
                    buffer.append(chars[i]);
            }
        }

        return buffer.toString();
    }

    protected abstract String createComponentMessage(ComponentTaskResult componentTaskResult)
        throws JBIMessageException;
}
