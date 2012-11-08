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
 * @(#)JBITaskMessageBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import javax.jbi.management.DeploymentException;

public interface JBITaskMessageBuilder {
    // Message types
    /*
     * Task status message type: <code>NONE</code>
     */
    public static final int NONE = 0;
    /*
     * Task status message type: <code>INFO</code>
     */
    public static final int INFO = 1;
    /*
     * Task status message type: <code>WARNING</code>
     */
    public static final int WARNING = 2;
    /*
     * Task status message type: <code>ERROR</code>
     */
    public static final int ERROR = 3;

    /**
     * Set the component name to be used by this message builder for
     * <component-task-result> elements.
     *
     * @param componentName The component name to be used
     *
     * @throws NullPointerException If the given component name is null
     */
    public void setComponentName(String componentName);

    public String createSuccessMessage(String taskId);

    public String createSuccessMessage(String    taskId,
                                       String    locToken,
                                       String    locMessage,
                                       Object    locParam);

    public String createSuccessMessage(String    taskId,
                                       String    locToken,
                                       String    locMessage,
                                       Object[]  locParam);

    public String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object    locParam,
                                         Throwable throwable);

    public String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object    locParam);

    public String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object[]  locParam,
                                         Throwable throwable);

    public String createExceptionMessage(String    taskId,
                                         String    locToken,
                                         String    locMessage,
                                         Object[]  locParam);
    
    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object    locParam,
                               Throwable throwable)
        throws DeploymentException;

    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object    locParam)
        throws DeploymentException;
    
    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object[]  locParam,
                               Throwable throwable)
        throws DeploymentException;
    
    public void throwException(String    taskId,
                               String    locToken,
                               String    locMessage,
                               Object[]  locParam)
        throws DeploymentException;
    
    public String createComponentMessage(
            String    taskId,
            boolean   success,
            int       messageType,
            String    locToken,
            String    locMessage,
            Object[]  locParam,
            Throwable throwable)
        throws JBIMessageException;

    public ComponentTaskResult createComponentTaskResult(
            String    taskId,
            boolean   success,
            int       messageType,
            String    locToken,
            String    locMessage,
            Object[]  locParam,
            Throwable throwable)
        throws JBIMessageException;
}
