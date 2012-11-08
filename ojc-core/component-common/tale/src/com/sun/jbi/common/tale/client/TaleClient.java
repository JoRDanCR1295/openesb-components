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
 * @(#)TaleClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client;

import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * Defines interface for ALE Java client.
 * @author Kevan Simpson
 */
public interface TaleClient {
    /**
     * Name of default template to use when sending ALE messages.
     */
    public static final String DEFAULT_TEMPLATE = "defaultALERequestTemplate";
    
    /**
     * Sends an alert message.
     * @param request The ALE message.
     * @throws TaleException if an error occurs sending message.
     */
    public void sendAlert(TaleRequest request) throws TaleException;
    /**
     * Sends a log message.
     * @param request The ALE message.
     * @throws TaleException if an error occurs sending message.
     */
    public void sendLog(TaleRequest request) throws TaleException;
    /**
     * Sends an error message.
     * @param request The ALE message.
     * @throws TaleException if an error occurs sending message.
     */
    public void sendError(TaleRequest request) throws TaleException;
    
    /**
     * Adds a request template to this client.
     * @param name The name of the template.
     * @param template The template request.
     */
    public void addRequestTemplate(String name, TaleRequest template);
    /**
     * Fetches a request template, so as to modify its properties.
     * @param name The name of the template.
     * @return A template request or <code>null</code>.
     */
    public TaleRequest getRequestTemplate(String name);
    /**
     * Removes a request template from this client.
     * @param name The name of the template.
     * @return A template request or <code>null</code>.
     */
    public TaleRequest removeRequestTemplate(String name);
    
    /**
     * Instantiates an ALE request using the default template.
     * @return A new request.
     */
    public TaleRequest newRequest();
    /**
     * Instantiates an ALE request using the specified template.
     * @param templateName The name of the template.
     * @return A new request.
     */
    public TaleRequest newRequest(String templateName);
    
    /**
     * Fetches the convenience alert client.
     * @return the convenience alert client.
     */
    public AlertClient getAlertClient();
    /**
     * Fetches the convenience logging client.
     * @return the convenience logging client.
     */
    public LogClient getLogClient();
    /**
     * Fetches the convenience error reporting client.
     * @return the convenience error reporting client.
     */
    public ErrorClient getErrorClient();
}
