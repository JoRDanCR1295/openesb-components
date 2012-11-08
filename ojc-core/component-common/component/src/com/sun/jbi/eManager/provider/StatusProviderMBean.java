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
 * @(#)StatusProviderMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

/**
 *
 * @author Sun Microsystems
 */
public interface StatusProviderMBean extends ComponentEndpointMonitoringData {

    /**  Retrieves a short display name, should be kept as succinct as possible - as a guideline max 16 characters */
    String getShortDisplayName();
    /**  Retrieves the list of provisioning endpoints for that component */
    public String[] getProvisioningEndpoints();
    /**  Retrieves the list of consuming endpoints for that component */
    public String[] getConsumingEndpoints();
    /**  Retrieves the total number of requests handled */
    public Long getTotalRequests();
    /**  Retrieves the total number of replies sent out */
    public Long getTotalReplies();
    /**  Retrieves the total number of messages with faults */
    public Long getTotalErrors();
    /**  Retrieves the total number of messages successfully processed. */
    public Long getTotalDone();
    /**  Retrieves the total number of message requests sent. */
    public Long getTotalSentRequests();
    /**  Retrieves the total number of message replies sent. */
    public Long getTotalSentReplies();
    /**  Retrieves the total number of errors sent. */
    public Long getTotalSentErrors();
    /**  Retrieves the total number of messages sent that were successfully processed.  */
    public Long getTotalSentDones();
    /**  Retrieves the total number of requests received that were successfully processed.  */
    public Long getTotalReceivedRequests();
    /**  Retrieves the total number of received replies. */
    public Long getTotalReceivedReplies();
    /**  Retrieves the total number of received errors. */
    public Long getTotalReceivedErrors();
    /**  Retrieves the total number of receives successfully processed. */
    public Long getTotalReceivedDones();
    /**  Retrieves the number of sent requests for the specified endpoint. */
    public Long getSentRequests(String endpoint);
    /**  Retrieves the number of sent replies for the specified endpoint. */
    public Long getSentReplies(String endpoint);
    /**  Retrieves the number of sent errors for the specified endpoint. */
    public Long getSentErrors(String endpoint);
    /**  Retrieves the number of sent successfully processed for the specified endpoint. */
    public Long getSentDones(String endpoint);
    /**  Retrieves the number of received requests for the specified endpoint. */
    public Long getReceivedRequests(String endpoint);
    /**  Retrieves the number of received replies for the specified endpoint. */
    public Long getReceivedReplies(String endpoint);
    /**  Retrieves the number of received errors for the specified endpoint. */
    public Long getReceivedErrors(String endpoint);
    /**  Retrieves the number of received successfully processed for the specified endpoint. */
    public Long getReceivedDones(String endpoint);

    static final String OBJECT_NAME_PREFIX = "com.sun.ebi:ServiceType=Status,InstallationType=";
    static final String OBJECT_NAME_SUFFIX = ",IdentificationName=";

    static final String COMPONENT_TYPE_BINDING = "bindingComponents";
    static final String COMPONENT_TYPE_ENGINE = "engineComponents";
}
