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
 * @(#)ExchangeFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.filter;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;

/**
 * Defines the contract for a {@link MessagingChannel} filter, responsible for
 * processing incoming and outgoing message exchanges.
 * 
 * @author Kevan Simpson
 */
public interface ExchangeFilter {

    /**
     * Processes an exchange sent via the NMR.
     * @param msg The sent exchange.
     * @param params Optional params needed by filter, provided by <code>FilterBase</code>.
     * @return The message exchange to send or <code>null</code>.
     * @throws MessagingException if an error occurs processing.
     */
    public MessageExchange processOutgoingExchange(MessageExchange msg, Object... params) throws MessagingException;

    /**
     * Processes an exchange received from the NMR.
     * @param msg The received exchange.
     * @param params Optional params needed by filter, provided by <code>FilterBase</code>.
     * @return The received message exchange or <code>null</code>.
     * @throws MessagingException if an error occurs processing.
     */
    public MessageExchange processIncomingExchange(MessageExchange msg, Object... params) throws MessagingException;

    /**
     * Adds {@link ServiceQuality) instances to this filter.
     * @param endpoint The endpoint for which the qualities are configured.
     * @param qos Zero or more service qualities.
     */
    public void addServiceQuality(EndpointInfo endpoint, ServiceQuality... qos);

    /**
     * Fetches a {@link ServiceQuality) for the specified endpoint.
     * @param endpoint The endpoint for which the quality is configured.
     * @return A <code>ServiceQuality</code> or <code>null</code>.
     */
    public ServiceQuality getServiceQuality(EndpointInfo endpoint);

    /**
     * Removes {@link ServiceQuality) instances from this filter.
     * @param endpoint The endpoint for which the qualities are configured.
     * @param qos Zero or more service qualities.
     */
    public void removeServiceQuality(EndpointInfo endpoint, ServiceQuality... qos);

    /**
     * Closes the filter and cleans up resources as needed.
     */
    public void close();
}
