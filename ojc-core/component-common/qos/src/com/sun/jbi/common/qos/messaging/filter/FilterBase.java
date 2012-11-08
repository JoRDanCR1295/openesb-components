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
 * @(#)FilterBase.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.filter;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.AppInfo;
import com.sun.jbi.common.qos.messaging.ExchangeTemplates;
import com.sun.jbi.common.qos.messaging.SendFailureListener;

/**
 * Defines contract for supporting chained {@link ExchangeFilter} implementations.
 * @author Kevan Simpson
 */
public interface FilterBase {

    public static final Long ASYNC_SEND = new Long(Long.MIN_VALUE);

    /**
     * Adds an {@link ExchangeFilter} to this base.
     * @param filters One or more filters.
     */
    public void addExchangeFilter(ExchangeFilter... filters);

    /**
     * Fetches the base's <code>ComponentContext</code>.
     * @return the base's <code>ComponentContext</code>.
     */
    public ComponentContext getContext();

    /**
     * Fetches the base's <code>DeliveryChannel</code>.
     * @return the base's <code>DeliveryChannel</code>.
     */
    public DeliveryChannel getDeliveryChannel();

    /**
     * Fetches application info for the specified endpoint.
     * @param endpt An endpoint descriptor.
     * @return application info or <code>null</code>.
     */
    public AppInfo getAppInfo(EndpointInfo endpt);

    /**
     * Fetches an {@link ExchangeTemplates} for the specified unique id.
     * @param guid A unique message id.
     * @return an exchange template or <code>null</code>.
     */
    public ExchangeTemplates getTemplates(String guid);

    /**
     * Sends notification of sending errors to {@link SendFailureListener}s.
     * @param error The caught exception.
     * @param mex The exchange that caused the error.
     */
    public void notifyListeners(MessagingException error, MessageExchange mex);

    /**
     * Sends the specified {@link MessageExchange}.
     * @param msg A message exchange.
     * @throws MessagingException If an error occurs sending exchange.
     */
    public void sendMessage(MessageExchange msg) throws MessagingException;

    /**
     * Remove templates object from cache (should be called after successful delivery)
     */
    public void removeTemplates(String theGuid);
}
