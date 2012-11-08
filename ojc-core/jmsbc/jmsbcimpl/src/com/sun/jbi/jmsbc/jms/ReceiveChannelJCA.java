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
 * @(#)ReceiveChannelJCA.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import javax.resource.spi.endpoint.MessageEndpointFactory;

/**
 *
 * ReceiveChannelJCA is an abstraction of a JMS message in flow "pipeline".
 * Messages flow from the JMS Server.  This type of channel uses the JCA message
 * inflow architectur for message processing.
 *
 */
public interface ReceiveChannelJCA extends ReceiveChannel {


    /**
     * Sets the MessageEndpointFactory which will be used by the channel
     * to generate a MessageEndpoint to which the Message will be handed off
     * for processing.
     *
     * @param factory  The MessageEndpointFactory used to generate MessageEndpoint(s).
     */
    public void setMessageEndpointFactory (MessageEndpointFactory factory);

    /**
     * This will set the Maximum Concurrent Consumers. If value is 0 then default is used.
     * 
     * @param maximumConcurrencyConsumers
     */
    public void setMaximumConcurrentConsumers (int maximumConcurrentConsumers);

    public void setAppendRedeliveryHandlingWaitTime(long appendRedeliveryHandlingWaitTime);
}
