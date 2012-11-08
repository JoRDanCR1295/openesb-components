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
 * @(#)SendFailureListener.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.messaging;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

/**
 * An error listener for scenarios when the asynchronous nature of sending 
 * message exchanges prevents errors that occur during the send cannot be 
 * thrown/returned to the users.  
 * <p>
 * Three of these scenarios have been identified:
 * <ol>
 *      <li>When user has specified waitTime for redelivery, a <code>TimerTask</code>
 *          is created to execute the send.  If the send fails, the 
 *          {@link MessagingException} cannot be caught by the component as it 
 *          was thrown by the task's thread, not the component's.</li>
 *      <li>When user has configured throttling and the max count has been 
 *          reached, message exchanges will be queued until the throttling count 
 *          is decremented.  If the send fails once the exchange is unqueued, 
 *          there is no means to pass or throw the {@link MessagingException} 
 *          to the component.</li>
 *      <li>When a service unit has been stopped, no invocations should occur for
 *          endpoints defined in that service unit.  Consequently, these message 
 *          exchanges will also be queued until the service unit is started or 
 *          undeployed.  If started, queued exchanges will be sent and any 
 *          {@link MessagingException}s that occur will be diverted to the 
 *          listener(s). If undeployed, {@link MessagingException}s will be thrown 
 *          to indicate the channel's inability to send the queued exchanges.</li>
 * </ol>
 * 
 * @author Kevan Simpson
 */
public interface SendFailureListener {
    /**
     * Handles {@link MessagingException}s that occur when a {@link MessageExchange}
     * cannot be sent on the {@link DeliveryChannel}.
     * 
     * @param error The exception that is caught or prepared by {@link MessagingChannel}.
     * @param mex The message exchange related to the specified error.
     */
    public void handleSendFailure(MessagingException error, MessageExchange mex);
}
