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
 * @(#)Channel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSOperation;

/**
 *  A Channel is an abstraction of a JMS message flow "pipeline".
 *   
 */
public interface Channel {
    
    // JMS message types as ints
    public static final int MESSAGE_TYPE_UNKNOWN = -1;
    public static final int MESSAGE_TYPE_TEXT = 0;
    public static final int MESSAGE_TYPE_STREAM = 1;
    public static final int MESSAGE_TYPE_BYTES = 2;
    public static final int MESSAGE_TYPE_MAP = 3;
    public static final int MESSAGE_TYPE_OBJECT = 4;
    public static final int MESSAGE_TYPE_MESSAGE = 5;
    
    // JMS Destination Types as ints
    public static final int DESTINATION_TYPE_UNKNOWN = -1;
    public static final int DESTINATION_TYPE_TOPIC = 0;
    public static final int DESTINATION_TYPE_QUEUE = 1;
    
    /**
     * Initializes the channel.
     *
     * @param jmsAddress The JMSAddress instance.
     * @param jmsOperation  The JMSOperation instance.
     */
    public void initialize (JMSAddress jmsAddress,
                            JMSOperation jmsOperation);    

    /**
     * Opens the channel.
     * Connection to the JMS server instance should be established.
     *
     * @throws ChannelException upon error
     */
    public void open() throws ChannelException;

    /**
     * Closes the channel.
     * Connection to the JMS server should be closed.
     *
     * @throws ChannelException upon error
     */
    public void close() throws ChannelException;
    
    /**
     * Allow message to "flow" through channel.
     *
     * @throws ChannelException upon error
     */
    public void start() throws ChannelException;

    /**
     * Stop (block) messages from flowing through channel.
     *
     * @throws ChannelException upon error
     */
    public void stop() throws ChannelException;
    
    /**
     * Checks whether the channel is opened.
     * Connection to the JMS server instance should be established.
     *
     * @returns true if channel is opened, false otherwsie.
     */
    public boolean isOpen();
    
    /**
     * Checks whether the channel has been started.
     *
     * @returns true if channel is started, false otherwsie.
     */
    public boolean isStarted();
}
