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
 * @(#)InboundReplyContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.swiftbc.extservice.server.SwiftCallback;

/**
 * Holds objects used by the asynchronous outbound message processor to process the NMR reply it
 * receives to complete an inbound JBI message exchange (Swift->nmr, nmr->Swift).
 * 
 * @author S. Nageswara Rao
 */
public class InboundReplyContext extends SAGConstants {

    private long mRequestInvocationTime;

    private NormalizedMessage mSwiftRequest;

    private MessageExchangeReplyListener mListener;

    private SwiftCallback mSwiftCallback;


    /**
     * Creates a new instance of InboundReplyContext
     * 
     * @param requestInvocationTime The time in milliseconds when the MessageExchange was received
     * @param SwiftRequest The received Swift message
     * @param listener The call back which will be processing the reply
     * @param SwiftCallback a channel to route the acknowledgments to the Swift external system
     */
    public InboundReplyContext(long requestInvocationTime, NormalizedMessage SwiftRequest,
            MessageExchangeReplyListener listener, SwiftCallback SwiftCallback) {
        this.mRequestInvocationTime = requestInvocationTime;
        this.mSwiftRequest = SwiftRequest;
        this.mListener = listener;
        this.mSwiftCallback = SwiftCallback;
    }

    /**
     * Creates a new instance of InboundReplyContext
     * 
     * @param requestInvocationTime The time in milliseconds when the MessageExchange was received
     * @param SwiftRequest The received Swift message
     * @param listener The call back which will be processing the reply
     * @param SwiftCallback a channel to route the acknowledgments to the Swift external system
     * @param ackCond The ackowledgment condition
     */
    public InboundReplyContext(long requestInvocationTime, NormalizedMessage SwiftRequest,
            MessageExchangeReplyListener listener, SwiftCallback SwiftCallback, String ackCond) {
        this.mRequestInvocationTime = requestInvocationTime;
        this.mSwiftRequest = SwiftRequest;
        this.mListener = listener;
        this.mSwiftCallback = SwiftCallback;
    }

    /**
     * Returns the time in milliseconds when the MessageExchange (request) was received.
     * 
     * @return The time in milliseconds when the MessageExchange (request) was received.
     */
    public long getRequestInvocationTime() {
        return mRequestInvocationTime;
    }

    /**
     * Returns the received Swift Message
     * @return The Swift message.
     */
    public NormalizedMessage getSwiftRequest() {
        return mSwiftRequest;
    }

    /**
     * Returns the reply listener which will process the NMR reponse.
     * @return The inbound reply listener.
     */
    public MessageExchangeReplyListener getMessageExchangeReplyListener() {
        return mListener;
    }

    /**
     * Returns the channel to route the acknowledgments to the Swift external system 
     * @return The Swift Callback.
     */
    public SwiftCallback getSwiftCallback() {
        return mSwiftCallback;
    }

}
