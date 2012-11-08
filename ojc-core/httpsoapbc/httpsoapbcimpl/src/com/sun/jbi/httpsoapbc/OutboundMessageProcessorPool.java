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
 * @(#)OutboundMessageProcessorPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.httpsoapbc.management.HTTPManagementMBean;

import java.util.LinkedList;
import java.util.Map;


public class OutboundMessageProcessorPool {
    private final LinkedList<OutboundMessageProcessor> mOutboundMessageProcessors;
    private final MessagingChannel mChannel;
    private final Map mEndpoints;
    private final Map mInboundExchanges;
    private final HTTPManagementMBean mManagementMBean;
    private int mActiveProcessors = 0;
    private int mMaxSize = 0;

    /** Creates a new instance of OutboundMessageProcessorPool */
    public OutboundMessageProcessorPool(MessagingChannel chnl, Map endpoints, Map inboundExchanges, HTTPManagementMBean managementMBean) {
        mOutboundMessageProcessors = new LinkedList<OutboundMessageProcessor>();
	mChannel = chnl;
	mEndpoints = endpoints;
	mInboundExchanges = inboundExchanges;
	mManagementMBean = managementMBean;
    }
    
    public OutboundMessageProcessorPool(int initSize, int maxSize, MessagingChannel chnl, Map endpoints, Map inboundExchanges, HTTPManagementMBean managementMBean) {
        this(chnl, endpoints, inboundExchanges, managementMBean);
        for (int i = 0; i < initSize; ++i) {
            mOutboundMessageProcessors.addFirst(new OutboundMessageProcessor(mChannel, mEndpoints, mInboundExchanges, mManagementMBean));
        }
        mMaxSize = maxSize;
    }
    
    public synchronized OutboundMessageProcessor retrieve() {
        OutboundMessageProcessor transformer = null;
        
	if (!mOutboundMessageProcessors.isEmpty()) {
	    transformer = mOutboundMessageProcessors.removeFirst();
	} else {
	    transformer = new OutboundMessageProcessor(mChannel, mEndpoints, mInboundExchanges, mManagementMBean);
	}

        mActiveProcessors++;
        return transformer;
    }
    
    public synchronized boolean relinquish(OutboundMessageProcessor transformer) {
        boolean success = false;
        if (mActiveProcessors + mOutboundMessageProcessors.size() > mMaxSize) {
            success = true;
        } else {        
            if (transformer != null) {
                if (!mOutboundMessageProcessors.contains(transformer)) {
                    mOutboundMessageProcessors.addFirst(transformer);
                    success = true;
                }
            }
        }
        mActiveProcessors--;
        return success;
    }
    
    public synchronized void setSize(int size) {
        if (size < mActiveProcessors + mOutboundMessageProcessors.size()) {
            int numToRemove = (size < mOutboundMessageProcessors.size())? size : mOutboundMessageProcessors.size();
            for (int i=0; i < numToRemove; i++) {
                mOutboundMessageProcessors.removeFirst();
            }
        }
    }
}
