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
 * @(#)AbstractExchangeFilter.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.util.Util;

/**
 * Abstract base class for {@link ExchangeFilter} implementations.
 * @author Kevan Simpson
 */
public abstract class AbstractExchangeFilter implements ExchangeFilter {

    private Logger mLogger;
    private FilterBase mFilterBase;
    private Map<EndpointInfo, ServiceQuality> mQosMap;

    protected AbstractExchangeFilter(FilterBase base) {
        mFilterBase = base;
        mQosMap = new HashMap<EndpointInfo, ServiceQuality>();
        // to avoid multiple loggers, use MessagingChannel as logger name
        mLogger = Util.getLogger(base.getContext(), MessagingChannel.class.getName());
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#processIncomingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public abstract MessageExchange processIncomingExchange(MessageExchange msg, Object... params) throws MessagingException;

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#processOutgoingExchange(javax.jbi.messaging.MessageExchange, java.lang.Object[]) */
    public abstract MessageExchange processOutgoingExchange(MessageExchange msg, Object... params) throws MessagingException;

    /**
     * Returns <code>true</code> if the specified {@link ServiceQuality} is valid for this filter.
     *
     * @param srvcQual The specified <code>ServiceQuality</code>.
     * @return <code>true</code> if the quality is valid, else <code>false</code>.
     */
    protected abstract boolean acceptsServiceQuality(ServiceQuality srvcQual);

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#addServiceQuality(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.qos.ServiceQuality[]) */
    public void addServiceQuality(EndpointInfo endpoint, ServiceQuality... qos) {
        if (endpoint != null && qos != null) {

            for (ServiceQuality qual : qos) {
                if (acceptsServiceQuality(qual)) {
                    synchronized (mQosMap) {
                        mQosMap.put(endpoint, qual);
                    }
                    break;	// the assumption is each filter will support only one ServiceQuality instance
                }
            }
        }
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#close() */
    public void close() {
        // no op, by default
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#getServiceQuality(com.sun.jbi.common.descriptor.EndpointInfo) */
    public ServiceQuality getServiceQuality(EndpointInfo endpoint) {
        return (endpoint == null) ? null : mQosMap.get(endpoint);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.ExchangeFilter#removeServiceQuality(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.qos.ServiceQuality[]) */
    public void removeServiceQuality(EndpointInfo endpoint, ServiceQuality... qos) {
        if (endpoint != null) {
            if (qos == null || qos.length == 0) {
                synchronized (mQosMap) {
                    mQosMap.remove(endpoint);
                }
            } else {
                for (ServiceQuality qual : qos) {
                    if (acceptsServiceQuality(qual)) {
                        synchronized (mQosMap) {
                            mQosMap.remove(endpoint);
                        }
                        break;	// the assumption is each filter will support only one ServiceQuality instance
                    }
                }
            }
        }
    }

    protected FilterBase getBase() {
        return mFilterBase;
    }

    /**
     * Fetches the {@link ServiceQuality} for the specified message.  The
     * filter implementation is expected to cast the return value to the
     * correct type.
     *
     * @param msg The specified message.
     * @return A configured <code>ServiceQuality</code> or <code>null</code>.
     */
    protected ServiceQuality getServiceQuality(MessageExchange msg) {
        // use original endpoint, not loopback or error, for lookup...
        ServiceEndpoint endpt = Redelivery.getEndpoint(msg);
        if (endpt == null) {
            return null;
        } else {
            EndpointInfo info = EndpointInfo.valueOf(endpt, false);
            return mQosMap.get(info);
        }
    }

    protected Logger log() {
        return mLogger;
    }

    /* For filter implementations that do not want to use MessagingChannel as logger name. */
    protected void setLogger(Logger logger) {
        if (logger != null) {
            mLogger = logger;
        }
    }

    protected MessagingException unsupportedRedeliveryPattern(MessageExchange msg,
            Failure failure) {
        String pattern = (msg instanceof InOut)
                ? "InOut" : ((msg instanceof InOnly)
                ? "InOnly" : msg.getClass().getSimpleName());
        String err = I18n.loc(
                "QOS-6010: Invalid redelivery configuration: {0} not supported for {1}!",
                // TODO get pattern, migrate ExchangePattern
                pattern, failure);
        mLogger.warning(err);
        return new MessagingException(err);
    }
}
