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
 * @(#)BaseMessagingChannel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.common.qos.messaging;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.descriptor.QoSAssembly;
import com.sun.jbi.common.qos.messaging.filter.ExchangeFilter;
import com.sun.jbi.common.qos.messaging.filter.FilterBase;
import com.sun.jbi.common.qos.messaging.filter.FilterChainImpl;
import com.sun.jbi.common.qos.messaging.filter.FilterChain;
import com.sun.jbi.common.qos.messaging.filter.RedeliveryFilter;
import com.sun.jbi.common.qos.messaging.filter.ThrottlingFilter;
import com.sun.jbi.common.util.Util;

/**
 * Default implementation of a {@link MessagingChannel} capable of
 * tracking redelivery status of message exchanges.
 * 
 * @author Kevan Simpson
 */
public class BaseMessagingChannel implements MessagingChannel, FilterBase {

    private ComponentContext mCtx;
    private DeliveryChannel mChannel;
    private MessageExchangeFactory mExchangeFactory;
    private Map<String, ExchangeTemplates> mTemplatesMap;
    private Map<String, Set<EndpointInfo>> mConsumerEndpointInfobySU;
    private Map<EndpointInfo, AppInfo> mAppInfoMap;
    private Set<SendFailureListener> mFailureListeners;
    //private List<ExchangeFilter> mExchangeFilters;
    private FilterChain filterChain;
    private Logger mLogger;
    private DeploymentLookup mDeploymentLookup;

    public BaseMessagingChannel(ComponentContext ctx)
            throws MessagingException {
        this(ctx, true);
    }

    public BaseMessagingChannel(ComponentContext ctx, boolean includeDefaultFilters)
            throws MessagingException {
        mCtx = ctx;
        mChannel = mCtx.getDeliveryChannel();
        mTemplatesMap = new HashMap<String, ExchangeTemplates>();
        mConsumerEndpointInfobySU = new HashMap<String, Set<EndpointInfo>>();
        mAppInfoMap = new HashMap<EndpointInfo, AppInfo>();
        mFailureListeners = new HashSet<SendFailureListener>();
//        mExchangeFilters = new ArrayList<ExchangeFilter>();
        filterChain = new FilterChainImpl();
        if (includeDefaultFilters) {
//            addExchangeFilter(new ThrottlingFilter(this),
//                              RedeliveryFilter.createRedeliveryFilter(this));
            filterChain.addFirst(new ThrottlingFilter(this));
            //disabled for testing
            filterChain.addLast(RedeliveryFilter.createRedeliveryFilter(this));

        }

        mExchangeFactory = mChannel.createExchangeFactory();
        mLogger = Util.getLogger(mCtx, MessagingChannel.class.getName());
        try {
            mDeploymentLookup = new DeploymentLookup(getContext());
        } catch (DeploymentException de) {
            String msg = I18n.loc(
                    "QOS-6080: Failed to acquire Deployment Service: {0}", de.
                    getMessage());
            log().log(Level.SEVERE, msg, de);
            throw new MessagingException(msg, de);
        }
    }

    public DeploymentLookup getLookup() {
        return mDeploymentLookup;
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#addServiceQualityToEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.qos.ServiceQuality[]) */
    public void addServiceQualityToEndpoint(EndpointInfo endpoint, ServiceQuality... qos) {
        if (endpoint != null)
            for (ExchangeFilter filter : filterChain.getFilterList())
                filter.addServiceQuality(endpoint, qos);
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#removeServiceQualityFromEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.qos.ServiceQuality[]) */
    public void removeServiceQualityFromEndpoint(EndpointInfo endpoint, ServiceQuality... qos) {
        for (ExchangeFilter filter : filterChain.getFilterList())
            filter.removeServiceQuality(endpoint, qos);
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#getServiceQualitiesForEndpoint(com.sun.jbi.common.descriptor.EndpointInfo) */
    public List<ServiceQuality> getServiceQualitiesForEndpoint(EndpointInfo endpoint) {
        List<ServiceQuality> list = new ArrayList<ServiceQuality>();
        ServiceQuality qual = null;
        for (ExchangeFilter filter : filterChain.getFilterList()) {
            qual = filter.getServiceQuality(endpoint);
            if (qual != null)
                list.add(qual);
        }

        return list;
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#getServiceQuality(com.sun.jbi.common.descriptor.EndpointInfo, java.lang.Class) */
    public <T extends ServiceQuality> T getServiceQuality(EndpointInfo endpoint, Class<T> qosType) {
        ServiceQuality qual = null;
        for (ExchangeFilter filter : filterChain.getFilterList()) {
            qual = filter.getServiceQuality(endpoint);
            if (qual != null && qosType.isInstance(qual))
                return (T) qual;
        }

        return null;
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#installServiceQualities(java.lang.String, java.lang.String) */
    public void installServiceQualities(String suName, String suPath)
            throws DeploymentException {
        try {
            QoSAssembly sa = getLookup().getQoSAssembly(suName);
            ServiceUnit srvcUnit = ServicesDescriptor.parse(suName, suPath);
            Map<EndpointInfo, List<ServiceQuality>> qosMap =
                    sa.getServiceQualities();

            Set<EndpointInfo> endpointInfoSet = qosMap.keySet();
            mConsumerEndpointInfobySU.put(suName, endpointInfoSet);

            AppInfo appInfo = new AppInfo(sa, suName);
            for (EndpointInfo epInfo : srvcUnit.getServices().getEndpoints()) {
                // add assembly and su info for all endpoints
                mAppInfoMap.put(epInfo, appInfo);

                if (epInfo.isProvides())
                    addServiceQualityToEndpoint(epInfo);
                else {
                    // add the QOS params for the consumer endpoints.
                    List<ServiceQuality> qosList = qosMap.get(epInfo);
                    if (qosList != null && qosList.size() > 0) {
                        ServiceQuality[] qosArray = new ServiceQuality[qosList.
                                size()];
                        addServiceQualityToEndpoint(epInfo, qosList.toArray(
                                qosArray));
                    }

                }
            }
        } catch (DeploymentException de) {
            throw de;
        } catch (Exception e) {
            String msg = I18n.loc(
                    "QOS-6063: Failed to install service qualities for service unit \"{0}\": {1}",
                    suName, e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new DeploymentException(msg, e);
        }

    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#uninstallServiceQualities(java.lang.String) */
    public void uninstallServiceQualities(String suName) {
        Set<EndpointInfo> endpointInfoSet = mConsumerEndpointInfobySU.remove(
                suName);    // changed from get() to remove()

        if (endpointInfoSet != null)
            for (EndpointInfo epInfo : endpointInfoSet)
                if (!epInfo.isProvides()) {
                    removeServiceQualityFromEndpoint(epInfo);
                    mAppInfoMap.remove(epInfo);
                }

        // NEVER throws Exception as this is to clear cache
        getLookup().notifyUndeployed(suName);
    }

    /** @see javax.jbi.messaging.DeliveryChannel#accept() */
    public MessageExchange accept() throws MessagingException {
        MessageExchange ex = null;
        do {
            ex = mChannel.accept();
            /*
             * No QoS support for this send if Endpoint is not set on the
             * MessageExchange by calling setEndpoint(ServiceEndpoint endpoint).
             * Please note that NMR allows multiple ways to route the message, QoS
             * library is not equipped to support all those scenarios
             */
            if (ex.getEndpoint() == null)
                return ex;
            ex = filterChain.processIncomingExchange(ex);
//            for (ExchangeFilter filter : mExchangeFilters) {
//                ex = filter.processIncomingExchange(ex);
//                if (ex == null) break;
//            }
        } while (ex == null);
        return ex;
    }

    /** @see javax.jbi.messaging.DeliveryChannel#accept(long) */
    public MessageExchange accept(long wait) throws MessagingException {
        MessageExchange ex = null;
        do {
            ex = mChannel.accept(wait);
            if (ex == null)
                return ex;
            /*
             * No QoS support for this send if Endpoint is not set on the
             * MessageExchange by calling setEndpoint(ServiceEndpoint endpoint).
             * Please note that NMR allows multiple ways to route the message, QoS
             * library is not equipped to support all those scenarios
             */
            if (ex.getEndpoint() == null)
                return ex;
            ex = filterChain.processIncomingExchange(ex);
//            for (ExchangeFilter filter : mExchangeFilters) {
//                ex = filter.processIncomingExchange(ex);
//                if (ex == null) break;
//            }
        } while (ex == null);
        return ex;
    }

    /** @see javax.jbi.messaging.DeliveryChannel#close() */
    public void close() throws MessagingException {
        MessagingException ex = null;
        try {
            filterChain.close();
        } catch (MessagingException e) {
            ex = e;
        } finally {
            mChannel.close();
            if (ex != null)
                throw ex;
        }
//        for (ExchangeFilter filter : mExchangeFilters) {
//            try {
//                filter.close();
//            }
//            catch (Exception e) {
//                // catch to make sure DC has a chance to close
//                String msg = I18n.loc(
//                        "QOS-6076: {0} failed to close: {1}",
//                        filter.getClass().getSimpleName(), e.getMessage());
//                log().log(Level.WARNING, msg, e);
//                ex = new MessagingException(msg, e);
//            }
//        }
    }

    /** @see javax.jbi.messaging.DeliveryChannel#createExchangeFactory() */
    public MessageExchangeFactory createExchangeFactory() {
        return mChannel.createExchangeFactory();
    }

    /** @see javax.jbi.messaging.DeliveryChannel#createExchangeFactory(javax.xml.namespace.QName) */
    public MessageExchangeFactory createExchangeFactory(QName arg0) {
        return mChannel.createExchangeFactory(arg0);
    }

    /** @see javax.jbi.messaging.DeliveryChannel#createExchangeFactory(javax.jbi.servicedesc.ServiceEndpoint) */
    public MessageExchangeFactory createExchangeFactory(ServiceEndpoint arg0) {
        return mChannel.createExchangeFactory(arg0);
    }

    /** @see javax.jbi.messaging.DeliveryChannel#createExchangeFactoryForService(javax.xml.namespace.QName) */
    public MessageExchangeFactory createExchangeFactoryForService(QName arg0) {
        return mChannel.createExchangeFactoryForService(arg0);
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#send(com.sun.jbi.common.qos.messaging.ExchangeTemplates) */
    public void send(ExchangeTemplates param) throws MessagingException {
        String uniqueId = param.getUniqueId();
        mTemplatesMap.put(uniqueId, param);

        if (mLogger.isLoggable(Level.FINER))
            mLogger.finer("Adding template " + uniqueId + " to mTemplatesMap, now have: " + mTemplatesMap.
                    keySet());

        send(param.createExchange());
    }

    /** @see javax.jbi.messaging.DeliveryChannel#send(javax.jbi.messaging.MessageExchange) */
    public void send(MessageExchange msg) throws MessagingException {
        /*
         * No QoS support for this send if Endpoint is not set on the
         * MessageExchange by calling setEndpoint(ServiceEndpoint endpoint).
         * Please note that NMR allows multiple ways to route the message, QoS
         * library is not equipped to support all those scenarios
         */
        if (msg.getEndpoint() == null) {
            mChannel.send(msg);
            return;
        }

//        for (ExchangeFilter filter : mExchangeFilters) {
//            msg = filter.processOutgoingExchange(msg, ASYNC_SEND);
//            if (msg == null) break;
//        }
        msg = filterChain.processOutgoingExchange(msg, ASYNC_SEND);

        if (msg != null)
            mChannel.send(msg);
        else if (log().isLoggable(Level.FINEST)) {
            log().finest("QOS-1003: Send aborted, filters processed exchange...");
            log().finest("QOS-1006: testing for message id");
        }
    }

    /** @see javax.jbi.messaging.DeliveryChannel#sendSync(javax.jbi.messaging.MessageExchange) */
    public boolean sendSync(MessageExchange msg) throws MessagingException {
        return sendSync(msg, 0);
    }

    /** @see javax.jbi.messaging.DeliveryChannel#sendSync(javax.jbi.messaging.MessageExchange, long) */
    public boolean sendSync(MessageExchange msg, long wait) throws MessagingException {
//        Long objWait = new Long(wait);
//        for (ExchangeFilter filter : mExchangeFilters) {
//            msg = filter.processOutgoingExchange(msg, objWait);
//            if (msg == null) break;
//        }
        msg = filterChain.processOutgoingExchange(msg, wait);

        if (msg != null)
            return mChannel.sendSync(msg, wait);
        else {
            if (log().isLoggable(Level.FINEST))
                log().finest(
                        "QOS-1003: Send aborted, filters processed exchange...");
            return false;
        }
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#getExchangeFactory() */
    public MessageExchangeFactory getExchangeFactory() {
        return mExchangeFactory;
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#addSendFailureListener(com.sun.jbi.common.qos.messaging.SendFailureListener) */
    public void addSendFailureListener(SendFailureListener sfl) {
        if (sfl != null)
            mFailureListeners.add(sfl);
    }

    /** @see com.sun.jbi.common.qos.messaging.MessagingChannel#removeSendFailureListener(com.sun.jbi.common.qos.messaging.SendFailureListener) */
    public void removeSendFailureListener(SendFailureListener sfl) {
        if (sfl != null)
            mFailureListeners.remove(sfl);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#addExchangeFilter(com.sun.jbi.common.qos.messaging.filter.ExchangeFilter[]) */
    public void addExchangeFilter(ExchangeFilter... filters) {
        if (filters != null)
            for (int i = 0, n = filters.length; i < n; i++)
                if (filters[i] != null)
                    filterChain.addLast(filters[i]);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#getContext() */
    public ComponentContext getContext() {
        return mCtx;
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#getDeliveryChannel() */
    public DeliveryChannel getDeliveryChannel() {
        return mChannel;
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#getTemplates(java.lang.String) */
    public ExchangeTemplates getTemplates(String guid) {
        return mTemplatesMap.get(guid);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#getAppInfo(com.sun.jbi.common.descriptor.EndpointInfo) */
    public AppInfo getAppInfo(EndpointInfo endpt) {
        return mAppInfoMap.get(endpt);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#notifyListeners(javax.jbi.messaging.MessagingException, javax.jbi.messaging.MessageExchange) */
    public void notifyListeners(MessagingException error, MessageExchange mex) {
        for (SendFailureListener sfl : mFailureListeners)
            sfl.handleSendFailure(error, mex);
    }

    /** @see com.sun.jbi.common.qos.messaging.filter.FilterBase#sendMessage(javax.jbi.messaging.MessageExchange) */
    public void sendMessage(MessageExchange msg) throws MessagingException {
        send(msg);
    }

    protected Logger log() {
        return mLogger;
    }

    /**
     * {@inheritDoc}
     */
    public void removeTemplates(String theGuid) {
        mTemplatesMap.remove(theGuid);

        if (mLogger.isLoggable(Level.FINER))
            mLogger.finer("Removed template " + theGuid + " from mTemplatesMap, now have: " + mTemplatesMap.
                    keySet());
    }
}
