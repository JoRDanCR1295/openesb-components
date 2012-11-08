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
 * @(#)AbstractExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange.Role;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.ObjectName;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.jbi.component.toolkit.util.I18n;

/**
 * Abstract base implementation of {@link ExchangeHandler}.
 * @author Kevan Simpson
 */
public abstract class AbstractExchangeHandler implements ExchangeHandler {
    private ManagerContext mMgrCtx;
    private List<PollerThread> mPollers;
    private Logger mLogger;

    protected AbstractExchangeHandler(ComponentContext ctx) {
        mPollers = new ArrayList<PollerThread>();
        mLogger = Util.getLogger(ctx, this.getClass().getName());
    }

    /** @see javax.management.NotificationListener#handleNotification(javax.management.Notification, java.lang.Object) */
    public void handleNotification(Notification note, Object handback) {
        if (note instanceof AttributeChangeNotification) {
            AttributeChangeNotification chg = (AttributeChangeNotification) note;
            if (chg.getAttributeName().equals(AcceptPoller.POLLER_COUNT_PROPERTY)) {
                int newVal = ((Integer) chg.getNewValue()).intValue();
                updatePollers(newVal);
            }
        }
    }

    /** @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return null;    // do we bother fetching from component, no
    }
    
    /** @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext arg0) throws JBIException {
        getContext().getConfiguration().addNotificationListener(this);
        getPollers().clear();
    }

    /**
     * Starts NMR polling threads. 
     * @see javax.jbi.component.ComponentLifeCycle#start() 
     */
    public void start() throws JBIException {
        startAccepting();
    }

    /**
     * Stops NMR polling threads.
     * @see javax.jbi.component.ComponentLifeCycle#stop()
     */
    public void stop() throws JBIException {
        stopAccepting();
        getPollers().clear();
    }

    /** @see javax.jbi.component.ComponentLifeCycle#shutDown() */
    public void shutDown() throws JBIException {
        getPollers().clear();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#getContext() */
    public ManagerContext getContext() {
        return mMgrCtx;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#setContext(com.sun.jbi.component.toolkit.lifecycle.ManagerContext) */
    public void setContext(ManagerContext ctx) {
        mMgrCtx = ctx;
    }

    protected void startAccepting() throws JBIException {
        try {
            updatePollers(getPollerCount());
        }
        catch (Exception e) {
            String msg = I18n.loc(
                    "COMPTK-6013: Failed to start \"{0}\" NMR pollers: {1}", 
                    getContext().getComponentContext().getComponentName(), e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }
    }
    
    protected void stopAccepting() throws JBIException {
        boolean logFiner = log().isLoggable(Level.FINER);
        // first, stop the runnables
        for (PollerThread pt : getPollers()) {
            // give each poller chance to stop, regardless of errors
            try {
                pt.getPoller().stop();

                if (logFiner) {
                    log().finer("COMPTK-2006: NMR poller stopped" + pt.getName());
                }
            }
            catch (Exception in) {
                log().log(Level.WARNING,
                          I18n.loc("COMPTK-6014: Failed to stop {0} NMR poller: {1}", 
                                   pt.getName(), in.getMessage()),
                          in);
            }
        }
        
        // then, kill the threads
        boolean waitOnThreads = true;
        while (waitOnThreads) {
            waitOnThreads = false;
            for (PollerThread pt : getPollers()) {
                if (pt.isAlive()) {
                    waitOnThreads = true;
                    
                    try { pt.join(10); }
                    catch (InterruptedException ie) { /* ignore */ }
                }
            }
        }
    }

    protected void updatePollers(int newThreadCount) {
        Property prop = getContext().getConfiguration()
                .getProperty(AcceptPoller.POLLER_COUNT_PROPERTY);
        Integer newValue = Integer.valueOf(newThreadCount),
                suggested = (Integer) prop.suggestValue(newValue, true);
        if (suggested == null) {    // descriptor was modified to remove default
            suggested = AcceptPoller.DEFAULT_POLLER_COUNT;
        }
        if (!suggested.equals(newValue)) {
            log().info(I18n.loc(
                    "COMPTK-5007: {0} was misconfigured with {1} poller threads - {2} poller threads will be running!", 
                    getComponentName(), newValue, suggested));
            newThreadCount = suggested.intValue();   // should have at least one poller
            prop.setValue(suggested.toString());
        }
        
        boolean logFiner = log().isLoggable(Level.FINER);
        String compName = getContext().getComponentContext().getComponentName();
        String ptBase = compName +"-thread-", apBase = compName +"-poller-";
        
        int oldThreadCount = getPollers().size(), delta = 0;
        for (int i = oldThreadCount; i < newThreadCount; i++) {
            AcceptPoller ap = new AcceptPoller(getContext(), (apBase + String.valueOf(i)));
            PollerThread pt = new PollerThread(ap, (ptBase + String.valueOf(i)));
            pt.start();
            getPollers().add(pt);
            ++delta;
            if (logFiner) {
                log().finer("COMPTK-2005: NMR poller "+ pt.getName() +" started for "+ compName);
            }
        }

        for (int i = oldThreadCount - 1; i >= newThreadCount; i--) {
            PollerThread pt = getPollers().remove(i);
            pt.getPoller().stop();
            --delta;
            if (logFiner) {
                log().finer("COMPTK-2006: NMR poller stopped" + pt.getName());
            }
        }
        
        if (log().isLoggable(Level.CONFIG)) {
            // separate messages so started/stopped can be i18n'd
            if (delta > 0) {
                log().config(I18n.loc(
                        "COMPTK-4004: {0} started {1} NMR poller threads.", 
                        compName, String.valueOf(delta)));
            }
            else {
                log().config(I18n.loc(
                        "COMPTK-4005: {0} stopped {1} NMR poller threads.", 
                        compName, String.valueOf(Math.abs(delta))));
            }
        }
        // TODO Resize the XmlResourceProviderPool with the the new thread count
        try {
            XmlUtil.getXmlResourcePool().resizePool(getPollers().size());
        } 
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Looks up the provisioning endpoint configured to handle the specified message exchange.
     * @param mex A message exchange received from the NMR.
     * @return A provisioning {@link Endpoint} or <code>null</code> if the specified
     *          exchange is in the <code>CONSUMER</code> {@link Role}.
     * @throws JBIException If a provisioning endpoint is not defined.
     */
    protected Endpoint findProvisioningEndpoint(MessageExchange mex) throws JBIException {
        ServiceEndpoint ept = mex.getEndpoint();
        EndpointInfo info = EndpointInfo.valueOf(ept, true);
        // provisioning
        Endpoint endpt = getContext().getEndpointManager().lookupEndpoint(info);  
        
        // verify provisioning endpoint
        if (endpt == null) {
            // don't error when receiving responses from consumed endpoints
            if (mex.getRole().equals(Role.PROVIDER)) {
                String err = I18n.loc(
                        "COMPTK-6030: {0} cannot find provisioning endpoint "+
                        "(name={1},srvc={2}) for message (id={3})", 
                        getContext().getComponentContext().getComponentName(), 
                        ept.getEndpointName(), ept.getServiceName(), mex.getExchangeId());
                log().warning(err);
                throw new JBIException(err);
            }
        }

        return endpt;
    }

    /**
     * Utility method to send a message which consumes a service.
     * @param src The message data.
     * @param operation The operation to invoke.
     * @throws JBIException If an error occurs sending message exchange.
     */
    protected void invoke(MessageExchange mex, Source src, QName operation) throws JBIException {
        // Create new message and populate its content
        NormalizedMessage yIn = mex.createMessage();
        yIn.setContent(src);
        // Set message as "in" reference in exchange 
        mex.setMessage(yIn, "in");  // "in" literal described in JBI spec

        mex.setOperation(operation);
        send(mex);
    }

    /**
     * Utility method to send a {@link MessageExchange} with logging.
     * @param msg The exchange to send.
     * @throws JBIException If an error occurs sending.
     */
    protected void send(MessageExchange msg) throws JBIException {
        // TODO do we validate status/messages/anything?
        try {
            if (log().isLoggable(Level.FINE)) {
                ServiceEndpoint endpt = msg.getEndpoint();
                log().log(Level.FINE, "COMPTK-3013: Sending message exchange (id={0}) to endpoint({1}-{2})",
                          new Object[] { msg.getExchangeId(), endpt.getServiceName(), endpt.getEndpointName() });
                if (log().isLoggable(Level.FINER)) {
                    log().finer("COMPTK-2007: Destination endpoint definition:\n"+ 
                                String.valueOf(endpt));
                }
            }

            getContext().getMessagingChannel().send(msg);
        }
        catch (MessagingException mex) {
            log().log(Level.WARNING,
                      I18n.loc("COMPTK-6028: Failed to send message exchange (id={0}): {1}", 
                               msg.getExchangeId(), mex.getMessage()), 
                      mex);
            throw mex;
        }
    }

    /**
     * Sends a {@link MessageExchange} with {@link ExchangeStatus#ERROR ERROR} status.
     * @param msg The exchange to send.
     * @param error The error, may be <code>null</code>.
     * @throws JBIException If an error occurs sending.
     */
    protected void sendError(MessageExchange msg, Exception error) throws JBIException {
        msg.setStatus(ExchangeStatus.ERROR);
        
        String faultActor = String.valueOf(
                msg.getProperty(ExchangeUtil.FAULTACTOR_PROPERTY_NAME));
        if (error == null) {
            msg.setError(new Exception(I18n.loc(
                    "COMPTK-6028: Failed to send message exchange (id={0}): {1}", 
                    msg.getExchangeId(), faultActor)));
        }
        // systemic: only send java.lang.Exception
        else if (error.getClass().getName().equals("java.lang.Exception")) {
            msg.setError(error);
        }
        else {
            msg.setError(new Exception(error.getMessage(), error));
        }
        
        // ensure error properties are populated
        if (!getComponentName().equals(faultActor)) {
            // properties not set, let's do so now
            ExchangeUtil.setErrorData(msg, 
                                      error.getMessage(), 
                                      FaultCode.Server, 
                                      null, // default message is provided by utility 
                                      getComponentName());
        }
        send(msg);
    }

    /**
     * Utility method to create a {@link MessageExchange}.
     * @param ptrn The exchange pattern, must be {@link ExchangePattern#IN_ONLY}
     *             or {@link ExchangePattern#IN_OUT}.
     * @return A message exchange.
     * @throws MessagingException If creation fails or specified pattern is unsupported.
     */
    protected MessageExchange createExchange(ExchangePattern ptrn) throws MessagingException {
        MessageExchangeFactory factory = getContext().getMessagingChannel().getExchangeFactory(); 
        switch (ptrn) {
            case IN_ONLY: {
                return factory.createInOnlyExchange();
            }
            case IN_OUT: {
                return factory.createInOutExchange();
            }
            default: {
                String err = I18n.loc(
                        "COMPTK-6029: Failed to create exchange - unsupported pattern: {0}", 
                        ptrn);
                log().warning(err);
                throw new MessagingException(err);
            }
        }
    }

    /**
     * Shortcut to fetch the component name.
     * @return The component name.
     */
    protected String getComponentName() {
        return getContext().getComponentContext().getComponentName();
    }

    /**
     * Returns the configured number of NMR pollers for this component.
     * This implementation returns the value of the {@link AcceptPoller#POLLER_COUNT_PROPERTY}
     * if present, or {@link AcceptPoller#DEFAULT_POLLER_COUNT} if absent.
     * 
     * @return the configured number of NMR pollers for this component.
     */
    protected int getPollerCount() {
        String prop = getContext().getConfiguration()
                .getProperty(AcceptPoller.POLLER_COUNT_PROPERTY).getValue();
        return Util.parseInt(prop, AcceptPoller.DEFAULT_POLLER_COUNT.intValue());
    }
    
    /**
     * @param mgrCtx the <code>ManagerContext</code> to set
     */
    protected void setManagerContext(ManagerContext mgrCtx) {
        mMgrCtx = mgrCtx;
    }

    protected Logger log() {
        return mLogger;
    }

    protected List<PollerThread> getPollers() {
        return mPollers;
    }

    protected JBIException error(String msg, Throwable t) {
        if (t == null) {
            log().warning(msg);
            return new JBIException(msg);
        }
        else if (t instanceof JBIException) {
            log().log(Level.WARNING, msg, t);
            return ((JBIException) t);
        }
        else {
            log().log(Level.WARNING, msg, t);
            return new JBIException(msg, t);
        }
    }

    private static class PollerThread extends Thread {
        private AcceptPoller mPoller;
        
        public PollerThread(AcceptPoller ap, String name) {
            super(ap, name);
            mPoller = ap;
        }
        
        public AcceptPoller getPoller() {
            return mPoller;
        }
    }
}
