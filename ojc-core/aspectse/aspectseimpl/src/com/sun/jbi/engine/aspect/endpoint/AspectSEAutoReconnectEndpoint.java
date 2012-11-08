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
 * @(#)AspectSEAutoReconnectEndpoint.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;

import com.sun.jbi.engine.aspect.endpoint.handler.AspectSEAutoReconnectEndpointHandler;
import com.sun.jbi.engine.aspect.endpoint.support.AutoReconnectHelper;
import com.sun.jbi.engine.aspect.endpoint.handler.AspectConstants;
import com.sun.jbi.engine.aspect.endpoint.mbean.AspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.AutoReconnectAspectSEEndpointConfiguration;
import com.sun.jbi.engine.aspect.endpoint.mbean.AutoReconnectAspectSEEndpointConfigurationMbean;
import javax.jbi.messaging.MessagingException;

/**
 *
 *
 *
 * In a request/reply scenario, if the service provider is unavailable, one has
 * to block the client until the service is up again and a response is received.
 * The Auto-Reconnect Aspect would keep trying to make the call to the
 * destination every configurable seconds for another configurable amount of
 * time until the service provider becomes available again or the maximum number
 * of retries is reached based on the configuration.
 * <p>
 * This pattern ensures the Retry policy acts between the client and the service
 * that is being invoked. Therefore, if this aspect is configured, all messages
 * flowing between the client and the service provider flows through the Retry
 * engine core.
 *
 * <p>
 * Composite Apps requiring use of this aspect will have to employ the <b>Filter
 * Request/Reply</b> exchange pattern.
 * <p>
 * The Retry Aspect works on idempotent operations that do not need to go back
 * to the client for conflict resolution
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEAutoReconnectEndpoint extends AspectSEEndpoint {

    private Logger logger = Logger.getLogger(AspectSEAutoReconnectEndpoint.class.getName());
    /* rate at which reconnect has to be carried out */
    private String rate="5000";

    /* no of times reconnect has to be tried */
    private String timeout="10";

    /* Class that helps in carrying out reconnection*/
    private AutoReconnectHelper reconnectHelper;

    public AspectSEAutoReconnectEndpoint(EndpointInfo info) {
        super(info);
        setHandler(new AspectSEAutoReconnectEndpointHandler(this));
        reconnectHelper = new AutoReconnectHelper(this);
    }



    @Override
    public void init() {
        getHandler().parse();
    }

    @Override
    public void handleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
    throws JBIException {
        // TODO Auto-generated method stub
        logger.log(Level.CONFIG, "Reconnect Rate:" + rate);
        logger.log(Level.CONFIG, "Reconnect Timeout:" + timeout);
        reconnectHelper.invokeThroughReconnector(inOut, ctx);
    }

    @Override
    public void handleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
    throws JBIException {
        // TODO Auto-generated method stub
        logger.log(Level.CONFIG, "Reconnect Rate:" + rate);
        logger.log(Level.CONFIG, "Reconnect Timeout:" + timeout);
        reconnectHelper.invokeThroughReconnector(inOnly, ctx);
    }

    /**
     * Callback method used by the helperclass to call invoke method.
     *
     * @param CRLInOut inOut ME
     * @param ExchangeContext ctx
     * @throws JBIException
     *
     */
    public void callbackHandleFilterInvoke(CRLInOut inOut, ExchangeContext ctx)
    throws JBIException {
        super.handleFilterInvoke(inOut, ctx);
    }

    /**
     * Callback method used by the helperclass to call invoke method.
     *
     * @param CRLInOnly inOnly ME
     * @param ExchangeContext ctx
     * @throws JBIException
     *
     */
    public void callbackHandleFilterInvoke(CRLInOnly inOnly, ExchangeContext ctx)
    throws JBIException {
        super.handleFilterInvoke(inOnly, ctx);
    }

    @Override
    public void handleFilterResponse(CRLInOut inOut, CRLInOut originalInOut,
            ExchangeContext ctx) throws JBIException {
        reconnectHelper.handleResponse(originalInOut);
        super.handleFilterResponse(inOut, originalInOut, ctx);
    }

    @Override
    public void handleFault(CRLInOut inOut, CRLInOut originalInOut)
    throws JBIException {
        if(reconnectHelper.handleFault(originalInOut)) {
            super.handleFault(inOut, originalInOut);
        }
    }

    @Override
    public void handleStatus(CRLInOnly inOnly, CRLInOnly originalInOnly)
    throws MessagingException, JBIException {
        if(reconnectHelper.handleStatus(originalInOnly)) {
            super.handleStatus(inOnly, originalInOnly);
        }
    }

    @Override
    public void shutdown() throws InstanceNotFoundException, MBeanRegistrationException {
    	super.shutdown();
    	reconnectHelper.cleanup();

    }

    @Override
    public void registerMbean() {
        try {
            AspectSEEndpointConfiguration mbean = new AutoReconnectAspectSEEndpointConfiguration(
                    timeout, rate);
            mbean.addNotificationListener(this, null, null);
            super.registerMbean(mbean, "AutoReconnect");
        } catch (NotCompliantMBeanException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void handleNotification(Notification notification, Object handback) {
        String attributeName = null;
        AttributeChangeNotification attributeNotification = null;
        String attributeValue = null;
        if (notification instanceof AttributeChangeNotification) {
            attributeNotification = (AttributeChangeNotification) notification;

            // Check if configuration change is for Cache SE component
            if (attributeNotification.getSource() instanceof AutoReconnectAspectSEEndpointConfigurationMbean) {
                attributeName = attributeNotification.getAttributeName();
                if (true == attributeName.equals(AspectConstants.RETRY_PROP_RATE)) {
                    attributeValue = (String) attributeNotification
                            .getNewValue();
                    setRate(attributeValue);
                }
                if (true == attributeName.equals(AspectConstants.RETRY_PROP_TIMEOUT)) {
                    attributeValue = (String) attributeNotification
                            .getNewValue();
                    setTimeout(attributeValue);
                }
                //persist the changes to the corresponding config file
                save();
            }
        }
    }

    /**
     * Getter method for retry rate property.
     *
     */
    public String getRate() {
        return String.valueOf(rate);
    }

    /**
     * Getter method for retry timeout property.
     *
     */
    public String getTimeout() {
        return String.valueOf(timeout);
    }

    /**
     * Setter method for retry rate property.
     * @param retryRate
     */
    public void setRate(String retryRate) {
        rate = retryRate;
    }

    /**
     * Setter method for retry timeout property.
     * @param retryTimeout
     */
    public void setTimeout(String retryTimeout) {
        timeout = retryTimeout;
    }
}
