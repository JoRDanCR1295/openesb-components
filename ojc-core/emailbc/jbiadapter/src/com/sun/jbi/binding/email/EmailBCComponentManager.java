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
 * @(#)EmailBCComponentManager.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email;

import javax.jbi.JBIException;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import javax.jbi.component.ComponentContext;

/**
 * Email Binding Component implementation.
 * 
 * @author CDK
 */
public class EmailBCComponentManager extends AbstractComponentManager {

    @Override
    public void init(ComponentContext ctx) throws JBIException {
        super.init(ctx);
        I18n.setAlerter(getAlerter());
        //I18n.fine(log(), "EMAILBC-1001: Initializing Email Binding Component...");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
    @Override
    public void start() throws JBIException {
        I18n.fine(log(), "EMAILBC-1002: Starting Email Binding Component...");

        // starts NMR polling threads
        super.start();

//        String info = I18n.loc("EMAILBC-5001: Started Email Binding Component successfully!");
        //log().info(info);
//        getAlerter().info(info, getComponentContext().getComponentName(),
//                null, null, ComponentType.BindingComponent,
//                OperationalState.STARTED, "EMAILBC-5001");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
    @Override
    public void stop() throws JBIException {

        I18n.fine(log(), "EMAILBC-1003: Stopping Email Binding Component...");

        // stops NMR polling threads
        super.stop();

//        String info = I18n.loc("EMAILBC-5003: Stopped Email Binding Component successfully!");
//        log().info(info);
//        getAlerter().info(info, getComponentContext().getComponentName(),
//                null, null, ComponentType.BindingComponent,
//                OperationalState.STOPPED, "EMAILBC-5003");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
    @Override
    public void shutDown() throws JBIException {
        I18n.fine(log(), "EMAILBC-1004: Shutting down Email Binding Component...");


        super.shutDown();

//        String info = I18n.loc("EMAILBC-5004: Shut down Email Binding Component successfully!");
//        log().info(info);
//        getAlerter().info(info, getComponentContext().getComponentName(),
//                null, null, ComponentType.BindingComponent,
//                OperationalState.SHUTDOWN, "EMAILBC-5004");
        I18n.setAlerter(null);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new EmailBCEndpointManager(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new EmailBCExchangeHandler(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    @Override
    protected Object initRuntimeMBean() throws JBIException {
        return new EmailBCConfiguration(getComponentContext(), getConfig());
    }
}
