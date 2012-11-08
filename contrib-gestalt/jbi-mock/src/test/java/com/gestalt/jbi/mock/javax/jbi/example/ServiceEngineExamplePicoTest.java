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
 * ServiceEngineExamplePicoTest.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.example;

import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContext;
import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContextBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannel;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannelBasic;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockInOutBasic;
import com.gestalt.jbi.mock.javax.jbi.servicedesc.MockServiceEndpoint;
import com.gestalt.jbi.mock.javax.jbi.servicedesc.MockServiceEndpointBasic;

import junit.framework.TestCase;

import org.picocontainer.MutablePicoContainer;

import org.picocontainer.defaults.DefaultPicoContainer;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;


/**
 * An example of how to use the Pico Container to inject the Mock JBI classes
 * into a JBI Component to write Unit tests for a JBI Component.
 */
public class ServiceEngineExamplePicoTest extends TestCase {
    public void testServiceEngineSend() throws Exception {
        ServiceEngineExample se = new ServiceEngineExample();

        MutablePicoContainer pico = new DefaultPicoContainer();
        pico.registerComponentImplementation(ComponentContext.class,
            MockComponentContextBasic.class);

        /**
         * Could have used the ServiceMix ServiceEndpoint implementation
         * see ServiceEngineExampleManualTest.java for an example
         */
        pico.registerComponentImplementation(ServiceEndpoint.class,
            MockServiceEndpointBasic.class);
        pico.registerComponentImplementation(DeliveryChannel.class,
            MockDeliveryChannelBasic.class);
        pico.registerComponentImplementation(MessageExchange.class,
            MockInOutBasic.class);

        // Cast to the Mock interface and never the concrete Basic class
        MockComponentContext context = (MockComponentContext) pico
            .getComponentInstance(ComponentContext.class);
        MockServiceEndpoint serviceEndpoint = (MockServiceEndpoint) pico
            .getComponentInstanceOfType(ServiceEndpoint.class);
        MockDeliveryChannel channel = (MockDeliveryChannel) pico
            .getComponentInstance(DeliveryChannel.class);
        MessageExchange me = (MessageExchange) pico.getComponentInstance(MessageExchange.class);

        assertNotNull("Unable to get an instance of ComponentContext", context);
        assertNotNull("Unable to get an instance of ServiceEndpoint",
            serviceEndpoint);
        assertNotNull("Unable to get an instance of DeliveryChannel", channel);
        assertNotNull("Unable to get an instance of MessageExchange", me);

        context.setDeliveryChannel(channel);
        context.setEndpoints(new ServiceEndpoint[] { serviceEndpoint });

        se.init(context);
        se.handleMessageExchange(me);
    }
}
