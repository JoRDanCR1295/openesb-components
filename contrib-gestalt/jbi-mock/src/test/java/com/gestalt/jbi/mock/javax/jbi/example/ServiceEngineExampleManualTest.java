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
 * ServiceEngineExampleManualTest.java - ver 1.0 - 2006
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

import junit.framework.TestCase;

import org.apache.servicemix.jbi.framework.ComponentNameSpace;
import org.apache.servicemix.jbi.servicedesc.InternalEndpoint;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.namespace.QName;


/**
 * An example of how to manually inject the Mock JBI classes
 * into a JBI Component to write Unit tests for a JBI Component.
 */
public class ServiceEngineExampleManualTest extends TestCase {
    public void testServiceEngine() throws Exception {
        ServiceEngineExample se = new ServiceEngineExample();

        MockComponentContext context = new MockComponentContextBasic();
        MockDeliveryChannel channel = new MockDeliveryChannelBasic();
        MessageExchange me = new MockInOutBasic();

        // Example of how to use existing ServiceEndpoint implementations
        // Could have used MockServiceEndpointBasic as well
        ServiceEndpoint[] serviceEndpoints = {
            new InternalEndpoint(new ComponentNameSpace("container1",
                    "component1"), "endpoint1", new QName("service1"))
        };

        context.setDeliveryChannel(channel);
        context.setEndpoints(serviceEndpoints);

        se.init(context);
        se.handleMessageExchange(me);
    }
}
