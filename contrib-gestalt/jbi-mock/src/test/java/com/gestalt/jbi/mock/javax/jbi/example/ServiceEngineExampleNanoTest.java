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
 * ServiceEngineExampleNanoTest.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.example;

import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContext;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannel;
import com.gestalt.jbi.mock.javax.jbi.servicedesc.MockServiceEndpoint;
import junit.framework.TestCase;
import org.nanocontainer.script.ScriptedContainerBuilder;
import org.nanocontainer.script.xml.XMLContainerBuilder;
import org.picocontainer.PicoContainer;
import org.picocontainer.defaults.ObjectReference;
import org.picocontainer.defaults.SimpleReference;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URL;


/**
 * An example of how to use the Nano Container to inject the Mock JBI classes
 * into a JBI Component to write Unit tests for a JBI Component.
 * Uses a configuration xml file rather than having to do it programmatically.
 */
public class ServiceEngineExampleNanoTest extends TestCase {
    public void testServiceEngineSend() throws Exception {
        ServiceEngineExample se = new ServiceEngineExample();

        URL url = this.getClass().getResource("/nanocontainer.xml");
        File file = new File(url.getFile());
        assertTrue("nanocontainer.xml does not exist", file.exists());

        Reader script = new FileReader(file);
        XMLContainerBuilder builder = new XMLContainerBuilder(script,
                getClass().getClassLoader());
        PicoContainer pico = buildContainer(builder, null, "SOME_SCOPE");

        // Cast to the Mock interface and never the concrete Basic class
        MockComponentContext context = (MockComponentContext) pico
            .getComponentInstanceOfType(ComponentContext.class);
        MockServiceEndpoint serviceEndpoint = (MockServiceEndpoint) pico
            .getComponentInstanceOfType(ServiceEndpoint.class);
        MockDeliveryChannel channel = (MockDeliveryChannel) pico
            .getComponentInstanceOfType(DeliveryChannel.class);
        MessageExchange me = (MessageExchange) pico.getComponentInstanceOfType(MessageExchange.class);

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

    /**
     * Put this in a common class and do not mass copy this to each
     * test.
     */
    public PicoContainer buildContainer(ScriptedContainerBuilder builder,
        PicoContainer parentContainer, Object scope) {
        ObjectReference containerRef = new SimpleReference();
        ObjectReference parentContainerRef = new SimpleReference();

        parentContainerRef.set(parentContainer);
        builder.buildContainer(containerRef, parentContainerRef, scope, true);

        return (PicoContainer) containerRef.get();
    }
}
