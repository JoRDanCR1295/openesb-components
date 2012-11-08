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
 * ServiceEngineExampleSpringTest.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.example;

import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContext;

import junit.framework.TestCase;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.xml.XmlBeanFactory;

import org.springframework.core.io.ClassPathResource;

import javax.jbi.messaging.MessageExchange;

import javax.xml.namespace.QName;


/**
 * An example of how to use the Spring Framework to inject the Mock JBI classes
 * into a JBI Component to write Unit tests for a JBI Component.
 * Uses a configuration xml file rather than having to do it programmatically.
 * This is the preferred method of injection.
 */
public class ServiceEngineExampleSpringTest extends TestCase {
    public void testServiceEngineSend() throws Exception {
        ServiceEngineExample se = new ServiceEngineExample();

        ClassPathResource resource = new ClassPathResource("/spring-basic.xml");
        BeanFactory beanFactory = new XmlBeanFactory(resource);

        // Cast to the Mock interface and never the concrete Basic class
        MockComponentContext context = (MockComponentContext) beanFactory
            .getBean("componentContext");
        MessageExchange me = (MessageExchange) beanFactory.getBean(
                "messageExchange");

        assertNotNull("Unable to get an instance of ComponentContext", context);
        assertNotNull("Unable to get an instance of ServiceEndpoint",
            context.getEndpoints(new QName("localPart")));
        assertNotNull("Unable to get an instance of DeliveryChannel",
            context.getDeliveryChannel());
        assertNotNull("Unable to get an instance of MessageExchange", me);

        se.init(context);
        se.handleMessageExchange(me);
    }
}
