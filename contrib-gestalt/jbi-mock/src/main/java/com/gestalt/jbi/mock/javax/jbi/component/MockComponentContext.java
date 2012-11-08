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
 * MockComponentContext.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.component;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;


/**
 * Top level Mock interface that all Mock ComponentContext
 * classes should implement.
 * All the non-jbi methods should be defined here, in order
 * for unit tests to properly cast the object and have
 * access to our new methods. Otherwise the unit tests
 * would have to cast down to the mock implementation.
 * <p>Say for example we added a new method called
 * setOutMessage to MockComponentContextBasic.
 * Then in our unit test we wanted to get a ComponentContext
 * object from the DI container,
 * ComponentContext context =
 *      (ComponentContext) container.getBean("componentContext");
 * Doing this does not allow us access to the new setOutMessage() method.
 * In order to get access we would have to down cast by saying
 * MockComponentContextBasic context =
 *      (MockComponentContextBasic) container.getBean("componentContext");
 * We want to avoid having to downcast to a concrete class.
 */
public interface MockComponentContext extends ComponentContext {
    /**
     * Sets the DeliveryChannel of the ComponentContext that is returned
     * when components call ComponentContext.getDeliveryChannel();
     * @param deliveryChannel The mock DeliveryChannel
     */
    public void setDeliveryChannel(DeliveryChannel deliveryChannel);

    /**
     * Sets the MBeanNames of the ComponentContext that is returned
     * when components call ComponentContext.getMBeanNames();
     * @param mbeanNames The mock MBeanNames
     */
    public void setMBeanNames(MBeanNames mbeanNames);

    /**
     * Sets the ServiceEndpoints of the ComponentContext that is returned
     * when components call
     * ComponentContext.getEndpoint(),
     * ComponentContext.getEndpoints(),
     * ComponentContext.getEndpointsForService(),
     * ComponentContext.getExternalEndpoints(), or
     * ComponentContext.getExternalEndpointsForService().
     * @param serviceEndpoints The mock ServiceEndpoint array
     */
    public void setEndpoints(ServiceEndpoint[] serviceEndpoints);
}
