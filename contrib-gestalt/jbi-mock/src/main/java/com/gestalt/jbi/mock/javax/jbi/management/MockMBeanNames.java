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
 * MockMBeanNames.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.management;

import javax.jbi.management.MBeanNames;


/**
 * Top level Mock interface that all Mock MBeanNames
 * classes should implement.
 * All the non-jbi methods should be defined here, in order
 * for unit tests to properly cast the object and have
 * access to our new methods. Otherwise the unit tests
 * would have to cast down to the mock implementation.
 * <p>Say for example we added a new method called
 * setMBeanKey to MockMBeanNamesBasic.
 * Then in our unit test we wanted to get a MBeanNames
 * object from the DI container,
 * MBeanNames mbeanNames =
 *      (MBeanNames) container.getBean("mbeanNames");
 * Doing this does not allow us access to the new setMBeanKey() method.
 * In order to get access we would have to down cast by saying
 * MockMBeanNamesBasic mbeanNames =
 *      (MockMBeanNamesBasic) container.getBean("mbeanNames");
 * We want to avoid having to downcast to a concrete class.
 */
public interface MockMBeanNames extends MBeanNames {
    public void setJmxDomainName(String jmxDomainName);

    public void setMBeanKey(String mbeanKey);
}
