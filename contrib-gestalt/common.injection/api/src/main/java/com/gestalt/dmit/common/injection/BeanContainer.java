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
 * BeanContainer.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.dmit.common.injection;


/**
 * Interface encapsulating the common and different ways
 * each Dependency Injection container provides methods
 * to retreive Objects, or in this case Beans.
 * The names used here are modeled after the spring framework,
 * but that doesn't mean you can't use other DI containers.
 * This interface is used to abstract the different DI containers
 * such as Pico, Nano, and Spring allowing developers to use any of the
 * three and not be tied at compile time to a specific one.
 * This interface can easily be extended to support other DI containers.
 */
public interface BeanContainer {
    /**
     * Asks the container to create or retreive the following
     * object based on the ID attribute in the XML configuration file.
     * For example, if you have a spring xml file with the following
     * content:
     * <bean id="myString" class="java.lang.String"/>
     * you would call this method with the following id:
     * String s = (String) getBean("myString");
     * <p>The ID concept is only used by the Spring Framework.</p>
     * @param id The String ID in the XML config file used to reference
     * this object. For example id="myString"
     * @return The object defined by the class attribute.
     */
    public Object getBean(String id);

    /**
     * A Null BeanContainer which is used when access to a real
     * implementation is not available and the developer wants to
     * avoid seeing java.lang.NullPointerExceptions.
     * Currently used by the BeanContainerFactory when it is unable
     * to find or create a implementation. Instead of returning a null,
     * it returns this object to avoid NPEs.
     */
    public static class NullBeanContainer implements BeanContainer {
        public Object getBean(String id) {
            throw new java.lang.UnsupportedOperationException();
        }
    }
}
