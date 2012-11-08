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
 * BeanContainerSpringImpl.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.dmit.common.injection.spring.impl;

import com.gestalt.dmit.common.injection.BeanContainer;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.xml.XmlBeanFactory;

import org.springframework.core.io.ClassPathResource;


/**
 * Spring Framework implementation of the BeanContainer.
 * Currently takes a XML Configuration File as a parameter
 * to initialize its container. For an example see the
 * /test/unit/resources/spring-test.xml file.
 */
public class BeanContainerSpringImpl implements BeanContainer {
    private BeanFactory beanFactory;

    public BeanContainerSpringImpl(String xmlConfigFile) {
        ClassPathResource resource = new ClassPathResource(xmlConfigFile);
        beanFactory = new XmlBeanFactory(resource);
    }

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
    public Object getBean(String id) {
        return beanFactory.getBean(id);
    }

    public BeanFactory getBeanFactory() {
        return beanFactory;
    }

    public void setBeanFactory(BeanFactory beanFactory) {
        this.beanFactory = beanFactory;
    }
}
