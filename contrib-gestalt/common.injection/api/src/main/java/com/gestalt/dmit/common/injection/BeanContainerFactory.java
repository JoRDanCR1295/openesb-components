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
 * BeanContainerFactory.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.dmit.common.injection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.lang.reflect.Constructor;


/**
 * Dependency Injection Factory hiding which
 * DI container the developer chooses to use, such as
 * Pico, Nano, or Spring.
 * The default implementation currently returned is Spring.
 */
public class BeanContainerFactory {
    private static Log log = LogFactory.getLog(BeanContainerFactory.class);
    private static final String DEFAULT_IMPL =
        "com.gestalt.dmit.common.injection."
        + "spring.impl.BeanContainerSpringImpl";

    /**
     * Allows you to specify a XML configuration file such as the
     * ones used with Nano or Spring.
     * For example, Spring allows you to specify a XML configuration file.
     * There XmlBeanFactory class can be initialized
     * from a spring-basic.xml (name is not significant) file which is
     * expected to be in your classpath.
     * There are two times you might want this to be in your classpath. One
     * when your actual code wants to use the common injection API and the
     * other is when your tests want to utilize the API.
     * If you are using maven, this is a snippet of what your project.xml
     * file would look like to include it in your code's classpath verses your
     * testing classpath.
     * <build>
     *      <sourceDirectory>src/main/java</sourceDirectory>
     *      <nagEmailAddress>turbine-maven-dev@jakarta.apache.org</nagEmailAddress>
     *      <defaultGoal>jar:install</defaultGoal>
     *      <unitTestSourceDirectory>src/test/unit/src</unitTestSourceDirectory>
     *      <unitTest>
     *          <includes>
     *              <include>*Test.java</include>
     *          </includes>
     *      </unitTest>
     *      <resources>
     *          <resource>
     *              <directory>src/test/unit/resources</directory>
     *                  <includes>
     *                      <include>spring-basic.xml</include>
     *                  </includes>
     *           </resource>
     *       </resources>
     * </build>
     * Notice the resources tag is outside the unitTest tag. This means the xml
     * file will be included in the artifact that is produced.
     * <p>
     * If your tests want to use the common injection API, then all you need to
     * do is include the resources tag inside the unitTest tag.
     * <build>
     *      <sourceDirectory>src/main/java</sourceDirectory>
     *      <nagEmailAddress>turbine-maven-dev@jakarta.apache.org</nagEmailAddress>
     *      <defaultGoal>jar:install</defaultGoal>
     *      <unitTestSourceDirectory>src/test/unit/src</unitTestSourceDirectory>
     *      <unitTest>
     *          <includes>
     *              <include>*Test.java</include>
     *          </includes>
     *          <resources>
     *              <resource>
     *                  <directory>src/test/unit/resources</directory>
     *                      <includes>
     *                          <include>spring-basic.xml</include>
     *                      </includes>
     *              </resource>
     *          </resources>
     *      </unitTest>
     * </build>
     * Here maven will only include the resources in your tests classpath
     * and not include the resources in the artifact. This will copy the
     * spring-basic.xml file to the target folder. Then the value
     * you pass into the createBeanContainer method would be /spring-basic.xml.
     * </p>
     *
     * @param xmlConfigFile The name of the xml configuration file
     * such as /spring-basic.xml.
     * @return BeanContainer A BeanContainer implementation or when an
     * Exception occurs it returns a BeanContainer.NullBeanContainer.
     */
    public static BeanContainer createBeanContainer(String xmlConfigFile) {
        try {
            Class impl = Class.forName(DEFAULT_IMPL);
            Constructor ctor = impl.getDeclaredConstructor(new Class[] {
                        String.class
                    });

            return (BeanContainer) ctor.newInstance(new Object[] { xmlConfigFile });
        } catch (Exception e) {
            log.error("Unable to load bean container", e);

            return new BeanContainer.NullBeanContainer();
        }
    }
}
