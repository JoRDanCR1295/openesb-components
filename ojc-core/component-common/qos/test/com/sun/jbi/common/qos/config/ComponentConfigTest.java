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
 * @(#)ComponentConfigTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.Properties;

import com.sun.jbi.common.descriptor.JbiDescriptor;

/**
 * 
 * @author Kevan Simpson
 */
public class ComponentConfigTest extends ConfigTestCase {
    /**
     * @param name
     */
    public ComponentConfigTest(String name) {
        super(name);
    }

    public void testPropertyParsing() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        assertEquals("Wrong name!", "sun-xslt-engine", config.getName());
        runPropertyAssertions("ThreadCount", "10", 0, config);
        runPropertyAssertions("Foo1", "11", 0, config);
        runPropertyAssertions("Foo2", "12", 0, config);
    }
    
    public void testConfigToProperties() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        assertEquals("Wrong name 1", "sun-xslt-engine", config.getName());
        assertEquals("Foo2 property count wrong!", 1, config.getProperty("Foo2").count());
        config.getProperty("Foo2").addValue("2nd value");
        assertEquals("Foo2 property count wrong!", 2, config.getProperty("Foo2").count());
        Properties props = ConfigPersistence.toProperties(config);
        ComponentConfig actual = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        assertEquals("Wrong name w", "sun-xslt-engine", config.getName());
        ConfigPersistence.toConfig(actual, props);
        runPropertyAssertions("ThreadCount", "10", 0, actual);
        runPropertyAssertions("Foo1", "11", 0, actual);
        assertEquals("Foo2 property count wrong!", 2, actual.getProperty("Foo2").count());
        runPropertyAssertions("Foo2", "12", 0, actual);
        runPropertyAssertions("Foo2", "2nd value", 1, actual);
    }
}
