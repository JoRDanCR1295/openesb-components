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
 * @(#)ConfigPeristenceTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.qos.config.AppVar.VarType;

/**
 * Unit test for {@link ConfigPersistence}.
 * @author Kevan Simpson
 */
public class ConfigPeristenceTest extends ConfigTestCase {
    /**
     * @param name
     */
    public ConfigPeristenceTest(String name) {
        super(name);
    }

    public void testComponentPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        config.getProperty("ThreadCount").setValue("23");
        config.getProperty("Foo2").addValue("23");
        config.getProperty("Foo2").addValue("24");
        ConfigPersistence.persistConfig(config, resolvePath("descriptors"));
        // reload default values
        ComponentConfig actual = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        assertEquals("wrong default value - ThreadCount", 
                     "10", actual.getProperty("ThreadCount").getValue());
        assertEquals("wrong default value count - Foo2", 
                     1, actual.getProperty("Foo2").count());
        assertEquals("wrong default value - Foo2", 
                     "12", actual.getProperty("Foo2").getValue());
        ConfigPersistence.loadConfig(actual, resolvePath("descriptors"));
        assertEquals("wrong persisted value - ThreadCount", 
                     "23", actual.getProperty("ThreadCount").getValue());
        assertEquals("wrong persisted value count - Foo2", 
                     3, actual.getProperty("Foo2").count());
        assertEquals("wrong persisted value - Foo2", 
                     "12", actual.getProperty("Foo2").getValueAt(0));
        assertEquals("wrong persisted 2nd value - Foo2", 
                     "23", actual.getProperty("Foo2").getValueAt(1));
        assertEquals("wrong persisted 3rd value - Foo2", 
                     "24", actual.getProperty("Foo2").getValueAt(2));
    }

    public void testApplicationPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        // app configs
        runAppConfigAssertions("foo", config, false);
        AppConfig foo = populateAppConfigTestData("foo", config);
        assertNotNull("missing app config", foo);
        assertEquals("erroneous value", 0, foo.getProperty("configurationName").count());
        runAppConfigAssertions("foo", config, true);
        runAppConfigAssertions("bar", config, false);
        populateAppConfigTestData("bar", config);
        runAppConfigAssertions("bar", config, true);
        // app vars
        populateAppVarTestData("var1", VarType.String, "var1 value", config);
        populateAppVarTestData("var2", VarType.Number, "123", config);
        // persist
        ConfigPersistence.persistApplicationConfig(config, resolvePath("descriptors"));
        // reload default values
        ComponentConfig actual = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        runAppConfigAssertions("foo", actual, false);
        runAppConfigAssertions("bar", actual, false);
        runAppVarAssertions("var1", VarType.String, "var1 value", actual, false);
        runAppVarAssertions("var2", VarType.Number, "123", actual, false);
        ConfigPersistence.loadApplicationConfig(actual, resolvePath("descriptors"));
        runAppConfigAssertions("foo", actual, true);
        runAppConfigAssertions("bar", actual, true);
        runAppVarAssertions("var1", VarType.String, "var1 value", actual, true);
        runAppVarAssertions("var2", VarType.Number, "123", actual, true);
    }

    public void testAppConfigPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        // app configs
        runAppConfigAssertions("foo", config, false);
        populateAppConfigTestData("foo", config);
        runAppConfigAssertions("foo", config, true);
        runAppConfigAssertions("bar", config, false);
        populateAppConfigTestData("bar", config);
        runAppConfigAssertions("bar", config, true);
        // persist
        ConfigPersistence.persistApplicationConfig(config, resolvePath("descriptors"));
        // reload default values
        ComponentConfig actual = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        runAppConfigAssertions("foo", actual, false);
        runAppConfigAssertions("bar", actual, false);
        ConfigPersistence.loadApplicationConfig(actual, resolvePath("descriptors"));
        runAppConfigAssertions("foo", actual, true);
        runAppConfigAssertions("bar", actual, true);
    }

    public void testAppVarPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        // app vars
        populateAppVarTestData("var1", VarType.String, "var1 value", config);
        populateAppVarTestData("var2", VarType.Number, "123", config);
        // persist
        ConfigPersistence.persistApplicationConfig(config, resolvePath("descriptors"));
        // reload default values
        ComponentConfig actual = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        runAppVarAssertions("var1", VarType.String, "var1 value", actual, false);
        runAppVarAssertions("var2", VarType.Number, "123", actual, false);
        ConfigPersistence.loadApplicationConfig(actual, resolvePath("descriptors"));
        runAppVarAssertions("var1", VarType.String, "var1 value", actual, true);
        runAppVarAssertions("var2", VarType.Number, "123", actual, true);
    }
}
