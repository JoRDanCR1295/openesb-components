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

import com.sun.jbi.common.qos.config.AppVar.VarType;
import com.sun.jbi.common.util.test.UtilTestCase;

/**
 * Unit test for {@link ConfigPersistence}.
 * @author Kevan Simpson
 */
public abstract class ConfigTestCase extends UtilTestCase {
    public ConfigTestCase(String name) {
        super(name);
    }
    
    protected AppConfig populateAppConfigTestData(String acName, ComponentConfig config) {
        config.putAppConfig(new AppConfig(acName, config.getAppConfigDefs()));
        AppConfig foo = config.getAppConfig(acName);
        foo.getProperty("AC1").setValue("foo ac1");
        foo.getProperty("AC2").addValue("foo ac2 - 0");
        foo.getProperty("AC2").addValue("foo ac2 - 1");
        foo.getProperty("AC2").addValue("foo ac2 - 2");
        foo.getProperty("AC2").addValue("foo ac2 - 3");
        return foo;
    }

    protected AppVar populateAppVarTestData(String name, VarType type, String value, ComponentConfig config) {
        config.putAppVar(new AppVar(name, value, type));
        return config.getAppVar(name);
    }
    
    protected void runAppConfigAssertions(String acName, ComponentConfig config, boolean expected) throws Exception {
        if (expected) {
            AppConfig foo = config.getAppConfig(acName);
            assertNotNull("missing app config: "+ acName, foo);
            assertEquals("wrong AC1 value count - "+ acName, 1, foo.getProperty("AC1").count());
            assertEquals("wrong AC1 value - "+ acName, "foo ac1", foo.getProperty("AC1").getValue());
            for (int i = 0; i < 4; i++) {
                assertEquals("wrong AC2."+ i +" value - "+ acName, "foo ac2 - "+ i, foo.getProperty("AC2").getValueAt(i));
            }
        }
        else {
            assertNull("unexpected app config: "+ acName, config.getAppConfig(acName));
        }
    }

    protected void runAppVarAssertions(String varName, VarType type, String value,
                                       ComponentConfig config, boolean expected) throws Exception {
        if (expected) {
            AppVar var = config.getAppVar(varName);
            assertNotNull("missing app var: "+ varName, var);
            assertEquals("wrong app var type: "+ varName, type, var.getType());
            assertEquals("wrong app var value: "+ varName, value, var.getValue());
        }
        else {
            assertNull("unexpected app var: "+ varName, config.getAppVar(varName));
        }
    }

    protected void runPropertyAssertions(String name, String value, int index,
                                         ComponentConfig config) throws Exception {
        Property p = config.getProperty(name);
        assertNotNull("Null Property for "+ name, p);
        assertEquals("Prop Name is wrong "+ name, name, p.getName());
        assertEquals("Property Value is wrong for "+ name, value, p.getValueAt(index));
    }
}
