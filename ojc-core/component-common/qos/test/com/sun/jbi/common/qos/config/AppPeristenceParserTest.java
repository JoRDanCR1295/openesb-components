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
 * @(#)AppPeristenceParserTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.qos.config.AppVar.VarType;

/**
 * Unit test for {@link AppPersistenceParsers}.
 * @author Kevan Simpson
 */
public class AppPeristenceParserTest extends ConfigTestCase {
    /**
     * @param name
     */
    public AppPeristenceParserTest(String name) {
        super(name);
    }

    public void testApplicationPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        populateAppConfigTestData("foo", config);
        populateAppConfigTestData("bar", config);
        populateAppVarTestData("var2", VarType.Number, "123", config);
        populateAppVarTestData("var1", VarType.String, "var1 value", config);
        
        String expected = "<app>"+ getAppConfigXml("bar") + getAppConfigXml("foo") +
                getAppVarXml("var1", VarType.String, "var1 value") + 
                getAppVarXml("var2", VarType.Number, "123") +"</app>";
        String actual = AppPersistenceParser.toAppXml(config);
        getXmlTester().compareXml(expected, actual);
    }

    public void testAppConfigPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        populateAppConfigTestData("foo", config);
        populateAppConfigTestData("bar", config);
        
        String expected = "<app>"+ getAppConfigXml("bar") + getAppConfigXml("foo") +"</app>";
        String actual = AppPersistenceParser.toAppXml(config);
        getXmlTester().compareXml(expected, actual);
    }

    public void testAppVarPersistence() throws Exception {
        ComponentConfig config = ComponentConfig.parse(readInputSource(
                "descriptors", JbiDescriptor.META_INF_DIR, JbiDescriptor.JBI_DESC_FILE_NAME));
        populateAppVarTestData("var2", VarType.Number, "123", config);
        populateAppVarTestData("var1", VarType.String, "var1 value", config);
        
        String expected = "<app>"+ getAppVarXml("var1", VarType.String, "var1 value") + 
                getAppVarXml("var2", VarType.Number, "123") +"</app>";
        String actual = AppPersistenceParser.toAppXml(config);
        getXmlTester().compareXml(expected, actual);
    }
    
    String getAppConfigXml(String name) {
        return "<config name=\""+ name +"\">"+
                "<property name=\"AC1\"><value>foo ac1</value></property>"+
                "<property name=\"AC2\"><value>foo ac2 - 0</value>"+
                "<value>foo ac2 - 1</value><value>foo ac2 - 2</value>"+
                "<value>foo ac2 - 3</value></property></config>";
    }
    String getAppVarXml(String name, VarType type, String value) {
        return "<var name=\""+ name +"\" type=\""+ type +"\">"+ value +"</var>";
    }
}
