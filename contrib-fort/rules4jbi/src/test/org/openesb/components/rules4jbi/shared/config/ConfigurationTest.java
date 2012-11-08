/*
 * @(#)ConfigurationTest.java        $Revision: 1.2 $ $Date: 2008/07/03 05:46:05 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.config;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/03 05:46:05 $
 * 
 * @since 0.1
 */
public class ConfigurationTest {

    private Configuration configuration;
    
    @Before
    public void setUp() {
        configuration = new Configuration();
        
        configuration.setRuleServiceProvider("org.jcp.jsr94.jess");
        configuration.setRuleServiceProviderClass("org.jcp.jsr94.jess.RuleServiceProviderImpl");
        configuration.setRulesetFile("data/ruleset.xml");
        configuration.setWSDLFile("data/rules.wsdl");
        
        List<String> classes = new ArrayList<String>();
        classes.add(Integer.class.getName());
        classes.add(Double.class.getName());
        configuration.setClasses(classes);
    }
    
    private void testConfiguration(Configuration config) {
        assertEquals("org.jcp.jsr94.jess", config.getRuleServiceProvider());
        assertEquals("org.jcp.jsr94.jess.RuleServiceProviderImpl", config.getRuleServiceProviderClass());
        assertEquals("data/ruleset.xml", config.getRulesetFile());
        assertEquals("data/rules.wsdl", config.getWSDLFile());

        List<String> classes = config.getClasses();
        assertEquals(2, classes.size());
        assertEquals("java.lang.Integer", classes.get(0));
        assertEquals("java.lang.Double", classes.get(1));
    }
    
    @Test
    public void gettersAndSetters() {
        testConfiguration(configuration);
    }
    
    @Test
    public void load() throws FileNotFoundException, InvalidConfigurationException {
        String testDirectory = System.getProperty("test.dir");
        File file = new File(testDirectory, "config.xml");

        Configuration config = Configuration.load(new FileInputStream(file));
        
        testConfiguration(config);
    }

    @Test
    public void save() throws FileNotFoundException, SaveFailedException, InvalidConfigurationException {
        /* We assume here, that load() already works properly */

        String testDirectory = System.getProperty("test.dir");
        File file = new File(testDirectory, "config-tmp.xml");
        configuration.save(new FileOutputStream(file));
        
        Configuration loadedConfig = Configuration.load(new FileInputStream(file));
        testConfiguration(loadedConfig);
        
        file.delete();
    }

    @Test
    public void toXML() {
        String expected = "<config xmlns='http://www.milanfort.com/xml/ns/jbi/rules/configuration'>"
                + "<rule-service-provider>org.jcp.jsr94.jess</rule-service-provider>"
                + "<rule-service-provider-class>org.jcp.jsr94.jess.RuleServiceProviderImpl" 
                + "</rule-service-provider-class>"
                + "<ruleset-file>data/ruleset.xml</ruleset-file>"
                + "<wsdl-file>data/rules.wsdl</wsdl-file>"
                + "<classes>"
                + "<class-name>java.lang.Integer</class-name>" 
                + "<class-name>java.lang.Double</class-name>"
                + "</classes>"
                + "</config>";
        
        assertEquals(XOMUtils.toElement(expected), XOMUtils.toElement(configuration.toXML(false)));
    }
}
