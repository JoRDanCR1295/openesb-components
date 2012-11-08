/*
 * @(#)Rules4JBIBootstrapTest.java        $Revision: 1.1 $ $Date: 2008/11/15 01:22:24 $
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

package org.openesb.components.rules4jbi.engine.component;

import java.io.File;

import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;

import org.junit.Test;

//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/15 01:22:24 $
 * 
 * @since 0.3
 */
public class Rules4JBIBootstrapTest {

    @Test
    public void getConfigurationValue() throws Exception {
        String testDirectory = System.getProperty("test.dir");
        
        File file = new File(testDirectory, "configuration.xml");
        
        final Builder builder = new Builder();
        Document document = builder.build(file);
        
        Element configurationElement = document.getRootElement();
        
        assertEquals(17, Rules4JBIBootstrap.getConfigurationValue(configurationElement, "PoolSize", -1));
        
        assertEquals(10, Rules4JBIBootstrap.getConfigurationValue(configurationElement, "MaxServiceUnits", -1));
        
        assertEquals(-1, Rules4JBIBootstrap.getConfigurationValue(configurationElement, "HttpDefaultPort", 80));
        
        assertEquals(12, Rules4JBIBootstrap.getConfigurationValue(configurationElement, "Foo", 12));
    }
}
