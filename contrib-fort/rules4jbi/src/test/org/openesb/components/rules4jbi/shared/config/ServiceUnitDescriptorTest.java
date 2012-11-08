/*
 * @(#)ServiceUnitDescriptorTest.java        $Revision: 1.2 $ $Date: 2008/07/03 05:46:04 $
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
import javax.xml.namespace.QName;
import nu.xom.Element;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/03 05:46:04 $
 * 
 * @since 0.1
 */
public class ServiceUnitDescriptorTest {

    @Test
    public void createDescriptor() {
        String expected = "<jbi version='1.0' xmlns='http://java.sun.com/xml/ns/jbi' "
                + "xmlns:ns1='http://www.example.com/xml/ns/descriptor'>"
                + "<services binding-component='false'>"
                + "<provides interface-name='ns1:TestPortType' "
                + "service-name='ns1:TestService' "
                + "endpoint-name='TestPort'/></services></jbi>";
        
        Element result = ServiceUnitDescriptor.createDescriptor("http://www.example.com/xml/ns/descriptor",
                                    "TestPortType", "TestService", "TestPort");
        
        assertEquals(XOMUtils.toElement(expected), result);
    }
    
    @Test
    public void load() throws FileNotFoundException, InvalidServiceUnitDescriptorException {
        String testDirectory = System.getProperty("test.dir");
        
        File file1 = new File(testDirectory, "descriptor1.xml");
        
        ServiceUnitDescriptor descriptor1 = ServiceUnitDescriptor.load(new FileInputStream(file1));
        
        assertEquals(new QName("http://www.example.com/xml/ns/descriptor", "TestPortType"), 
                descriptor1.getInterfaceName());
        
        assertEquals(new QName("http://www.example.com/xml/ns/descriptor", "TestService"),
                descriptor1.getServiceName());
        
        assertEquals("TestPort", descriptor1.getEndpointName());
        
        
        File file2 = new File(testDirectory, "descriptor2.xml");
        
        ServiceUnitDescriptor descriptor2 = ServiceUnitDescriptor.load(new FileInputStream(file2));
        
        assertEquals(new QName("http://abc.com/jbi", "hello"), descriptor2.getInterfaceName());
        
        assertEquals(new QName("http://abc.com/jbi", "helloService"), descriptor2.getServiceName());
        
        assertEquals("helloendpoint", descriptor2.getEndpointName());
    }
    
    @Test(expected=InvalidServiceUnitDescriptorException.class)
    public void loadException() throws FileNotFoundException, InvalidServiceUnitDescriptorException {
        String testDirectory = System.getProperty("test.dir");
        
        File file3 = new File(testDirectory, "descriptor3.xml");
        
        ServiceUnitDescriptor descriptor3 = ServiceUnitDescriptor.load(new FileInputStream(file3));
        
        assertTrue(true);
    }
}
