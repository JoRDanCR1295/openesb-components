/*
 * @(#)NamespacePrefixGeneratorTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.schema;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.xml.namespace.QName;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class NamespacePrefixGeneratorTest {
    
    /*
     * ns1="http://www.example.org/abc"
     * ns2="http://www.example.org/def"
     * 
     * ns1:car
     * ns1:car
     * ns1:user
     * ns2:user
     * ns1:department
     * ns1:department
     * not registered
     */
    private QName element1 = new QName("http://www.example.org/abc", "car", "ns1");

    private QName element2 = new QName("http://www.example.org/abc", "car", "ns2");
    
    private QName element3 = new QName("http://www.example.org/abc", "user", "ns1");
    
    private QName element4 = new QName("http://www.example.org/def", "user", "ns1");
    
    private QName element5 = new QName("http://www.example.org/abc", "department", "ns2");
    
    private QName element6 = new QName("http://www.example.org/abc", "department", "ns2");
    
    private QName element7 = new QName("http://www.example.org/def", "department", "ns1");
    
    private NamespacePrefixGenerator namespacePrefixGenerator;

    @Before
    public void setUp() {
        Set<QName> elements = new LinkedHashSet<QName>();
        elements.add(element1);
        elements.add(element2);
        elements.add(element3);
        elements.add(element4);
        elements.add(element5);
        elements.add(element6);
        // we do not register element7
        
        namespacePrefixGenerator = new NamespacePrefixGenerator(elements);
    }

    @Test
    public void testSize() {
        assertEquals(2, namespacePrefixGenerator.registeredNamespaces());
    }
    
    @Test
    public void getPrefixedLocalName() throws InterruptedException {
        assertEquals("ns1:car", namespacePrefixGenerator.getPrefixedLocalName(element1));
        assertEquals("ns1:car", namespacePrefixGenerator.getPrefixedLocalName(element2));
        assertEquals("ns1:user", namespacePrefixGenerator.getPrefixedLocalName(element3));
        assertEquals("ns2:user", namespacePrefixGenerator.getPrefixedLocalName(element4));
        assertEquals("ns1:department", namespacePrefixGenerator.getPrefixedLocalName(element5));
        assertEquals("ns1:department", namespacePrefixGenerator.getPrefixedLocalName(element6));
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void unregisteredPrefixedLocalNameTest() {
        assertEquals("ns1:car", namespacePrefixGenerator.getPrefixedLocalName(element1));
        assertEquals("ns1:car", namespacePrefixGenerator.getPrefixedLocalName(element2));
        
        assertEquals("ns2:department", namespacePrefixGenerator.getPrefixedLocalName(element7));
    }
    
    @Test
    public void getPrefixes() {
        Map<String, String> prefixes = namespacePrefixGenerator.getPrefixes();
        
        assertEquals(2, prefixes.size());

        Set<String> keySet = prefixes.keySet();
        
        assertFalse(keySet.contains("ns0"));
        assertTrue(keySet.contains("ns1"));
        assertTrue(keySet.contains("ns2"));
        assertFalse(keySet.contains("ns3"));
        
        assertEquals("http://www.example.org/abc", prefixes.get("ns1"));
        assertEquals("http://www.example.org/def", prefixes.get("ns2"));
    }
    
    @Test
    public void getNamespaces() {
        List<String> namespaces =  namespacePrefixGenerator.getNamespaces();
        
        assertEquals(2, namespaces.size());

        assertEquals("http://www.example.org/abc", namespaces.get(0));
        assertEquals("http://www.example.org/def", namespaces.get(1));
    }
}
 