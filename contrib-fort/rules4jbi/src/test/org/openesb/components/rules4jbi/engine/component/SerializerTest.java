/*
 * @(#)SerializerTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import nu.xom.Element;
import org.junit.Before;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

@XmlRootElement(name = "user", namespace = "http://whatever")
@XmlType(namespace = "http://whatever")
@XmlAccessorType(XmlAccessType.FIELD)
class Foo {
    
    @XmlElement(namespace="http://whatever")
    int value;

    public Foo() {}
    
    public Foo(int value) {
        this.value = value;
    }
}

@XmlRootElement(name = "client", namespace = "http://whatnot")
@XmlType(namespace = "http://whatnot")
@XmlAccessorType(XmlAccessType.FIELD)
class Bar {
    @XmlAttribute(namespace="http://whatnot")
    String name;
    @XmlElement(namespace="http://whatnot")
    int age;
    
    public Bar() {}

    public Bar(String name, int age) {
        this.name = name;
        this.age = age;
    }
}

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class SerializerTest {

    private Foo foo = null;
    
    private Bar bar = null;
    
    private Serializer serializer = null;
    
    
    
    @Before
    public void setUp() {
        foo = new Foo(12);
        bar = new Bar("Joe", 77);
        
        serializer = new Serializer(new Class<?>[] {Foo.class, Bar.class});
//        serializer = new Serializer();
    }
    
    @Test
    public void serialize() {
        String expected = "<user xmlns='http://whatever'><value>12</value></user>";
        Element result = serializer.serialize(foo);
        assertEquals(XOMUtils.toElement(expected), result);

        expected = "<ns1:client xmlns:ns1='http://whatnot' ns1:name='Joe'><ns1:age>77</ns1:age></ns1:client>";
        result = serializer.serialize(bar);
        assertEquals(XOMUtils.toElement(expected), result);
    }

    @Test(expected=IllegalArgumentException.class)
    public void serializeException() {
        Element result = serializer.serialize(new StringBuilder("nothing"));
    }
    
    @Test
    public void deserialize() {
        String xmlString = "<user xmlns='http://whatever'><value>12</value></user>";
        Object result = serializer.deserialize(XOMUtils.toElement(xmlString));
        assertTrue(result instanceof Foo);
        assertEquals(12, ((Foo) result).value);
        
        xmlString = "<ns1:client xmlns:ns1='http://whatnot' ns1:name='Joe'><ns1:age>77</ns1:age></ns1:client>";
        result = serializer.deserialize(XOMUtils.toElement(xmlString));
        assertTrue(result instanceof Bar);
        assertEquals("Joe", ((Bar) result).name);
        assertEquals(77, ((Bar) result).age);
    }
}
