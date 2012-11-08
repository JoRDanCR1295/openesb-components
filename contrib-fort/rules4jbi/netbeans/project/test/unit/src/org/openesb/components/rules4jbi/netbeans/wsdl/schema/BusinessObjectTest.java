/*
 * @(#)BusinessObjectTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;
import org.junit.Test;
import static org.junit.Assert.*;

//no no-arg constructor, no namespace
@XmlRootElement
class Foo {
    private String ignore;

    public Foo(String ignore) {
        this.ignore = ignore;
    }
}

//no xmlRootElement annotation, no namespace
class Bar {
    public Bar() {}

}

//no namespace
@XmlRootElement
class FooBar {
    public FooBar() {}
}

@XmlRootElement(name="user", namespace="http://whatever")
@XmlType(namespace="http://whatever")
class Correct {
    public Correct() {}
}

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class BusinessObjectTest {

    @Test
    public void containsXmlRootElementAnnotation() {
        assertEquals(true, BusinessObject.containsXmlRootElementAnnotation(Foo.class));
        assertEquals(false, BusinessObject.containsXmlRootElementAnnotation(Bar.class));
        assertEquals(true, BusinessObject.containsXmlRootElementAnnotation(FooBar.class));
        assertEquals(true, BusinessObject.containsXmlRootElementAnnotation(Correct.class));
    }

    @Test
    public void containsNoArgConstructor() {
        assertEquals(false, BusinessObject.containsNoArgConstructor(Foo.class));
        assertEquals(true, BusinessObject.containsNoArgConstructor(Bar.class));
        assertEquals(true, BusinessObject.containsNoArgConstructor(FooBar.class));
        assertEquals(true, BusinessObject.containsNoArgConstructor(Correct.class));
    }
    
    @Test
    public void newInstance() {
        assertTrue(BusinessObject.newInstance(Bar.class) instanceof Bar);
        assertTrue(BusinessObject.newInstance(FooBar.class) instanceof FooBar);
        assertTrue(BusinessObject.newInstance(Correct.class) instanceof Correct);
    }
    
    @Test
    public void getElementName() {
        assertEquals(new QName("http://whatever", "user"), BusinessObject.getElementName(Correct.class));
    }
    
    @Test
    public void belongsToNamespace() {
        assertEquals(false, BusinessObject.belongsToNamespace(FooBar.class));
        assertEquals(true, BusinessObject.belongsToNamespace(Correct.class));
    }
}
