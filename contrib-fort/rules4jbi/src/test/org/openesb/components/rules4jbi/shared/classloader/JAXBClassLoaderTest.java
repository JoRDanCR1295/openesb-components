/*
 * @(#)JAXBClassLoaderTest.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
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

package org.openesb.components.rules4jbi.shared.classloader;

import java.io.File;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
 * 
 * @since 0.4
 */
public class JAXBClassLoaderTest {

    private URLClassLoader urlClassLoader;
    
    private JAXBClassLoader jaxbClassLoader;
    
    private Class<?> originalFoo;
    
    private Class<?> originalPerson;
    
    private Class<?> jaxbFoo;
    
    private Class<?> jaxbPerson;
    
    @Before
    public void setUp() throws MalformedURLException, ClassNotFoundException {
        final String testDirectory = System.getProperty("test.dir") + File.separator + "classloader";
        
        File fooBarJar = new File(testDirectory, "foobar.jar");
        File personJar = new File(testDirectory, "person.jar");

        urlClassLoader = new URLClassLoader(new URL[] {fooBarJar.toURI().toURL(), personJar.toURI().toURL()},
                this.getClass().getClassLoader());

        jaxbClassLoader = new JAXBClassLoader(new URL[] {fooBarJar.toURI().toURL(), personJar.toURI().toURL()},
                this.getClass().getClassLoader());
        
        originalFoo = urlClassLoader.loadClass("org.example.Foo");
        originalPerson = urlClassLoader.loadClass("org.example.Person");
        
        jaxbFoo = jaxbClassLoader.loadClass("org.example.Foo");
        jaxbPerson = jaxbClassLoader.loadClass("org.example.Person");
    }

    @Test
    public void classAnnotationsCount() {
        assertEquals(1, originalFoo.getAnnotations().length);
        assertEquals(0, originalPerson.getAnnotations().length);
        assertEquals(1, jaxbFoo.getAnnotations().length);
        assertEquals(3, jaxbPerson.getAnnotations().length);
    }

    @Test
    public void classAnnotationsPresence() {
        assertTrue(originalFoo.isAnnotationPresent(XmlRootElement.class));
        assertFalse(originalFoo.isAnnotationPresent(XmlType.class));
        assertFalse(originalFoo.isAnnotationPresent(XmlAccessorType.class));

        assertFalse(originalPerson.isAnnotationPresent(XmlRootElement.class));
        assertFalse(originalPerson.isAnnotationPresent(XmlType.class));
        assertFalse(originalPerson.isAnnotationPresent(XmlAccessorType.class));
        
        assertTrue(jaxbFoo.isAnnotationPresent(XmlRootElement.class));
        assertFalse(jaxbFoo.isAnnotationPresent(XmlType.class));
        assertFalse(jaxbFoo.isAnnotationPresent(XmlAccessorType.class));
        
        assertTrue(jaxbPerson.isAnnotationPresent(XmlRootElement.class));
        assertTrue(jaxbPerson.isAnnotationPresent(XmlType.class));
        assertTrue(jaxbPerson.isAnnotationPresent(XmlAccessorType.class));
    }
    
    @Test
    public void fieldAnnotationsCount() {
        assertEquals(0, annotatedFieldsCount(originalFoo));
        assertEquals(0, annotatedFieldsCount(originalPerson));
        assertEquals(0, annotatedFieldsCount(jaxbFoo));
        assertEquals(2, annotatedFieldsCount(jaxbPerson));
    }
    
    private int annotatedFieldsCount(Class<?> clazz) {
        final Field[] declaredFields = clazz.getDeclaredFields();
        
        int result = 0;
        
        for (Field field : declaredFields) {
            if (field.getAnnotations().length > 0) {
                result++;
            }
        }
        
        return result;
    }
    
    @Test
    public void fieldAnnotations() throws NoSuchFieldException {
        
        // transient fields should not get annotated
        assertEquals(0, jaxbPerson.getDeclaredField("id").getAnnotations().length);
        
        // static fields should not get annotated either
        assertEquals(0, jaxbPerson.getDeclaredField("count").getAnnotations().length);

        Field ageField = jaxbPerson.getDeclaredField("age");

        assertEquals(1, ageField.getAnnotations().length);

        XmlElement ageAnnotation = ageField.getAnnotation(XmlElement.class);
        
        assertEquals("http://www.example.org/", ageAnnotation.namespace());
        assertEquals(false, ageAnnotation.required());
        
        Field nameField = jaxbPerson.getDeclaredField("name");

        assertEquals(1, nameField.getAnnotations().length);

        XmlElement nameAnnotation = nameField.getAnnotation(XmlElement.class);
        
        assertEquals("http://www.example.org/", nameAnnotation.namespace());
        assertEquals(true, nameAnnotation.required());
    }
}
