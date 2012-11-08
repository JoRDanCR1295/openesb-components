/*
 * @(#)SelfFirstClassLoaderTest.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
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
public class SelfFirstClassLoaderTest {

    private URLClassLoader parent;
    
    private URLClassLoader delegatingChild;
    
    private SelfFirstClassLoader selfFirstChild;
    
    @Before
    public void setUp() throws MalformedURLException {
        final String testDirectory = System.getProperty("test.dir") + File.separator + "classloader";
        
        File fooBarJar = new File(testDirectory, "foobar.jar");
        File fooBazJar = new File(testDirectory, "foobaz.jar");
        File personJar = new File(testDirectory, "person.jar");
        
        parent = new URLClassLoader(new URL[] {fooBarJar.toURI().toURL(), personJar.toURI().toURL()});
        
        delegatingChild = new URLClassLoader(new URL[] {fooBazJar.toURI().toURL()}, parent);
        
        selfFirstChild = new SelfFirstClassLoader(new URL[] {fooBazJar.toURI().toURL()}, parent);
    }

    @Test
    public void loadClass() throws Exception {
        assertClassHasField("bar", parent.loadClass("org.example.Foo"));
        
        assertClassHasField("bar", delegatingChild.loadClass("org.example.Foo"));
        
        assertClassHasField("baz", selfFirstChild.loadClass("org.example.Foo"));
    }
    
    private void assertClassHasField(final String fieldName, final Class<?> clazz) {
        Field[] declaredFields = clazz.getDeclaredFields();
        
        assertEquals(1, declaredFields.length);
        
        assertEquals(fieldName, declaredFields[0].getName());
    }
    
    @Test
    public void loadClassFromParent() throws ClassNotFoundException {
        Class<?> personClass = selfFirstChild.loadClass("org.example.Person");
        
        /* This will always succeed; previous call would throw CNFE if unsuccessful */
        assertEquals("org.example.Person", personClass.getName());
        
        Class<?> listClass = selfFirstChild.loadClass("java.util.List");
        
        assertEquals("java.util.List", listClass.getName());
    }
}
