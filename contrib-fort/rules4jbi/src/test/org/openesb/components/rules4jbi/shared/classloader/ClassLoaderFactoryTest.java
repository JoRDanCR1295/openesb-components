/*
 * @(#)ClassLoaderFactoryTest.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
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
public class ClassLoaderFactoryTest {
    
    private File libDirectory = null;

    @Before
    public void setUp() {
        String testDirectory = System.getProperty("test.dir");

        libDirectory = new File(testDirectory, "lib");
    }

    @Test()
    public void createServiceUnitClassLoader() {
        ClassLoader result = ClassLoaderFactory.createServiceUnitClassLoader(
                libDirectory, libDirectory, libDirectory, ClassLoader.getSystemClassLoader());
        
        assertTrue(result instanceof JAXBClassLoader);
        
        result = ClassLoaderFactory.createServiceUnitClassLoader(
                libDirectory, null, libDirectory, ClassLoader.getSystemClassLoader());

        assertTrue(result instanceof SelfFirstClassLoader);

        result = ClassLoaderFactory.createServiceUnitClassLoader(
                libDirectory, null, null, ClassLoader.getSystemClassLoader());

        assertTrue(result instanceof SelfFirstClassLoader);
    }

    @Test(expected=BusinessObjectsNotFoundException.class)
    public void createServiceUnitClassLoaderException() {
        
        ClassLoaderFactory.createServiceUnitClassLoader(
                null, null, libDirectory, ClassLoader.getSystemClassLoader());
    }

    @Test
    public void isNonEmptyDirectory() {
        assertFalse(ClassLoaderFactory.isNonEmptyDirectory(null));

        File nonexistent = new File(libDirectory, "nonexistent");
        assertFalse(ClassLoaderFactory.isNonEmptyDirectory(nonexistent));
        
        File foo = new File(libDirectory, "foo.jar");
        assertFalse(ClassLoaderFactory.isNonEmptyDirectory(foo));
        
        assertTrue(ClassLoaderFactory.isNonEmptyDirectory(libDirectory));
    }
    
    @Test
    public void prepend() {
        File foo = new File(libDirectory, "foo.jar");

        File bar = new File(libDirectory, "bar.jar");

        File foobar = new File(libDirectory, "foobar.jar");

        File test = new File(libDirectory, "test.dat");
        
        File[] testArray = new File[3];
        
        testArray[0] = bar;
        testArray[1] = foobar;
        testArray[2] = test;

        File[] result = ClassLoaderFactory.prepend(foo, testArray);
        
        assertEquals(4, result.length);

        assertEquals(foo, result[0]);
        assertEquals(bar, result[1]);
        assertEquals(foobar, result[2]);
        assertEquals(test, result[3]);
    }
}
