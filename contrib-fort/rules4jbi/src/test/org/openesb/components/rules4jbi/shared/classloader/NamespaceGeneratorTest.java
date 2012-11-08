/*
 * @(#)NamespaceGeneratorTest.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
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

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:42 $
 * 
 * @since 0.4
 */
public class NamespaceGeneratorTest {

    @Test
    public void namespaceForClass() {
        assertEquals("urn:package:default", NamespaceGenerator.namespaceForClass("ClassInDefaultPackage"));
        
        assertEquals("urn:package:java.lang", NamespaceGenerator.namespaceForClass("java/lang/String"));
        assertEquals("urn:package:java.lang", NamespaceGenerator.namespaceForClass("java/lang/Object"));
        
        assertEquals("urn:package:whatever", NamespaceGenerator.namespaceForClass("whatever/Foo"));
        assertEquals("urn:package:whatever.something",
                NamespaceGenerator.namespaceForClass("whatever/something/Bar"));
        assertEquals("urn:package:whatever.something.somethingelse",
                NamespaceGenerator.namespaceForClass("whatever/something/somethingelse/Bar"));
        
        assertEquals("urn:package:com", NamespaceGenerator.namespaceForClass("com/Foo"));
        
        assertEquals("http://www.abc.com/", NamespaceGenerator.namespaceForClass("com/abc/Foo"));
        assertEquals("http://www.example.org/", NamespaceGenerator.namespaceForClass("org/example/Bar"));
        
        assertEquals("http://www.abc.com/project", NamespaceGenerator.namespaceForClass("com/abc/project/Foo"));
        assertEquals("http://www.abc.de/project", NamespaceGenerator.namespaceForClass("de/abc/project/Bar"));
        
        assertEquals("http://www.abc.com/project/core",
                NamespaceGenerator.namespaceForClass("com/abc/project/core/Foo"));
        assertEquals("http://www.abc.com/project/core",
                NamespaceGenerator.namespaceForClass("com/abc/project/core/FooBar"));
    }
}
