/*
 * @(#)TypeEditorTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
 * 
 * @since 0.1
 */
public class TypeEditorTest {
    
    private final String classOne = "Alfa";
    
    private final String classTwo = "something.Alfa";
    
    private final String classThree = "org.example.Alfa";
    
    private final String classFour = "com.milanfort.rules4jbi.component.ServiceUnit";

    @Test
    public void getPackageName() {
        assertEquals("", TypeEditor.getPackageName(classOne));
        assertEquals("something", TypeEditor.getPackageName(classTwo));
        assertEquals("org.example", TypeEditor.getPackageName(classThree));
        assertEquals("com.milanfort.rules4jbi.component", TypeEditor.getPackageName(classFour));
    }

    @Test
    public void getClassName() {
        assertEquals("Alfa", TypeEditor.getClassName(classOne));
        assertEquals("Alfa", TypeEditor.getClassName(classTwo));
        assertEquals("Alfa", TypeEditor.getClassName(classThree));
        assertEquals("ServiceUnit", TypeEditor.getClassName(classFour));
    }

    @Test
    public void isClassInDefaultPackage() {
        assertTrue(TypeEditor.isClassInDefaultPackage(classOne));
        
        assertFalse(TypeEditor.isClassInDefaultPackage(classTwo));
        assertFalse(TypeEditor.isClassInDefaultPackage(classThree));
        assertFalse(TypeEditor.isClassInDefaultPackage(classFour));
    }

    @Test
    public void extractPackageName() {
        assertEquals("something", TypeEditor.extractPackageName(classTwo));
        assertEquals("org.example", TypeEditor.extractPackageName(classThree));
        assertEquals("com.milanfort.rules4jbi.component", TypeEditor.extractPackageName(classFour));
    }

    @Test
    public void extractClassName() {
        assertEquals("Alfa", TypeEditor.extractClassName(classTwo));
        assertEquals("Alfa", TypeEditor.extractClassName(classThree));
        assertEquals("ServiceUnit", TypeEditor.extractClassName(classFour));
    }
}
