/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */


package org.netbeans.modules.jbi.apisupport.common;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import junit.framework.*;

/**
 *
 * @author chikkala
 */
public class JBIDescriptorFactoryTest extends TestCase {
    
    public JBIDescriptorFactoryTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    private void compIdentificationChangeTest(JbiComponentDescriptor compDesc, String newName, String newDesc)
    throws Exception {
        compDesc.setName(newName);
        compDesc.setDescription(newDesc);
        StringWriter writer = new StringWriter();
        DOMUtil.UTIL.DOM2Text(compDesc.getDOMDocument(), writer);
        StringReader reader = new StringReader(writer.getBuffer().toString());
        JbiComponentDescriptor newCompDesc = JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(reader);
        this.assertEquals("Expected new Name in Component Identification  ",
                newName, newCompDesc.getName());
        this.assertEquals("Expected new Description in Component Identification  ",
                newDesc , newCompDesc.getDescription());
        
    }
    
    public void testServiceEngineDescriptor() throws Exception {
        InputStream inS = this.getClass().getResourceAsStream("se_jbi.xml");
        if ( inS == null ) {
            this.fail("se_jbi.xml InputStream NULL");
        }
        InputStreamReader reader = new InputStreamReader(inS);
        JbiComponentDescriptor compDesc = JbiDescriptorFactory.getInstance().createServiceEngineDescriptor(reader);
        this.assertEquals("Expected Service Engine type from the descriptor ",
                JbiDescriptor.SERVICE_ENGINE_TYPE, compDesc.getType());
        
        String newName = "NewServiceEngine";
        String newDesc = "This is the new Description of the New Service Engine";
        compIdentificationChangeTest(compDesc, newName, newDesc);
    }
    
    public void testBindingComponentDescriptor() throws Exception {
        
        InputStream inS = this.getClass().getResourceAsStream("bc_jbi.xml");
        if ( inS == null ) {
            this.fail("bc_jbi.xml InputStream NULL");
        }
        
        InputStreamReader reader = new InputStreamReader(inS);
        JbiComponentDescriptor compDesc = JbiDescriptorFactory.getInstance().createBindingComponentDescriptor(reader);
        this.assertEquals("Expected Binding Component type from the descriptor ",
                JbiDescriptor.BINDING_COMPONENT_TYPE, compDesc.getType());
        
        String newName = "NewBindingComponent";
        String newDesc = "This is the new Description of the New Binding Component";
        compIdentificationChangeTest(compDesc, newName, newDesc);
        
    }
    
    public void testComponentDescriptorEditing() throws Exception {
        InputStream inS = this.getClass().getResourceAsStream("se_jbi.xml");
        if ( inS == null ) {
            this.fail("se_jbi.xml InputStream NULL");
        }
        InputStreamReader reader = new InputStreamReader(inS);
        JbiComponentDescriptor compDesc = JbiDescriptorFactory.getInstance().createServiceEngineDescriptor(reader);
        this.assertEquals("Expected Service Engine type from the descriptor ",
                JbiDescriptor.SERVICE_ENGINE_TYPE, compDesc.getType());
        
        String newName = "NewServiceEngine";
        String newDesc = "This is the new Description of the New Service Engine";
        
        String compClassName = "com.sun.jbi.test.ComponentRuntime";
        String[] compClassPath = {"lib/my.jar", "lib/your.jar", "lib/others.jar"};
        
        String bootstrapClassName = "com.sun.jbi.test.Installer";
        String[] bootstrapClassPath = {"lib/my.jar", "lib/your.jar"};
        
        compDesc.setName(newName);
        compDesc.setDescription(newDesc);
        
        compDesc.setComponentClassName(compClassName);
        compDesc.setComponentClassPath(compClassPath);
        
        compDesc.setBootstrapClassName(bootstrapClassName);
        compDesc.setBootstrapClassPath(bootstrapClassPath);
        
        StringWriter writer = new StringWriter();
        DOMUtil.UTIL.DOM2Text(compDesc.getDOMDocument(), writer);
        StringReader newReader = new StringReader(writer.getBuffer().toString());
        JbiComponentDescriptor newCompDesc = JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(newReader);
        
        System.out.println("############# modified jbi.xml BEGINE ###############");
        System.out.println(writer.getBuffer().toString());
        System.out.println("############# modified jbi.xml END ###############");
        
        this.assertEquals("Expected new Name in Component Identification  ",
                newName, newCompDesc.getName());
        this.assertEquals("Expected new Description in Component Identification  ",
                newDesc , newCompDesc.getDescription());

        this.assertEquals("Expected new Component Class Name ",
                compClassName, newCompDesc.getComponentClassName());        
        this.assertEquals("Expected 3 Component ClassPath Elements ",
                compClassPath.length , newCompDesc.getComponentClassPath().length);
        
        this.assertEquals("Expected new Bootstrap Class Name ",
                bootstrapClassName, newCompDesc.getBootstrapClassName());        
        this.assertEquals("Expected 2 Component ClassPath Elements ",
                bootstrapClassPath.length , newCompDesc.getBootstrapClassPath().length);
        
    }    
}
