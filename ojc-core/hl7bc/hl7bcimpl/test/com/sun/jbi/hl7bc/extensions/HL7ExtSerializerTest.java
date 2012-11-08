/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HL7ExtSerializerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import junit.framework.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.io.PrintWriter;
import java.util.HashMap;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.util.StringUtils;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;


/**
 *
 * @author Raghunadh
 */
public class HL7ExtSerializerTest extends TestCase {
 
	public HL7ExtSerializerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HL7ExtSerializerTest.class);
        
        return suite;
    }

	  /**
     * Test of registerSerializer method, of class com.sun.jbi.mqbc.extensions.MQExtSerializer.
     */
    public void testRegisterSerializer() {
        System.out.println("Testing registerSerializer");
        java.util.Map envVarsMap = new HashMap();
        ExtensionRegistry registry = new HL7ExtensionRegistry(envVarsMap);
        HL7ExtSerializer instance = new HL7ExtSerializer();
        
        instance.registerSerializer(registry);
            
    }

	    /**
     * Test of marshall method, of class com.sun.jbi.hl7bc.extensions.HL7ExtSerializer.
     */
    public void testMarshall() throws Exception {
        System.out.println("Testing marshall");
        
        Class parentType = null;
        QName elementType = null;
        ExtensibilityElement extension = null;
        PrintWriter pw = null;
        Definition def = null;
        ExtensionRegistry extReg = null;
        HL7ExtSerializer instance = new HL7ExtSerializer();
        
        instance.marshall(parentType, elementType, extension, pw, def, extReg);
        
     
        
        System.out.println("Successfully tested marshal");
    }

	 /**
     * Test of unmarshall method, of class com.sun.jbi.hl7bc.extensions.HL7ExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        System.out.println("Testing unmarshall");
        
        Class parentType = null;
        QName elementType = null;
        Element el = null;
        Definition def = null;
        ExtensionRegistry extReg = null;
        HL7ExtSerializer instance = new HL7ExtSerializer();
        
        ExtensibilityElement expResult = null;
        ExtensibilityElement result = instance.unmarshall(parentType, elementType, el, def, extReg);
        assertEquals(expResult, result);
    }


	

   
}
