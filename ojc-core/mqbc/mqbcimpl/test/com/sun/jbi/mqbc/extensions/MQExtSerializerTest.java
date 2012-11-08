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
 * @(#)MQExtSerializerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import junit.framework.*;
import java.io.Serializable;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.logging.Level;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.StringUtils;
import com.ibm.wsdl.util.xml.DOMUtils;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.xml.sax.EntityResolver;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.internationalization.Messages;


import com.sun.jbi.mqbc.extensions.MQExtensionRegistry;

/**
 *
 * @author rchen
 */
public class MQExtSerializerTest extends TestCase {
    
    public MQExtSerializerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(MQExtSerializerTest.class);
        
        return suite;
    }

    /**
     * Test of registerSerializer method, of class com.sun.jbi.mqbc.extensions.MQExtSerializer.
     */
    public void testRegisterSerializer() {
        System.out.println("registerSerializer");
        java.util.Map envVarsMap = new HashMap();
        ExtensionRegistry registry = new MQExtensionRegistry(envVarsMap);
        MQExtSerializer instance = new MQExtSerializer();
        
        instance.registerSerializer(registry);
            
    }

    /**
     * Test of marshall method, of class com.sun.jbi.mqbc.extensions.MQExtSerializer.
     */
    public void testMarshall() throws Exception {
        System.out.println("marshall");
        
        Class parentType = null;
        QName elementType = null;
        ExtensibilityElement extension = null;
        PrintWriter pw = null;
        Definition def = null;
        ExtensionRegistry extReg = null;
        MQExtSerializer instance = new MQExtSerializer();
        
        instance.marshall(parentType, elementType, extension, pw, def, extReg);
        
       
    }

    /**
     * Test of unmarshall method, of class com.sun.jbi.mqbc.extensions.MQExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        System.out.println("unmarshall");
        
        Class parentType = null;
        QName elementType = null;
        Element el = null;
        Definition def = null;
        ExtensionRegistry extReg = null;
        MQExtSerializer instance = new MQExtSerializer();
        
        ExtensibilityElement expResult = null;
        ExtensibilityElement result = instance.unmarshall(parentType, elementType, el, def, extReg);
        assertEquals(expResult, result);
        
        
    }
    
}
