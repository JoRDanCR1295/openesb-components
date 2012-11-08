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
 * @(#)SimpleTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.util.Collection;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.xml.WSDLReader;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

import junit.framework.TestCase;

/**
 * Simple test cases to test basic WSDL4J extension features.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public class SimpleTest extends TestCase {

    protected static final String SIMPLE_WSDL = "test/data/simple/simple.wsdl";
    protected static final String WSDL_TARGET_NS =
        "http://ojc.java.net/wsdl4jext/unittest/Simple";
    protected static final String XSD_TARGET_NS =
        "http://ojc.java.net/wsdl4jext/unittest/Simple/XSD";
    
    protected WSDLReader _wsdlReader;
    
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        
        _wsdlReader = WSDL4JExt.newWSDLReader(null);
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Tests loading a simple WSDL.
     */
    public void testReadWSDL() {
        try {
            Definition wsdlDef = _wsdlReader.readWSDL(SIMPLE_WSDL);
            assertNotNull(wsdlDef);
        } catch (WSDLException e) {
            fail(e.toString());
        }
    }
    
    /**
     * Tests XML schema components lookup.
     */
    public void testSchemaComponentsLookup() {
        try {
            Definition wsdlDef = _wsdlReader.readWSDL(SIMPLE_WSDL);
            SchemaTypeLoader loader = WSDL4JExt.getSchemaTypeLoader(wsdlDef);
            assertNotNull(loader);
            SchemaType simpleType =
                loader.findType(new QName(XSD_TARGET_NS, "stateCode"));
            assertNotNull(simpleType);
            SchemaType complexType =
                loader.findType(new QName(XSD_TARGET_NS, "complexType1"));
            assertNotNull(complexType);
            SchemaGlobalElement elem = 
                loader.findElement(new QName(XSD_TARGET_NS, "root"));
            assertNotNull(elem);
            simpleType = loader.findType(
                    new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string"));
            assertNotNull(simpleType);
            elem = loader.findElement(
                    new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "schema"));
            assertNotNull(elem);
        } catch (WSDLException e) {
            fail(e.toString());
        }
    }
    
    public void testBPELExtensibilityElementsLookup() {
        try {
            //Part link type lookup
            Definition wsdlDef = _wsdlReader.readWSDL(SIMPLE_WSDL);
            Collection<PartnerLinkType> plTypes =
                WSDL4JExt.getPartnerLinkTypes(wsdlDef);
            assertEquals(2, plTypes.size());
            PartnerLinkType plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(WSDL_TARGET_NS, "Simple1"));
            assertNotNull(plt);
            plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(WSDL_TARGET_NS, "partnerlinktype1"));
            assertNotNull(plt);
            assertNotNull(plt.getRole("role1"));
            assertNotNull(plt.getRole("role1").getPort());
            
            //Message property lookup
            Collection<MessageProperty> props =
                WSDL4JExt.getMessageProperties(wsdlDef);
            assertEquals(2, props.size());
            MessageProperty prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(WSDL_TARGET_NS, "stateCodeProp"));
            assertNotNull(prop);
            prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(WSDL_TARGET_NS, "dummyProp"));
            assertNotNull(prop);
            
            //Property alias lookup
            Collection<MessagePropertyAlias> propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef);
            assertEquals(3, propAliases.size());
            propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef,
                        new QName(WSDL_TARGET_NS, "stateCodeProp"));
            assertEquals(3, propAliases.size());
            for (MessagePropertyAlias alias : propAliases) {
                if (alias.getMessageType() != null) {
                    assertNull(alias.getElementName());
                    assertNull(alias.getTypeName());
                } else if (alias.getElementName() != null) {
                    assertNull(alias.getMessageType());
                    assertNull(alias.getTypeName());
                } else if (alias.getTypeName() != null) {
                    assertNull(alias.getElementName());
                    assertNull(alias.getMessageType());
                } else {
                    fail("One of message type, element or"
                            + "XML type must be defined.");
                }
            }
        } catch (WSDLException e) {
            fail(e.toString());
        }
    }
}
