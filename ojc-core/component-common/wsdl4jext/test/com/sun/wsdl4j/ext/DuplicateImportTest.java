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
 * @(#)ImportTest.java 
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
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.wsdl4j.ext.bpel.MessageProperty;
import com.sun.wsdl4j.ext.bpel.MessagePropertyAlias;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

import junit.framework.TestCase;

/**
 * WSDL and XSD import test cases that involve same namespace from different
 * resources.
 *  
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public class DuplicateImportTest extends TestCase {

    protected static final String TOP_WSDL =
        "test/data/duplicateimport/top.wsdl";
    protected static final String TOP_WSDL_TARGET_NS =
        "http://ojc.java.net/wsdl4jext/unittest/simpleimport";
    protected static final String IMPORTED_WSDL_TARGET_NS =
        "http://ojc.java.net/wsdl4jext/unittest/Imported";
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
            Definition wsdlDef = _wsdlReader.readWSDL(TOP_WSDL);
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
            Definition wsdlDef = _wsdlReader.readWSDL(TOP_WSDL);
            SchemaTypeLoader loader = WSDL4JExt.getSchemaTypeLoader(wsdlDef);
            assertNotNull(loader);
            
            //From directed imported xsd
            SchemaType simpleType =
                loader.findType(new QName(XSD_TARGET_NS, "stateCode"));
            assertNotNull(simpleType);
            SchemaType complexType =
                loader.findType(new QName(XSD_TARGET_NS, "complexType1"));
            assertNotNull(complexType);
            SchemaGlobalElement elem = 
                loader.findElement(new QName(XSD_TARGET_NS, "root"));
            assertNotNull(elem);
            
            //From imported WSDL's inline schema
            simpleType =
                loader.findType(new QName(XSD_TARGET_NS, "inlinedStateCode"));
            assertNotNull(simpleType);
            complexType =
                loader.findType(new QName(XSD_TARGET_NS, "inlinedComplexType1"));
            assertNotNull(complexType);
            elem = 
                loader.findElement(new QName(XSD_TARGET_NS, "inlinedRoot"));
            assertNotNull(elem);
            
            simpleType =
                loader.findType(new QName(XSD_TARGET_NS, "inlinedStateCode_2"));
            assertNotNull(simpleType);
            complexType =
                loader.findType(new QName(XSD_TARGET_NS, "inlinedComplexType1_2"));
            assertNotNull(complexType);
            elem = 
                loader.findElement(new QName(XSD_TARGET_NS, "inlinedRoot_2"));
            assertNotNull(elem);
            
            //From imported XSD imported by an inline schema
            simpleType =
                loader.findType(new QName(XSD_TARGET_NS, "stateCode_2"));
            assertNotNull(simpleType);
            complexType =
                loader.findType(new QName(XSD_TARGET_NS, "complexType1_2"));
            assertNotNull(complexType);
            elem = 
                loader.findElement(new QName(XSD_TARGET_NS, "root_2"));
            assertNotNull(elem);
        } catch (WSDLException e) {
            fail(e.toString());
        }
    }
    
    public void testBPELExtensibilityElementsLookup() {
        try {
            //Part link type lookup
            Definition wsdlDef = _wsdlReader.readWSDL(TOP_WSDL);
            Collection<PartnerLinkType> plTypes =
                WSDL4JExt.getPartnerLinkTypes(wsdlDef);
            assertEquals(5, plTypes.size());
            PartnerLinkType plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "Simple1"));
            assertNotNull(plt);
            plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "partnerlinktype1"));
            assertNotNull(plt);
            assertNotNull(plt.getRole("role1"));
            assertNotNull(plt.getRole("role1").getPort());
            plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "Simple1_2"));
            assertNotNull(plt);
            plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "partnerlinktype1_2"));
            assertNotNull(plt);
            assertNotNull(plt.getRole("role1"));
            assertNotNull(plt.getRole("role1").getPort());
            plt =
                WSDL4JExt.getPartnerLinkType(wsdlDef,
                        new QName(TOP_WSDL_TARGET_NS, "top1"));
            assertNotNull(plt);

            //Message property lookup
            Collection<MessageProperty> props =
                WSDL4JExt.getMessageProperties(wsdlDef);
            assertEquals(4, props.size());
            MessageProperty prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "stateCodeProp"));
            assertNotNull(prop);
            prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "dummyProp"));
            assertNotNull(prop);
            prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "stateCodeProp_2"));
            assertNotNull(prop);
            prop =
                WSDL4JExt.getMessageProperty(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "dummyProp_2"));
            assertNotNull(prop);
            
            //Property alias lookup
            Collection<MessagePropertyAlias> propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef);
            assertEquals(6, propAliases.size());
            propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "stateCodeProp"));
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
            propAliases =
                WSDL4JExt.getMessagePropertyAliases(wsdlDef,
                        new QName(IMPORTED_WSDL_TARGET_NS, "stateCodeProp_2"));
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
