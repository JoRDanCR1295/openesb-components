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
 * @(#)InlineSchemasTest.java 
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
 * Test cases to test WSDL4J extension for a WSDL with multiple inline
 * schemas that import each other.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class InlineSchemasTest extends TestCase {

    protected static final String A_WSDL = "test/data/inlineschemas/ShipmentResource.wsdl";
    protected static final String WSDL_TARGET_NS = "http://shipping.tracking";
    protected static final String NS_BF_2 =
        "http://docs.oasis-open.org/wsrf/bf-2";
    protected static final String ADDRESSING_NS =
        "http://www.w3.org/2005/08/addressing";
    
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
            Definition wsdlDef = _wsdlReader.readWSDL(A_WSDL);
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
            Definition wsdlDef = _wsdlReader.readWSDL(A_WSDL);
            SchemaTypeLoader loader = WSDL4JExt.getSchemaTypeLoader(wsdlDef);
            assertNotNull(loader);
            SchemaType complexType =
                loader.findType(new QName(NS_BF_2, "BaseFaultType"));
            assertNotNull(complexType);
            SchemaGlobalElement elem = 
                loader.findElement(new QName(NS_BF_2, "BaseFault"));
            assertNotNull(elem);
            
            elem = loader.findElement(new QName(ADDRESSING_NS, "EndpointReference"));
            assertNotNull(elem);
        } catch (WSDLException e) {
            fail(e.toString());
        }
    }
}
