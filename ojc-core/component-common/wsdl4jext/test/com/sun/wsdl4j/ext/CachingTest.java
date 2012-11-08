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
 * @(#)CachingTest.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.XmlException;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import junit.framework.TestCase;

/**
 * Tests catalog and caching in loading WSDL.  Make sure no repetitive loading
 * of same WSDLs and XSDs.
 * 
 * @author Jun Xu
 */
public class CachingTest extends TestCase {

    protected static final String TOP_WSDL =
        "test/data/catalogandcache/Catalog2_WSDefinition.wsdl";
    protected static final String CATALOG2_WSDL_TNS =
        "http://j2ee.netbeans.org/wsdl/Catalog2_WSDefinition";
    protected static final String CATALOG1_WSDL_TNS =
        "http://j2ee.netbeans.org/wsdl/Catalog1_WSDefinition";
    protected static final String CATALOG2_XSD_TNS =
        "http://xml.netbeans.org/schema/Catalog2_Schema";
    protected static final String CATALOG1_XSD_TNS =
        "http://xml.netbeans.org/schema/Catalog1_Schema";
    protected static final String CATALOG1_WSDL_INLINEXSD_TNS =
        "http://j2ee.netbeans.org/wsdl/Catalog1_WSDefinition_inlineschema";
    
    protected WSDLReader _wsdlReader;
    
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Tests loading a simple WSDL.
     */
    public void testCatalogAndCaching() {
        try {
            //Load the WSDL
            EntityResolver resolver = new EntityResolverImpl();
            DeferredActionRegistry registry = new DeferredActionRegistry();
            Map<String, Definition> wsdlCache = new HashMap<String, Definition>();
            Map<String, Schema> xsdCache = new HashMap<String, Schema>();
            _wsdlReader = WSDL4JExt.newWSDLReader(resolver, registry, wsdlCache, xsdCache);
            Definition wsdlDef = _wsdlReader.readWSDL(TOP_WSDL);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, resolver);
            assertNotNull(wsdlDef);

            //Assert XSD cache size
            assertEquals(4, xsdCache.size());
            
            //Assert XSD artifacts
            SchemaTypeLoader loader = WSDL4JExt.getSchemaTypeLoader(wsdlDef);
            
            //Catalog 1 XSD
            SchemaType catalog1Type =
                loader.findType(new QName(CATALOG1_XSD_TNS, "catalog1_type"));
            assertNotNull(catalog1Type);
            SchemaGlobalElement elem = 
                loader.findElement(new QName(CATALOG1_XSD_TNS, "catalog1_root"));
            assertNotNull(elem);
            
            //Catalog 1 WSDL inline XSD
            elem = 
                loader.findElement(
                        new QName(CATALOG1_WSDL_INLINEXSD_TNS,
                                "catalog1_inline_root"));
            assertNotNull(elem);
            
            //Catalog 2 XSD
            SchemaType catalog2Type =
                loader.findType(new QName(CATALOG2_XSD_TNS, "Catalog2_type"));
            assertNotNull(catalog2Type);
            elem = 
                loader.findElement(new QName(CATALOG2_XSD_TNS, "catalog2_use_catalog1_type"));
            assertNotNull(elem);
            elem = 
                loader.findElement(new QName(CATALOG2_XSD_TNS, "catalog2_root"));
            assertNotNull(elem);
            
            //Assert WSDL artifacts
            Message message =
                wsdlDef.getMessage(
                        new QName(CATALOG1_WSDL_TNS, "Catalog1_WSDefinitionOperationRequest"));
            assertNotNull(message);
        } catch (WSDLException e) {
            fail(e.toString());
        } catch (XmlException e) {
            fail(e.toString());
        }
    }
    
    private static class EntityResolverImpl implements EntityResolver {
        
        static private Map<String, String> locationMap =
            new HashMap<String, String>();
        
        static {
            locationMap.put("CatalogBpel1/Catalog1_WSDefinition.wsdl",
                    "test/data/catalogandcache/_dependent/CatalogBpel1/src/Catalog1_WSDefinition.wsdl");
            locationMap.put("CatalogBpel1/Catalog1_Schema.xsd",
                    "test/data/catalogandcache/_dependent/CatalogBpel1/src/Catalog1_Schema.xsd");
        }
        
        public InputSource resolveEntity(String publicId, String systemId)
                throws SAXException, IOException {
            String redirect = locationMap.get(systemId);
            if (redirect != null) {
                InputStream inStream = new FileInputStream(new File(redirect));
                InputSource inSource = new InputSource(inStream);
                inSource.setPublicId(publicId);
                inSource.setSystemId(redirect);
                return inSource;
            }
            return null;
        }
    }
}
