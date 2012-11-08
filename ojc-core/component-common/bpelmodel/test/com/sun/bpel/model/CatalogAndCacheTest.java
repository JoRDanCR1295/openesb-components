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
 * @(#)CatalogAndCacheTest.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

import junit.framework.TestCase;

/**
 * Tests catalog and caching.  Make sure catalog is correctly used and no
 * repetitive loading of same WSDLs and XSDs, and there is only single schema
 * type loader is created.
 * 
 * @author Jun Xu
 */
public class CatalogAndCacheTest extends TestCase {

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
    
    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testCase1() {
        try {
            String fileName1 =
                "/com/sun/bpel/model/catalogandcacheTest/Catalog2_BP_Impl_Catalog1Port.bpel";
            String fileName2 =
                "/com/sun/bpel/model/catalogandcacheTest/Catalog2_BP_Impl_Catalog2Port.bpel";
            
            URL url = getClass().getResource(fileName1);
            URI uri = url.toURI();
            
            //Assume uri is a file URI
            File bpelFile1 = new File(uri);
            String baseURI1 = uri.toString();

            url = getClass().getResource(fileName2);
            uri = url.toURI();
            
            File bpelFile2 = new File(uri);
            String baseURI2 = uri.toString();
            
            ParsingCaches caches = new ParsingCaches();
            DeferredActionRegistry registry = new DeferredActionRegistry();

            //Load the first BPEL file
            BPELParseContext parseContext =
                new BPELParseContext.DefaultParseContext();
            parseContext.setCatalog(bpelFile1);
            
            ProjectBasedWSDLResolver wsdlLoader =
                ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(
                        baseURI1, parseContext);
            parseContext.setWSDLResolver(wsdlLoader);
            
            ProjectBasedXSDResolver xsdResolver =
                ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(
                        baseURI1, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            Reader reader = new FileReader(bpelFile1);
            
            parseContext.setCaches(caches);
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document1 =
                BPELDocumentParseFactory.getInstance().load(
                        reader, parseContext);
            
            //Load the second BPEL file
            parseContext =
                new BPELParseContext.DefaultParseContext();
            parseContext.setCatalog(bpelFile2);
            
            wsdlLoader =
                ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(
                        baseURI2, parseContext);
            parseContext.setWSDLResolver(wsdlLoader);
            
            xsdResolver =
                ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(
                        baseURI2, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            reader = new FileReader(bpelFile2);
            
            parseContext.setCaches(caches);
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document2 =
                BPELDocumentParseFactory.getInstance().load(
                        reader, parseContext);
            
            for (Schema schemaObj : registry.getSchemaDocumentCollector()) {
                System.out.println(schemaObj.documentProperties().getSourceName());
            }
            
            //Apply deferred actions
            WSDL4JExt.applySingleSchemaTypeLoader(
                    registry, parseContext.getBaseURIResolver());
            
            assertNotNull(document1);
            assertNotNull(document2);
            
            //Assert cache size
            assertEquals(4, caches.getXSDCacheForWSDL().size());
            assertEquals(2, caches.getWSDLCacheForWSDL().size());
            assertEquals(2, caches.getWSDLCacheForBPEL().size());
            assertEquals(2, caches.getXSDCacheForBPEL().size());
            
            //Assert everything in BP 1
            BPELProcess process1 = document1.getDocumentProcess();
            assertNotNull(process1);
            
            //check if all imports have valid model loaded
            List imports = process1.getImports();
            Iterator it = imports.iterator();
            while(it.hasNext()) {
                Import imp = (Import) it.next();
                String location = imp.getLocation();
                Object importedObject = imp.getImportedObject();
                assertNotNull(importedObject);
                if(location.endsWith("xsd")) {
                    assertTrue(importedObject instanceof XMLSchema);
                } else if(location.endsWith("wsdl")) {
                    assertTrue(importedObject instanceof WSDLDocument);
                }
            }
                        
            SchemaTypeLoader loader1 = process1.getSchemaTypeLoader();
            //Assert single schema type system
            Collection<Definition> wsdls =
                process1.getAllImportedWSDLDefinitions();
            for (Definition wsdlDef : wsdls) {
                assertTrue(loader1 == WSDL4JExt.getSchemaTypeLoader(wsdlDef));
            }
            
            //Assert no repetitive loading of the same WSDL
            wsdls = process1.getImportedWSDLDefinitions(CATALOG1_WSDL_TNS);
            assertTrue(wsdls.size() > 0);
            Definition ct1wsdlInBpel1 = wsdls.iterator().next();
            wsdls = process1.getImportedWSDLDefinitions(CATALOG2_WSDL_TNS);
            assertTrue(wsdls.size() > 0);
            List<javax.wsdl.Import> wsdlList = wsdls.iterator().next().getImports(CATALOG1_WSDL_TNS);
            assertTrue(wsdlList.size() > 0);
            Definition ct1wsdlInCt2WSDLInBpel1 = wsdlList.get(0).getDefinition();
            assertTrue(ct1wsdlInBpel1 == ct1wsdlInCt2WSDLInBpel1);
            
            //Assert XML schema component lookup
            //Catalog 1 XSD
            SchemaType catalog1Type =
                loader1.findType(new QName(CATALOG1_XSD_TNS, "catalog1_type"));
            assertNotNull(catalog1Type);
            SchemaGlobalElement elem = 
                loader1.findElement(new QName(CATALOG1_XSD_TNS, "catalog1_root"));
            assertNotNull(elem);
            
            //Catalog 1 WSDL inline XSD
            elem = 
                loader1.findElement(
                        new QName(CATALOG1_WSDL_INLINEXSD_TNS,
                                "catalog1_inline_root"));
            assertNotNull(elem);
            
            //Catalog 2 XSD
            SchemaType catalog2Type =
                loader1.findType(new QName(CATALOG2_XSD_TNS, "Catalog2_type"));
            assertNotNull(catalog2Type);
            elem = 
                loader1.findElement(new QName(CATALOG2_XSD_TNS, "catalog2_use_catalog1_type"));
            assertNotNull(elem);
            elem = 
                loader1.findElement(new QName(CATALOG2_XSD_TNS, "catalog2_root"));
            assertNotNull(elem);
            
            //Assert WSDL artifacts
            Message message =
                ct1wsdlInBpel1.getMessage(
                        new QName(CATALOG1_WSDL_TNS, "Catalog1_WSDefinitionOperationRequest"));
            assertNotNull(message);
            
            
            //Assert everything in BP 2
            BPELProcess process2 = document2.getDocumentProcess();
            assertNotNull(process2);
            
            //check if all imports have valid model loaded
            imports = process1.getImports();
            it = imports.iterator();
            while(it.hasNext()) {
                Import imp = (Import) it.next();
                String location = imp.getLocation();
                Object importedObject = imp.getImportedObject();
                assertNotNull(importedObject);
                if(location.endsWith("xsd")) {
                    assertTrue(importedObject instanceof XMLSchema);
                } else if(location.endsWith("wsdl")) {
                    assertTrue(importedObject instanceof WSDLDocument);
                }
            }
                        
            SchemaTypeLoader loader2 = process2.getSchemaTypeLoader();
            //Assert single schema type system
            assertTrue(loader1 == loader2);
            wsdls =
                process2.getAllImportedWSDLDefinitions();
            for (Definition wsdlDef : wsdls) {
                assertTrue(loader2 == WSDL4JExt.getSchemaTypeLoader(wsdlDef));
            }
            
            //Assert no repetitive loading of the same WSDL
            wsdls = process2.getImportedWSDLDefinitions(CATALOG1_WSDL_TNS);
            assertTrue(wsdls.size() > 0);
            Definition ct1wsdlInBpel2 = wsdls.iterator().next();
            wsdls = process2.getImportedWSDLDefinitions(CATALOG2_WSDL_TNS);
            assertTrue(wsdls.size() > 0);
            wsdlList = wsdls.iterator().next().getImports(CATALOG1_WSDL_TNS);
            assertTrue(wsdlList.size() > 0);
            Definition ct1wsdlInCt2WSDLInBpel2 = wsdlList.get(0).getDefinition();
            assertTrue(ct1wsdlInBpel2 == ct1wsdlInCt2WSDLInBpel2);
            
            assertTrue(ct1wsdlInBpel1 == ct1wsdlInBpel2);
            
            //Assert WSDL artifacts
            message =
                ct1wsdlInBpel2.getMessage(
                        new QName(CATALOG1_WSDL_TNS, "Catalog1_WSDefinitionOperationRequest"));
            assertNotNull(message);
            
        } catch(Exception ex) {
            if (ex instanceof RuntimeException) {
                throw (RuntimeException) ex;
            } else {
                throw new RuntimeException(ex);
            }
        }
    }
}
