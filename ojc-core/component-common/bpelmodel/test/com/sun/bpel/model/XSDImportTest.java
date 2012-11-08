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
 * @(#)XSDImportTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.ProjectBasedWSDLResolver;
import com.sun.bpel.model.ProjectBasedWSDLResolverFactory;
import com.sun.bpel.model.ProjectBasedXSDResolver;
import com.sun.bpel.model.ProjectBasedXSDResolverFactory;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

import junit.framework.TestCase;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class XSDImportTest extends TestCase {

	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
	}

	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
	}

	public void testXSDImportedFromSameDirectory() {
        try {
            String fileName = "/com/sun/bpel/model/xsdImportTest/echo.bpel";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
             
            String baseURI = uri.toString();
            
            BPELParseContext parseContext = new BPELParseContext.DefaultParseContext();
            ProjectBasedWSDLResolver wsdlLoader = ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(baseURI, parseContext);
            parseContext.setWSDLResolver(wsdlLoader);
            
            ProjectBasedXSDResolver xsdResolver = ProjectBasedXSDResolverFactory.getInstance().newXSDResolver(baseURI, parseContext);
            parseContext.setXSDResolver(xsdResolver);
            
            Reader reader = new FileReader(new File(uri));
            
            ParsingCaches caches = new ParsingCaches();
            parseContext.setCaches(caches);
            DeferredActionRegistry registry = new DeferredActionRegistry();
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument document = BPELDocumentParseFactory.getInstance().load(reader, parseContext);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
            assertNotNull(document);
            
            BPELProcess process = document.getDocumentProcess();
            assertNotNull(process);
            
            //check if xsd import has vaid document
            List imports = process.getImports();
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
            
            //Assert XML schema component lookup
            SchemaTypeLoader loader = process.getSchemaTypeLoader();
            SchemaGlobalElement elem = loader.findElement(
                    new QName("urn:Foo", "helloObj"));
            assertNotNull(elem);
            SchemaType xmlType = loader.findType(
                    new QName("urn:Foo", "helloObjType"));
            assertNotNull(xmlType);
            
        } catch(Exception ex) {
            fail(ex.getMessage());
        }
    }
    
}
