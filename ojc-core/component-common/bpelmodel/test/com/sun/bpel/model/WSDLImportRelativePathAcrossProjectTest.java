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
 * @(#)WSDLImportRelativePathAcrossProjectTest.java 
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.extensions.schema.Schema;

import junit.framework.TestCase;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.BPELProcess;
import com.sun.bpel.model.Import;
import com.sun.bpel.model.ProjectBasedWSDLResolver;
import com.sun.bpel.model.ProjectBasedWSDLResolverFactory;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

public class WSDLImportRelativePathAcrossProjectTest extends TestCase {
    protected void tearDown() throws Exception {

        super.tearDown();
    }

    protected void setUp() throws Exception {

        super.setUp();
    }

    
    public void testWSDLImportedFromOneProjectToAnother() {
        try {
            String fileName = "/com/sun/bpel/model/acrossProjectTest/projectA/src/bank/bpelImportingWsdlFromProjectB.bpel";
            URL url = getClass().getResource(fileName);
            URI uri = url.toURI();
             
            String baseURI = uri.toString();
            
            BPELParseContext parseContext = new BPELParseContext.DefaultParseContext();
            ProjectBasedWSDLResolver loader = ProjectBasedWSDLResolverFactory.getInstance().newWSDLResolver(baseURI, parseContext);
            parseContext.setWSDLResolver(loader);
            
            String dependentProjectSourceDir = "/com/sun/bpel/model/acrossProjectTest/projectB/src";
            URL urlDependentProjectSourceDir = getClass().getResource(dependentProjectSourceDir);
            URI uriDependentProjectSourceDir = urlDependentProjectSourceDir.toURI();
             
            
            File dependentProjectSourceDirectory = new File(uriDependentProjectSourceDir);
            List projectSrcDirs = new ArrayList();
            projectSrcDirs.add(dependentProjectSourceDirectory);
            loader.setProjectClassPath(projectSrcDirs);
            
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
            
            List imports = process.getImports();
            Iterator it = imports.iterator();
            while(it.hasNext()) {
                Import imp = (Import) it.next();
                Object importedObject = imp.getImportedObject();
                assertNotNull(importedObject);
            }
        } catch(Exception ex) {
            fail(ex.getMessage());
        }
    }
}
