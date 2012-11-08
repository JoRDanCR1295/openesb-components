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
 * @(#)BPELModelWithCatalogTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.catalog;

import java.io.File;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.DefaultXSDResolverFactory;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.Utility;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

import javax.wsdl.Definition;
import javax.wsdl.Message;

public class BPELModelWithCatalogTest extends TestCase {

    private static final String BPELFILE_DIR = "CatalogSampleProject/build";

    public BPELModelWithCatalogTest() {
        super();
        // TODO Auto-generated constructor stub
    }

    public BPELModelWithCatalogTest(String testName) {
        super(testName);
        // TODO Auto-generated constructor stub
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(BPELModelWithCatalogTest.class);
        return suite;
    }


    protected RBPELProcess[] loadBPELModel() throws Exception {
        File bpelDir = new File (getClass().getResource(BPELFILE_DIR).toURI());
        HashSet ret = new HashSet();
        URL url = null;
        InputStream is = null;
        InputStreamReader reader = null;
        List bpelProcesses = new ArrayList();

        String [] bpelFiles = bpelDir.list(new FilenameFilter () {

            public boolean accept(File dir, String name) {
                // TODO Auto-generated method stub
                if (name.endsWith(".bpel"))
                    return true;
                return false;
            }
        });
        if ( bpelFiles != null && bpelFiles.length > 0) {
            for (int i=0; i< bpelFiles.length; i++) {
//                String bpelFileName = BPELFILE_DIR + File.separator + bpelFiles[i];
                File bpelFile = new File(bpelDir, bpelFiles[i]);
                String bpelFileURI = bpelFile.toURI().toString();
                url = bpelFile.toURL() ;// NO I18N at present
                is = url.openStream();
                reader = new InputStreamReader(is, "UTF-8");

                BPELParseContext parseContext = new ParseContextImpl();
                parseContext.setCatalog(bpelFile);
                IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory.getInstance()
                                                                       .newWSDLResolver(
                        bpelFileURI, parseContext
                    );
                parseContext.setWSDLResolver(wsdlResolver);


                // set the xsd resolver
                IXSDResolver xsdLoader = DefaultXSDResolverFactory.getInstance().newXSDResolver(
                        bpelFileURI, parseContext);
                parseContext.setXSDResolver(xsdLoader);

                ParsingCaches caches = new ParsingCaches();
                parseContext.setCaches(caches);
                DeferredActionRegistry registry = new DeferredActionRegistry();
                parseContext.setDeferredActionRegistry(registry);
                BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(
                        reader, parseContext
                    );
                WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
                bpelDoc.setBaseURI(url.toString());

                RBPELProcess bProc = (RBPELProcess) bpelDoc.getDocumentProcess();
                bpelProcesses.add(bProc);
             }
        }
        RBPELProcess[] bpelPrcs = new RBPELProcess [bpelProcesses.size()];
        bpelPrcs = (RBPELProcess[])bpelProcesses.toArray(bpelPrcs);
        return bpelPrcs;

    }

     public void testParsingModel () throws Exception {
         Utility.logEnter(getClass().getSimpleName(), "testParsingModel");

         RBPELProcess[] bpels = loadBPELModel();
         assertEquals(bpels.length, 1);
         RBPELProcess bpel = bpels[0];
         Message msg = bpel.getWSDLMessage(new QName("urn:GoogleSearch", "doGoogleSearch"));
         assertNotNull(msg);
         msg = bpel.getWSDLMessage(new QName("http://www.ncbi.nlm.nih.gov/soap/eutils/", "eInfoResponse_m"));
         assertNotNull(msg);
        Utility.logExit(getClass().getSimpleName(), "testParsingModel");

     }

}
