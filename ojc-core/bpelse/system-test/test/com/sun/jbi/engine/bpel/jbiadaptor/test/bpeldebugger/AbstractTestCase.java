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
 * @(#)AbstractTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.rmi.server.UID;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import junit.framework.TestCase;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.util.ParsingCaches;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.xsd.XMLSchema;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.SetUpHelper;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;


/**
 * @author Sun Microsystems
 */
public abstract class AbstractTestCase extends TestCase {
    String mEngId;
    Engine mEng;
    Properties mConnProp;
    StateManager mStateMgr;

    public AbstractTestCase(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        SetUpHelper setUp = new SetUpHelper();
        mEng = setUp.getEngine();
        mEngId = mEng.getId();
        mConnProp = setUp.getEngineProperties();
        mStateMgr = mEng.getStateManager();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    protected RBPELProcess loadBPELModel(String fileName) {
        String filePath = "bpel/" + fileName;

        RBPELProcess bProc = null;

        try {
            URL url = getClass().getResource(filePath);
            File bpelFile = new File(url.toURI());

            InputStream ipstream = url.openStream();
            InputStreamReader reader = new InputStreamReader(ipstream);
            BPELParseContext parseContext = new ParseContextImpl();
            parseContext.setCatalog(bpelFile);
            IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory.getInstance()
            .newWSDLResolver(url.toURI().toString(), parseContext);
            parseContext.setWSDLResolver(wsdlResolver);

            ParsingCaches caches = new ParsingCaches();
            parseContext.setCaches(caches);
            DeferredActionRegistry registry = new DeferredActionRegistry();
            parseContext.setDeferredActionRegistry(registry);
            BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(reader,
                    parseContext);
            WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
            bpelDoc.setBaseURI(bpelFile.getAbsolutePath());
            bProc = (RBPELProcess) bpelDoc.getDocumentProcess();
        } catch (Throwable t) {
            fail(" failed to parse the file " + t.getMessage());
        }

        return bProc;
    }

//    //protected State createState(String bpelId) {
//    protected State createState(QName bpelId) {
//        String bpId = new UID().toString();
//
//        return StateFactory.getStateFactory().createState(mEngId, bpelId, bpId);
//    }
}
