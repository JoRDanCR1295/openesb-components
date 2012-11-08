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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.common;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.wsdl.Definition;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

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
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;


/**
 * @author Sun Microsystems
 */
public class Utility {
    public static void assertArrayEquals(String msg, Object[] a, Object[] b) {
        if (!Arrays.equals(a, b)) {
            TestCase.fail(msg + " Arrays are not equal");
        }
    }

    public static void assertArrayEquals(String msg, long[] a, long[] b) {
        if (!Arrays.equals(a, b)) {
            TestCase.fail(msg + " Arrays are not equal");
        }
    }

    public static void logEnter(String className, String methodName) {
        System.out.println("Entering " + className + "_" + methodName);
    }

    public static void logExit(String className, String methodName) {
        System.out.println("Exiting " + className + "_" + methodName);
    }

    public static Element createDOMElement(InputSource is) {
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            dbf.setValidating(false);

            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(is);

            if (doc == null) {
                throw new IllegalArgumentException("xml parse error: ");
            }

            return doc.getDocumentElement();
        } catch (Exception e) {
            throw new RuntimeException("parse xml error");
        }
    }

    public static RBPELProcess loadBPEL(URL url) throws Exception {
        File bpelFile = new File(url.toURI());
        InputStream ipstream = url.openStream();
        InputStreamReader reader = new InputStreamReader(ipstream);
        BPELParseContext parseContext = new ParseContextImpl();
        parseContext.setCatalog(bpelFile.getParentFile(),bpelFile);

        IWSDLResolver wsdlResolver = DefaultWSDLResolverFactory.getInstance()
        .newWSDLResolver(url.toURI().toString(), parseContext);
        parseContext.setWSDLResolver(wsdlResolver);

        IXSDResolver xsdResolver = DefaultXSDResolverFactory.getInstance()
        .newXSDResolver(url.toURI().toString(), parseContext);
        parseContext.setXSDResolver(xsdResolver);

        ParsingCaches caches = new ParsingCaches();
        parseContext.setCaches(caches);
        DeferredActionRegistry registry = new DeferredActionRegistry();
        parseContext.setDeferredActionRegistry(registry);
        BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(reader,
                parseContext);
        WSDL4JExt.applySingleSchemaTypeLoader(registry, parseContext.getBaseURIResolver());
        bpelDoc.setBaseURI(bpelFile.getAbsolutePath());

        RBPELProcess bProc = (RBPELProcess) bpelDoc.getDocumentProcess();

        return bProc;
    }
}
