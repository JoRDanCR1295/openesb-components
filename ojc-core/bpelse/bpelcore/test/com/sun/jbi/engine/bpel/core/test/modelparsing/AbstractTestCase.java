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

package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;

import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;

import junit.framework.TestCase;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.bpel.xml.wsdl.impl.WSDLDocumentImpl;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import com.sun.wsdl4j.ext.WSDL4JExt;


/**
 * @author Sun Microsystems
 */
public abstract class AbstractTestCase extends TestCase {
    public AbstractTestCase(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    protected RBPELProcess loadBPELModel(String fileName) {
        String filePath = "bpel/" + fileName;
        
        RBPELProcess bProc = null;
        
        try {
            URL url = getClass().getResource(filePath);
            bProc = Utility.loadBPEL(url);
        } catch (Throwable t) {
            t.printStackTrace();
            fail(" failed to parse the file " + t.getMessage());
        }
        return bProc;
    }
    
    protected WSDLDocument loadWSDL(String location) throws Exception {
        URL wsdlURL = getClass().getResource(location);
        URI uri = wsdlURL.toURI();
        WSDLReader wsdlReader = WSDL4JExt.newWSDLReader(null);
        Definition wsdlDef = wsdlReader.readWSDL(new File(uri).getAbsolutePath());
        return new WSDLDocumentImpl(uri.toString(), wsdlDef);
    }
}
