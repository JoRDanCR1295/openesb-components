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
 * @(#)JBIMessageWithNSTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.modelparsing;

import java.io.InputStream;
import java.net.URL;

import javax.xml.namespace.QName;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.xml.wsdl.WSDLDocument;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;
import javax.wsdl.Definition;

/**
 * @author Sun Inc
 * Jul 11, 2006
 */
public class JBIMessageWithNSTest extends AbstractTestCase{

    /**
     * @param testName
     */
    public JBIMessageWithNSTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        OneTimesetUp.registerXmlResourceProviderPool();
    }

    protected void tearDown() throws Exception {
        super.tearDown();

        // remove BPELs and inserted instance data??
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(JBIMessageWithNSTest.class);

        return suite;
    }
    
    /** partly negative test from an updateVariable API perspective. Because in reality
     * the RuntimeVariable object doesn't change, just it's value changes.
     * @throws Exception
     */
    public void testJBIMessageNamespaces() throws Exception {
            RBPELProcess process = loadBPELModel("jbimessagewithnamespace/newProcess.bpel");
    
            String dataFilePath = "bpel/jbimessagewithnamespace/jbiMessageWithNS.xml";
            WSMessage msg = constructMsg(dataFilePath);
            Element part = msg.getPart("part1");
            assertTrue(part != null);
            String uri1 = part.lookupNamespaceURI("messageLevelPrefix");
            assertTrue("http://java.sun.com/bpelse/junit/messageLevel".equals(uri1));
            String uri2 = part.lookupNamespaceURI("overrideAtPartLevel");
            assertFalse("http://java.sun.com/bpelse/junit/AtMessageLevel/override/AtPart".equals(uri2));
            assertTrue("http://java.sun.com/bpelse/junit/overridden/at/partlevel".equals(uri2));
            String uri3 = part.lookupNamespaceURI("partLevelPrefix");
            assertTrue("http://java.sun.com/bpelse/junit/partLevel".equals(uri3));
    }
    
    private WSMessage constructMsg(String dataFilePath) {
        InputSource ipSource = null;
        WSMessage domMsg = null;

        try {
            URL fileURL = getClass().getResource(dataFilePath);
            InputStream ipstream = fileURL.openStream();
            ipSource = new InputSource(ipstream);

            //Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(ipSource);
            Element elem = Utility.createDOMElement(ipSource);

            WSDLDocument doc = loadWSDL("bpel/jbimessagewithnamespace/newWSDL.wsdl");
            Definition defn = doc.getDefinition();

            QName qName = new QName(defn.getTargetNamespace(),
                    "newWSDLOperationRequest");

            //domMsg = new JBIMessageImpl(doc, msg);
            domMsg = new JBIMessageImpl(elem.getOwnerDocument(), defn.getMessage(qName));
        } catch (Throwable t) {
            t.printStackTrace();
            fail(t.getMessage());
        }

        return domMsg;
    }

}
