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
 * @(#)DTELEngineTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel.test;

import junit.framework.*;
import com.sun.jbi.engine.dtel.DTELEngine;
import com.sun.jbi.engine.dtel.XmlUtil;

import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 * @author jwaldorf
 */
public class DTELEngineTest extends TestCase {

    public DTELEngineTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(DTELEngineTest.class);

        return suite;
    }

    public void testLoadDtelTable() {
        String mFile="test/data/riskscore.dtel";
        try {
            DTELEngine mDTELEngine = new DTELEngine();
            assertNotNull(mDTELEngine);

            mDTELEngine.loadDecisionTable(mFile);
        } catch (Exception ex) {
            fail(ex.getMessage());
        }
    }

    public void testRunDtelQuery() {
        String mFile="test/data/riskscore.dtel";
        String text="<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message xmlns:msgns=\"riskscore_dtel\" type=\"msgns:InputMessage\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><ns0:inElement xmlns:ns0=\"riskscore_dtel\"><Health>good</Health><Age>66.0</Age><Sex>Male</Sex></ns0:inElement></jbi:part></jbi:message>";
        try {
            DTELEngine mDTELEngine = new DTELEngine();
            assertNotNull(mDTELEngine);

            mDTELEngine.loadDecisionTable(mFile);

            DOMSource input = new DOMSource(XmlUtil.createDocumentFromXML(true, text));
            assertNotNull(input);

            DOMSource output =new DOMSource(XmlUtil.createDocument(true));
            assertNotNull(output);

            mDTELEngine.execute(input, output);
        } catch (Exception ex) {
            fail(ex.getMessage());
        }
    }

    /**
     * Runs the test suite using the textual runner.
     */
    public static void main(String[] args) {
        junit.textui.TestRunner.run(suite());
    }
}
