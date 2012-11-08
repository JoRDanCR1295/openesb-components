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
 * @(#)DOMHelperTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.miscellaneous;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.w3c.dom.Element;

import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.test.common.OneTimesetUp;
import com.sun.jbi.engine.bpel.core.test.common.Utility;


/**
 * @author Sun Microsystems
 */
public class DOMHelperTest extends TestCase {
    private static Logger mLogger = Logger.getLogger(
            "com.sun.jbi.engine.bpel.core.bpelpersist.ModelParsingID");

    public DOMHelperTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        // ensure XmlResourceProviderPool is available
        OneTimesetUp.getSetUp();
    }

    protected void tearDown() throws Exception {
        // remove BPELs and inserted instance data??
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(DOMHelperTest.class);

        return suite;
    }

    public void testCreateDOMFromString() throws Exception {
        Utility.logEnter(getClass().getSimpleName(), "testCreateDOMFromString");

        InputStream is = getClass().getResourceAsStream("data/echo.xml");
        ByteArrayOutputStream os = new ByteArrayOutputStream();

        try {
            int c;

            while ((c = is.read()) != -1) {
                os.write(c);
            }

            is.close();
            os.close();
        } catch (IOException e) {
            throw e;
        }

        String xmlString = new String(os.toByteArray());

        Element elem = DOMHelper.createDOM(xmlString);
        assertNotNull(elem);
        Utility.logExit(getClass().getSimpleName(), "testCreateDOMFromString");
    }

    public void testCreateFaultDOMFromException() throws Exception {
        Utility.logEnter(getClass().getSimpleName(),
            "testCreateFaultDOMFromException");

        Element elem = null;

        try {
            String s = null;
            s.contains("a");
        } catch (Throwable t) {
            String faultString = constructFault(t.getMessage());
            elem = DOMHelper.createDOM(faultString);
        }

        assertNotNull(elem);
        Utility.logExit(getClass().getSimpleName(),
            "testCreateFaultDOMFromException");
    }

    private String constructFault(String msg) {
        String retVal = "<fault><reason>" + msg + "</reason></fault>";

        return retVal;
    }
}
