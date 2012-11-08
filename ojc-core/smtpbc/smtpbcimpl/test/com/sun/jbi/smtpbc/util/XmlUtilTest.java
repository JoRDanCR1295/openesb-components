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
 * @(#)XmlUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.util;

import java.io.StringReader;

import junit.framework.*;

import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.sax.SAXSource;

import org.xml.sax.InputSource;

/**
 *
 * @author Vishnuvardhan P.R
 */
public class XmlUtilTest extends TestCase {
    
    public XmlUtilTest(final String testName) {
        super(testName);
    }

    @Override
	protected void setUp() throws Exception {
    }

    @Override
	protected void tearDown() throws Exception {
    }

    /**
     * Test of transformToDOMResult method, of class com.sun.jbi.smtpbc.util.XmlUtil.
     */
    public void testTransformToDOMResult() throws Exception {
        System.out.println("transformToDOMResult");
        
        final StringBuffer xmlString = new StringBuffer("<?xml version=\"1.0\" encoding=\"UTF-8\"?> ");
        xmlString.append("<roottag><bodytag></bodytag></roottag>");
        final StringReader stringReader = new StringReader(xmlString.toString());
        final SAXSource saxSource = new SAXSource(new InputSource(stringReader));
        
        final Source source = saxSource;
        
        
        final DOMResult result = XmlUtil.transformToDOMResult(source);
        
        Assert.assertEquals("roottag", result.getNode().getFirstChild().getLocalName());
        
    }
    
}
