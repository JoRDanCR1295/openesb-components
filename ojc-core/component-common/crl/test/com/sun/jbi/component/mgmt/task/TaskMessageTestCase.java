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
 * @(#)TaskMessageTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.mgmt.task;

import java.io.InputStream;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import junit.framework.TestCase;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;

import com.sun.jbi.component.mgmt.task.TaskXmlWriter;
import com.sun.jbi.crl.util.Util;

/**
 * Abstract base class for unit tests related to JBI task messages.
 * 
 * @author Kevan Simpson
 */
public abstract class TaskMessageTestCase extends TestCase {
    private static DocumentBuilderFactory mDocumentBuilderFactory = DocumentBuilderFactory.newInstance();
    
    private Properties mProps = null;
    
    /**
     * Constructs a test case for {@link TaskXmlWriter}.
     * @param testName The name of the test.
     */
    public TaskMessageTestCase(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        if (mProps == null) {
            InputStream is = TaskMessageTestCase.class.getResourceAsStream("task-message-xml.properties");
            mProps = new Properties();
            mProps.load(is);
        }
    }

    protected void tearDown() throws Exception {
    }

    protected boolean compareXml(String expectedXml, String actualXml) throws Exception {
        if (debug()) {
            System.out.println("Expected: "+ expectedXml);
            System.out.println("Actual:   "+ actualXml);
        }
        
        try {
            XMLUnit.setTestDocumentBuilderFactory(mDocumentBuilderFactory);
            XMLUnit.setControlDocumentBuilderFactory(mDocumentBuilderFactory);
            XMLUnit.setIgnoreWhitespace(true);
            
            Diff diff = XMLUnit.compareXML(expectedXml, actualXml);
            return diff.similar();
        }
        catch (ParserConfigurationException pce) {
            fail("Parsing error occurred comparing xml: "+ pce.getMessage());
            return false;
        }
    }
   
    protected String getProperty(String key) {
        return mProps.getProperty(key);
    }
    
    protected String insertParam(String rawXml, String token, String arg) {
        // allow for property substitution
        if (arg == null) {
            arg = mProps.getProperty(token);    // token is only a property key
            token = "{"+ token + "}";   // now it's a replaceable token
        }
        
        StringBuffer buff = new StringBuffer();
        int ix = rawXml.indexOf(token);
        buff.append(rawXml.substring(0, ix))
            .append(arg)
            .append(rawXml.substring((ix + token.length())));
        return buff.toString();
    }
    
    protected boolean debug() {
        if (mProps != null) {
            String debug = mProps.getProperty("debug");
            return (!Util.isEmpty(debug) && Boolean.parseBoolean(debug));
        }
        return false;
    }
}
