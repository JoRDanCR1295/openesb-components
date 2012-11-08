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
 * @(#)UtilTestCase.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util.test;

import java.io.File;

import javax.wsdl.Definition;
import javax.wsdl.xml.WSDLReader;

import junit.framework.TestCase;

import org.xml.sax.InputSource;

import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * Base class for ojc-core unit tests.
 * @author Kevan Simpson
 */
public class UtilTestCase extends TestCase {
    private static WSDLReader mReader = WSDL4JExt.newWSDLReader(null);
    private XmlTester mXmlTester = new XmlTester();
    
    /**
     * 
     */
    public UtilTestCase() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param name
     */
    public UtilTestCase(String name) {
        super(name);
        // TODO Auto-generated constructor stub
    }

    protected InputSource readInputSource(String... steps) throws Exception {
        StringBuffer buff = new StringBuffer();
        for (int i = 0, n = steps.length, X = (n - 1); i < n; i++) {
            buff.append(steps[i]);
            if (i < X) {
                buff.append(File.separator);
            }
        }

        return new InputSource(
                this.getClass().getResourceAsStream(buff.toString()));
    }
    
    /**
     * Loads a WSDL document from the specified relative path.
     * @param path A path, relative to the unit test, to a WSDL document.
     * @return A WSDL document.
     * @throws Exception If an error occurs reading WSDL.
     */
    protected Definition readWSDL(String path) throws Exception {
        return mReader.readWSDL(
                getClass().getResource(path).toURI().toString(), 
                new InputSource(getClass().getResourceAsStream(path)));
    }

    /**
     * Converts the specified relative path to an absolute path.
     * @param path A relative path.
     * @return The absolute path.
     */
    protected String resolvePath(String path) {
        return this.getClass().getResource(path).getFile();
    }

    protected WSDLReader getWSDLReader() {
        return mReader;
    }
    
    protected XmlTester getXmlTester() {
        return mXmlTester;
    }
    
    protected void setWSDLReader(WSDLReader reader) {
        mReader = reader;
    }
}
