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
 * @(#)JBIMessageTestCase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.message;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 *
 * @author Sun Microsystems
 *
 */
public abstract class JBIMessageTestCase extends TestCase {
    protected Document validateXMLString(String elementString, String elementName) {
        Document document = null;
        try {
            document = JBITaskUtils.parse(elementString);
        } catch (SAXException se) {
            fail(elementName + " XML string is not well-formed");
            return null;
        }

        try {
            JBITaskUtils.validate(document);
        } catch (SAXException se) {
            se.printStackTrace();
            fail(elementName + " XML string fails schema validation");
        }

        return document;
    }
}
