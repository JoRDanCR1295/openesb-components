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
 * @(#)JbiTaskUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.util.task;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author ksimpson
 */
public class JbiTaskUtil {
    private static DocumentBuilderFactory mDocumentBuilderFactory = DocumentBuilderFactory.newInstance();

    /** Not intended for instantiation. */
    private JbiTaskUtil() {
    }
    
    private static Schema schema = null;

    static {
        InputStream in = null;
        try {
            // JBI task schema deployed in main src, not test src
            in = JbiTask.class.getResourceAsStream("managementMessage.xsd");
            SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            schema = schemaFactory.newSchema(new StreamSource(in));
        } 
        catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("Error building schema validator", e);
        }
    }

    public static Document parse(String elementString) throws SAXException {
        if (elementString == null) return null;
        try {
            if (!elementString.startsWith("<?xml")) {
                int pos = elementString.indexOf('>');
                elementString =
                    elementString.substring(0, pos) +
                    " xmlns='" + JbiTask.JBI_TASK_NAMESPACE_URI + "'>" +
                    elementString.substring(pos + 1);
            }

            DocumentBuilder documentBuilder = null;
            synchronized (mDocumentBuilderFactory) {
                documentBuilder = mDocumentBuilderFactory.newDocumentBuilder();
            }
            InputSource is = new InputSource(new StringReader(elementString));
            return documentBuilder.parse(is);

        } 
        catch (ParserConfigurationException pce) {
            throw new SAXException("Unexpected parse configuration exception", pce);
        }
        catch (IOException ioe) {
            throw new SAXException("Unexpected IO exception", ioe);
        }
    }

    public static void validate(String elementString) throws SAXException {
        validate(parse(elementString));
    }

    public static void validate(Document document) throws SAXException {
        try {
            Validator validator = schema.newValidator();
            validator.validate(new DOMSource(document));
        } 
        catch (IOException ioe) {
            throw new RuntimeException("Unexpected IO exception", ioe);
        }
    }
}
