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
 * @(#)DOMModelTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.dom;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.ri.model.XMLModelTestCase;
import org.apache.commons.jxpath.xml.DocumentContainer;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Tests JXPath with DOM
 *
 * @author Dmitri Plotnikov
 * @version  
 */

public class DOMModelTest extends XMLModelTestCase {
    
    /**
     * Construct a new instance of this test case.
     *
     * @param name Name of the test case
     */
    public DOMModelTest(String name) {
        super(name);
    }

    /**
     * Return the tests included in this test suite.
     */
    public static Test suite() {
        return (new TestSuite(DOMModelTest.class));
    }

    protected String getModel() {
        return DocumentContainer.MODEL_DOM;
    }

    protected AbstractFactory getAbstractFactory() {
        return new TestDOMFactory();
    }
    
    public void testGetNode() {
        assertXPathNodeType(context, "/", Document.class);
        assertXPathNodeType(context, "/vendor/location", Element.class);
        assertXPathNodeType(context, "//location/@name", Attr.class);
    }
    
    protected String getXMLSignature(
        Object node,
        boolean elements,
        boolean attributes,
        boolean text,
        boolean pi) 
    {
        StringBuffer buffer = new StringBuffer();
        appendXMLSignature(buffer, node, elements, attributes, text, pi);
        return buffer.toString();
    }

    private void appendXMLSignature(
        StringBuffer buffer,
        Object object,
        boolean elements,
        boolean attributes,
        boolean text,
        boolean pi) 
    {
        Node node = (Node) object;
        int type = node.getNodeType();
        switch (type) {
            case Node.DOCUMENT_NODE :
                buffer.append("<D>");
                appendXMLSignature(
                    buffer,
                    node.getChildNodes(),
                    elements,
                    attributes,
                    text,
                    pi);
                buffer.append("</D");
                break;

            case Node.ELEMENT_NODE :
                String tag = elements ? ((Element) node).getTagName() : "E";
                buffer.append("<");
                buffer.append(tag);
                buffer.append(">");
                appendXMLSignature(
                    buffer,
                    node.getChildNodes(),
                    elements,
                    attributes,
                    text,
                    pi);
                buffer.append("</");
                buffer.append(tag);
                buffer.append(">");
                break;

            case Node.TEXT_NODE :
            case Node.CDATA_SECTION_NODE :
                if (text) {
                    String string = node.getNodeValue();
                    string = string.replace('\n', '=');
                    buffer.append(string);
                }
                break;
        }
    }

    private void appendXMLSignature(
        StringBuffer buffer,
        NodeList children,
        boolean elements,
        boolean attributes,
        boolean text,
        boolean pi) 
    {
        for (int i = 0; i < children.getLength(); i++) {
            appendXMLSignature(
                buffer,
                children.item(i),
                elements,
                attributes,
                text,
                pi);
        }
    }
}
