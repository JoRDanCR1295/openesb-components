/**
 *   xmpp-binding-component - XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.xmpp.component;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.custommonkey.xmlunit.XMLTestCase;

import java.io.StringWriter;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import static com.gestalt.jbi.xmpp.component.XMPPConsumerHandler.PacketType;

public class XMPPXBeanConsumerHanderTest extends XMLTestCase {
    XMPPXBeanConsumerHandler test = new XMPPXBeanConsumerHandler(null);

    private final String EXPECTED_SIMPLE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><message xmlns=\"http://com.gestalt.jbi/xmpp\" jabberId=\"from\"><body>body</body></message>";
    private final String EXPECTED_COMPLEX = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><message xmlns=\"http://com.gestalt.jbi/xmpp\" jabberId=\"from\"><body><tag xmlns=\"somens\">body</tag></body></message>";


    public void testSimpleBody() throws Exception {
        Document doc = test.wrapMessage(null, "from", "body", "123",
                PacketType.message);
        assertXMLEqual(EXPECTED_SIMPLE,docToString(doc));
    }

    public void testComplexBody() throws Exception {
        Document doc = test.wrapMessage(null, "from", "<tag xmlns=\"somens\">body</tag>", "123",
                PacketType.message);
        assertXMLEqual(EXPECTED_COMPLEX,docToString(doc));
    }

    private String docToString(Document doc) throws Exception {
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(doc);
        StringWriter sw = new StringWriter();
        StreamResult result = new StreamResult(sw);
        transformer.transform(source, result);

        return sw.toString();
    }
}
