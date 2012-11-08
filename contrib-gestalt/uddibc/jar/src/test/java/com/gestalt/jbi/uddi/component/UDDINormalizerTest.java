/**
 *   rss-binding-component - RSS Binding Component
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
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.nmr.NmrWrapperUtils;
import junit.framework.TestCase;
import org.w3c.dom.Document;

import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;


/**
 * Author: jblack
 * Date: June 25, 2007
 */
public class UDDINormalizerTest extends TestCase {
    private static final String JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/uddiWsdl\" " +
        "type=\"tns:uddiWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EndpointReferenceList xmlns=\"http://xml.netbeans.org/schema/1.0/ws/addressing/extensions\">" +
        "<EndpointReference>" + "<Address>http://foo.com/reference</Address>" +
        "</EndpointReference>" + "</EndpointReferenceList>" + "</jbi:part>" +
        "</jbi:message>";
    private static final String MULTIPLE_JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/uddiWsdl\" " +
        "type=\"tns:uddiWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EndpointReferenceList xmlns=\"http://xml.netbeans.org/schema/1.0/ws/addressing/extensions\">" +
        "<EndpointReference>" + "<Address>http://foo.com/reference</Address>" +
        "</EndpointReference>" + "<EndpointReference>" +
        "<Address>http://bar.com/reference</Address>" + "</EndpointReference>" +
        "</EndpointReferenceList>" + "</jbi:part>" + "</jbi:message>";
    private static final String EMPTY_JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/uddiWsdl\" " +
        "type=\"tns:uddiWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EndpointReferenceList xmlns=\"http://xml.netbeans.org/schema/1.0/ws/addressing/extensions\"/>" +
        "</jbi:part>" + "</jbi:message>";
    private static final QName TYPE = new QName("http://j2ee.netbeans.org/wsdl/uddiWsdl",
            "uddiWsdlOperationRequest", "tns");

    /**
     *
     * @throws Exception
     */
    public void testNormalizesSingleUddiEndpointReference()
        throws Exception {
        List<String> uddiInquiryResults = new ArrayList<String>();
        uddiInquiryResults.add("http://foo.com/reference");

        UDDINormalizer uddiNormalizer = new UDDINormalizer();
        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(TYPE, null);
        uddiNormalizer.createConsumerContent(TYPE, uddiInquiryResults, wrapper);

        Document result = wrapper.getResult();
        assertNotNull("Result was null", result);
        StringWriter sw = new StringWriter();
        try {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }
        assertNotNull("String Writer was null", sw);
        assertEquals("Result was not equal to multiple endpoint jbiMessage: ",
            JBI_MESSAGE, sw.getBuffer().toString());
    }

    /**
     *
     * @throws Exception
     */
    public void testNormalizesMultipleUddiEndpointReferences()
        throws Exception {
        List<String> uddiInquiryResults = new ArrayList<String>();
        uddiInquiryResults.add("http://foo.com/reference");
        uddiInquiryResults.add("http://bar.com/reference");

        UDDINormalizer uddiNormalizer = new UDDINormalizer();
        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(TYPE, null);
        uddiNormalizer.createConsumerContent(TYPE, uddiInquiryResults, wrapper);

        Document result = wrapper.getResult();
        assertNotNull("Result was null", result);
        StringWriter sw = new StringWriter();
        try {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }
        assertNotNull("String Writer was null", sw);
        assertEquals("Result was not equal to multiple endpoint jbiMessage: ",
            MULTIPLE_JBI_MESSAGE, sw.getBuffer().toString());
    }

    /**
     *
     * @throws Exception
     */
    public void testNormalizesEmptySetEndpointReferences()
        throws Exception {
        List<String> uddiInquiryResults = new ArrayList<String>();

        UDDINormalizer uddiNormalizer = new UDDINormalizer();
        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(TYPE, null);
        uddiNormalizer.createConsumerContent(TYPE, uddiInquiryResults, wrapper);

        Document result = wrapper.getResult();
        assertNotNull("Result was null", result);
        StringWriter sw = new StringWriter();
        try {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }
        assertNotNull("String Writer was null", sw);
        assertEquals("Result was not equal to multiple endpoint jbiMessage: ",
            EMPTY_JBI_MESSAGE, sw.getBuffer().toString());
    }

    /**
     *
     * @throws Exception
     */
    public void testIntentionalMisuseEndpointReference()
        throws Exception {
        List<String> uddiInquiryResults = new ArrayList<String>();
        uddiInquiryResults.add("</EndpointReferenceList>");

        UDDINormalizer uddiNormalizer = new UDDINormalizer();

        try {
            NmrWrapperUtils wrapper = new NmrWrapperUtils();
            wrapper.init(TYPE, null);
            uddiNormalizer.createConsumerContent(TYPE, uddiInquiryResults,
                wrapper);
            // String result = wrapper.getResult();
            assertTrue("parse exception not thrown as expected", false);
        } catch (Throwable t) {
            // SAXParseException for example
            assertTrue("expected", true);
        }
    }
}
