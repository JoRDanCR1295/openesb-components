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
 * GzipEncoderImpl.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.gestalt.jbi.encoding;

import com.gestalt.dmit.common.injection.BeanContainer;
import com.gestalt.dmit.common.injection.BeanContainerFactory;
import com.gestalt.jbi.mock.javax.jbi.component.MockComponentContext;
import com.gestalt.jbi.mock.javax.jbi.messaging.MockDeliveryChannel;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.servicemix.jbi.jaxp.BytesSource;
import org.custommonkey.xmlunit.XMLTestCase;
import org.custommonkey.xmlunit.XMLUnit;

import javax.activation.DataHandler;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringWriter;


/**
 * Unit testing of the Encoding JBI Service Engine.
 */
public class EncodingComponentTest extends XMLTestCase  {
    public EncodingComponentTest(String whichTest) {
        super(whichTest);
        XMLUnit.setIgnoreWhitespace(true);
    }

    /**
     * Used by JUnit to determine which tests to run.
     * @return Test suite to be run by JUnit
     */
    static public Test suite() {
        TestSuite suite = new TestSuite();

        suite.addTest(new EncodingComponentTest("testBasicZip"));
        suite.addTest(new EncodingComponentTest("testComponentTagEncoding"));
        suite.addTest(new EncodingComponentTest("testComponentZipEncoding"));
        suite.addTest(new EncodingComponentTest("testStripSoap"));
        suite.addTest(new EncodingComponentTest("testStripSoapRoundTrip"));
        return suite;
    }

    /**
     * This test method tests the lower level zip methods
     * that handle encoding and decoding.
     * Used to bypass all the JBI code to ensure the zip encode
     * decode works correctly.
     * @throws Exception
     */
    public void testBasicZip() throws Exception {
        EncodingComponent comp = new EncodingComponent();
        String input = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            + "<UOB version=\"0.5\">" + "<ForceStructureInformation/>"
            + "<Relationships count=\"1\">"
            + "<Relationship type=\"Scenario\">"
            + "<Description>UOB Data extracted SGS Workstation</Description>"
            + "<Assigned>" + "<UnitNode UIC=\"LOC000178\">"
            + "<ChildUnitNode UIC=\"SQU000376\"/>" + "</UnitNode>"
            + "</Assigned>" + "</Relationship>" + "</Relationships>" + "</UOB>";

        byte[] bytes = comp.encodeZip(input.getBytes());
        byte[] result = comp.decodeZip(bytes);
        assertXMLEqual(input, new String(result));
    }

    /**
     * Encode a basic XML document with "tag" encoding style.
     * Decode the encoded response. Verify the decoded document is the same
     * as the original one.
     */
    public void testComponentTagEncoding() {
        EncodingComponent encoding = new EncodingComponent();
        doTestComponentEncoding(encoding, EncodingStyle.TAG);
    }

    /**
     * Encode a basic XML document with "zip" encoding style.
     * Decode the encoded response. Verify the decoded document is the same
     * as the original one.
     */
    public void testComponentZipEncoding() {
        EncodingComponent encoding = new EncodingComponent();
        doTestComponentEncoding(encoding, EncodingStyle.ZIP);
    }

    public void testStripSoap() throws Exception {
        String msgWithSoap =
                "<e:Envelope xmlns:e=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
                "<e:Body>" +
                "<ping>" +
                "<pingRequest>" +
                "<request xmlns=\"http://pingpongws.nettoolkit.gestalt.com\">hello</request>" +
                "</pingRequest>" +
                "</ping>" +
                "</e:Body>" +
                "</e:Envelope>";

        String msgWithOutSoap =
                "<ping>" +
                "<pingRequest>" +
                "<request xmlns=\"http://pingpongws.nettoolkit.gestalt.com\">hello</request>" +
                "</pingRequest>" +
                "</ping>";

        EncodingComponent encodingComponent = new EncodingComponent();
        String newMsg = encodingComponent.stripSoap(msgWithSoap);

        assertXMLEqual(newMsg, msgWithOutSoap);
    }

    public void testStripSoapRoundTrip() throws Exception {
        String msgWithSoap =
                "<e:Envelope xmlns:e=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
                "<e:Body>" +
                "<ping>" +
                "<pingRequest>" +
                "<request xmlns=\"http://pingpongws.nettoolkit.gestalt.com\">hello</request>" +
                "</pingRequest>" +
                "</ping>" +
                "</e:Body>" +
                "</e:Envelope>";

        String msgWithOutSoap =
                "<ping>" +
                "<pingRequest>" +
                "<request xmlns=\"http://pingpongws.nettoolkit.gestalt.com\">hello</request>" +
                "</pingRequest>" +
                "</ping>";

        EncodingComponent encodingComponent = new EncodingComponent();
        byte[] encoded = encodingComponent.encode(msgWithSoap.getBytes());
        byte[] decoded = encodingComponent.decode(encoded);
        String test = encodingComponent.stripSoap(new String(decoded));
        assertXMLEqual(test, msgWithOutSoap);
    }

    /**
     * Encode a basic XML document.  Decode the encoded response.
     * Verify the decoded document is the same as the original one.
     */
    private void doTestComponentEncoding(EncodingComponent encoding,
                                         EncodingStyle encodingStyle) {
        try {
            final byte[] ORIGINAL_XML_DATA = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                + "<UOB version=\"0.5\">" + "<ForceStructureInformation/>"
                + "<Relationships count=\"1\">"
                + "<Relationship type=\"Scenario\">"
                + "<Description>UOB Data extracted SGS Workstation</Description>"
                + "<Assigned>" + "<UnitNode UIC=\"LOC000178\">"
                + "<ChildUnitNode UIC=\"SQU000359\"/>"
                + "<ChildUnitNode UIC=\"SQU000360\"/>"
                + "<ChildUnitNode UIC=\"SQU000361\"/>"
                + "<ChildUnitNode UIC=\"SQU000362\"/>"
                + "<ChildUnitNode UIC=\"SQU000363\"/>"
                + "<ChildUnitNode UIC=\"SQU000364\"/>"
                + "<ChildUnitNode UIC=\"SQU000365\"/>"
                + "<ChildUnitNode UIC=\"SQU000366\"/>"
                + "<ChildUnitNode UIC=\"SQU000367\"/>"
                + "<ChildUnitNode UIC=\"SQU000368\"/>"
                + "<ChildUnitNode UIC=\"SQU000369\"/>"
                + "<ChildUnitNode UIC=\"SQU000370\"/>"
                + "<ChildUnitNode UIC=\"SQU000371\"/>"
                + "<ChildUnitNode UIC=\"SQU000372\"/>"
                + "<ChildUnitNode UIC=\"SQU000373\"/>"
                + "<ChildUnitNode UIC=\"SQU000374\"/>"
                + "<ChildUnitNode UIC=\"SQU000375\"/>"
                + "<ChildUnitNode UIC=\"SQU000376\"/>" + "</UnitNode>"
                + "</Assigned>" + "</Relationship>" + "</Relationships>"
                + "</UOB>").getBytes();

            // dynamically load mock JBI classes
            BeanContainer beanContainer = BeanContainerFactory
                .createBeanContainer("/spring-basic.xml");
            MockComponentContext context = (MockComponentContext) beanContainer
                .getBean("componentContext");
            MockDeliveryChannel dc = (MockDeliveryChannel) context
                .getDeliveryChannel();

            // create request that will be received by Encoding SE
            MessageExchangeFactory exchangeFactory = dc.createExchangeFactory();
            InOut inout = exchangeFactory.createInOutExchange();
            NormalizedMessage inm = inout.createMessage();

            inm.setContent(new BytesSource(ORIGINAL_XML_DATA));
            inout.setInMessage(inm);
            inout.setProperty(EncodingComponent.ENCODE_PROPERTY_NAME,
                encodingStyle);

            // start Encoding SE and wait for its response
            encoding.init(context);
            encoding.onMessageExchange(inout);

            // verify encoded response is correct
            MessageExchange response = dc.getSendMessageExchange();
            assertEquals("encoding response contained an error1", null,
                response.getError());

            NormalizedMessage responseOut = response.getMessage("out");

            DataHandler dh = inm.getAttachment(EncodingComponent.ATTACHMENT_ID);
            BufferedInputStream in = new BufferedInputStream(dh.getInputStream());
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            int i;

            while ((i = in.read()) != -1) {
                baos.write(i);
            }

            byte[] inData = baos.toByteArray();

            // now decode response data back into original form
            InOut inout2 = exchangeFactory.createInOutExchange();
            inout2.setStatus(ExchangeStatus.ACTIVE);
            inout2.setInMessage(responseOut);

            // start Encoding SE and wait for its response
            encoding.onMessageExchange(inout2);

            MessageExchange response2 = dc.getSendMessageExchange();
            assertEquals("encoding response contained an error2", null,
                response2.getError());

            // verify encoded response is correct
            NormalizedMessage responseOut2 = response2.getMessage("out");

            byte[] decodedData = getBytes(responseOut2.getContent());
            assertNotNull("response data is null2", decodedData);

            // now for the big test - is decoded data same as original data
            assertXMLEqual(new String(ORIGINAL_XML_DATA), new String(decodedData));
        } catch (Throwable t) {
            t.printStackTrace();
            fail("Received Exception while testing: " + t.toString());
        }
    }

    private byte[] getBytes(Source source) {
        byte[] bytes = null;

        try {
            Transformer transformer =
                    TransformerFactory.newInstance().newTransformer();

            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(source, result);

            bytes = result.getWriter().toString().getBytes();
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        return bytes;
    }

}
