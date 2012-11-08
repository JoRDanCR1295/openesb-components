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
package com.gestalt.jbi.rss.component;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.W3CGeoModuleImpl;
import com.sun.syndication.feed.module.georss.geometries.Position;
import com.sun.syndication.feed.synd.*;

import org.custommonkey.xmlunit.XMLTestCase;

import org.w3c.dom.Document;

import java.io.StringWriter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


/**
 * Author: cgallemore Date: May 24, 2007
 */
public class RSSNormalizerTest extends XMLTestCase {
    private static final String LONGITUDE = "11.23";
    private static final String LATITUDE = "25.6234";
    private static Date newDate = new Date();
    private static final String JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/rssWsdl\" " +
        "type=\"tns:rssWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EntryList xmlns=\"http://xml.netbeans.org/schema/1.0/extensions/rssbc\">" +
        "<Entry>" + "<title>Entry 1</title>" +
        "<link>http://localhost:8000/rss/feed/entry1</link>" +
        "<description>First Entry</description>" + "<author>Brown</author>" +
        "<publishDate>" + newDate + "</publishDate>" + "</Entry>" +
        "<Entry>" + "<title>Entry 2</title>" +
        "<link>http://localhost:8000/rss/feed/entry2</link>" +
        "<description>Second Entry</description>" + "<author></author>" +
        "<publishDate>" + newDate + "</publishDate>" +
        "</Entry>" + "</EntryList>" + "</jbi:part>" +
        "</jbi:message>";
    private static final String GEO_JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/rssWsdl\" " +
        "type=\"tns:rssWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EntryList xmlns=\"http://xml.netbeans.org/schema/1.0/extensions/rssbc\">" +
        "<Entry>" + "<title>GeoRSS new entry</title>" +
        "<link>http://localhost:8000/georss/feed/entry</link>" +
        "<description>New Entry</description>" + "<author></author>" +
        "<publishDate>" + newDate + "</publishDate>" +
        "<longitude>" + LONGITUDE + "</longitude>" +
        "<latitude>" + LATITUDE + "</latitude>" + "</Entry>" + "</EntryList>" +
        "</jbi:part>" + "</jbi:message>";
    private static final String NO_ENTRY_CONTENT_JBI_MESSAGE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<jbi:message xmlns:tns=\"http://j2ee.netbeans.org/wsdl/rssWsdl\" " +
        "type=\"tns:rssWsdlOperationRequest\" version=\"1.0\" " +
        "xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\">" +
        "<jbi:part>" +
        "<EntryList xmlns=\"http://xml.netbeans.org/schema/1.0/extensions/rssbc\">" +
        "<Entry>" + "<title>rss-bc</title>" +
        "<link>http://jaxbld01:8282/job/rss-bc/com.gestalt.jbi.components.binding.jbi.rss$rss-binding-component-install/88/</link>" +
        "<description/>" + "<author/>" + "<publishDate>" + newDate + "</publishDate>" +
        "</Entry>" + "</EntryList>" + "</jbi:part>" + "</jbi:message>";
    private static final QName TYPE = new QName("http://j2ee.netbeans.org/wsdl/rssWsdl",
            "rssWsdlOperationRequest", "tns");
    private Logger log = Logger.getLogger(RSSNormalizerTest.class.getName());

    @SuppressWarnings("unchecked")
    public void testCreateConsumerContent() throws Exception {
        RSSNormalizer rssNormalizer = new RSSNormalizer();

        Document result = rssNormalizer.createConsumerContent(TYPE,
                (List<SyndEntry>) createFeed().getEntries());

        assertNotNull("Result was null", result);

        StringWriter sw = new StringWriter();

        try {
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }

        assertNotNull("String Writer was null", sw);
        log.fine("Consumer Test complete.");
        assertXMLEqual("Result was not equal to jbiMessage: \nJBI_MESSAGE = \n" +
            JBI_MESSAGE + "\nResult = \n" + sw.getBuffer().toString(), JBI_MESSAGE,
            sw.getBuffer().toString());
    }

    @SuppressWarnings("unchecked")
    public void testCreateConsumerGeoContent() throws Exception {
        RSSNormalizer rssNormalizer = new RSSNormalizer();
        Document result = rssNormalizer.createConsumerContent(TYPE,
                (List<SyndEntry>) createGeoFeed().getEntries());

        assertNotNull("Result was null", result);

        StringWriter sw = new StringWriter();

        try {
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }

        assertNotNull("String Writer was null", sw);
        assertXMLEqual("Result was not equal to jbiMessage: \nGEO_JBI_MESSAGE = \n" +
            GEO_JBI_MESSAGE + "\nResult = \n" + sw.getBuffer().toString(), GEO_JBI_MESSAGE,
            sw.getBuffer().toString());
    }

    @SuppressWarnings("unchecked")
    public void testCreateAtomFeedWithoutContent() throws Exception {
        RSSNormalizer rssNormalizer = new RSSNormalizer();
        Document result = rssNormalizer.createConsumerContent(TYPE,
                (List<SyndEntry>) createAtomFeedWithoutContent().getEntries());

        assertNotNull("Result was null", result);

        StringWriter sw = new StringWriter();

        try {
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult streamResult = new StreamResult(sw);
            transformer.transform(new DOMSource(result), streamResult);
        } catch (Exception e) {
            fail("Caught Exception trying to convert Document to String: " + e);
        }

        assertNotNull("String Writer was null", sw);
        assertXMLEqual("Result was not equal to jbiMessage: \n" + 
            "NO_ENTRY_CONTENT_JBI_MESSAGE = \n" + NO_ENTRY_CONTENT_JBI_MESSAGE + 
            "\nResult = \n" + sw.getBuffer().toString(),
            NO_ENTRY_CONTENT_JBI_MESSAGE, sw.getBuffer().toString());
    }

    /**
     * Creates one feed with single entry.
     *
     * @return SyndFeed
     */
    private SyndFeed createFeed() {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle("RSS Blog");
        feed.setLink("http://localhost:8000/rss/feed");
        feed.setDescription("RSS Blogs are cool");
        feed.setFeedType("rss_2.0");

        SyndEntry entry1 = new SyndEntryImpl();
        entry1.setTitle("Entry 1");
        entry1.setLink("http://localhost:8000/rss/feed/entry1");
        entry1.setAuthor("Brown");
        entry1.setPublishedDate(newDate);

        SyndContent description = new SyndContentImpl();
        description.setType("text/plain");
        description.setValue("First Entry");

        entry1.setDescription(description);

        SyndEntry entry2 = new SyndEntryImpl();
        entry2.setTitle("Entry 2");
        entry2.setLink("http://localhost:8000/rss/feed/entry2");
        entry2.setPublishedDate(newDate);

        SyndContent description2 = new SyndContentImpl();
        description2.setType("text/plain");
        description2.setValue("Second Entry");

        entry2.setDescription(description2);

        List<SyndEntry> list = new ArrayList<SyndEntry>();
        list.add(entry1);
        list.add(entry2);
        feed.setEntries(list);

        return feed;
    }

    /**
     * Creates a Feed containing GEO points
     *
     * @return SyndFeed
     */
    private SyndFeed createGeoFeed() {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle("GeoRSS Blog");
        feed.setLink("http://localhost:8000/georss/feed");
        feed.setDescription("GeoRSS Blogs are cool");
        feed.setFeedType("rss_2.0");

        SyndEntry entry = new SyndEntryImpl();
        entry.setTitle("GeoRSS new entry");
        entry.setLink("http://localhost:8000/georss/feed/entry");
        entry.setPublishedDate(newDate);

        SyndContent description = new SyndContentImpl();
        description.setType("text/plain");
        description.setValue("New Entry");
        entry.setDescription(description);

        GeoRSSModule geoRSSModule = new W3CGeoModuleImpl();
        geoRSSModule.setPosition(new Position(Double.parseDouble(LATITUDE),
                Double.parseDouble(LONGITUDE)));
        entry.getModules().add(geoRSSModule);

        List<SyndEntry> list = new ArrayList<SyndEntry>();
        list.add(entry);
        feed.setEntries(list);

        return feed;
    }

    /**
     * Creates one ATOM feed with single entry that does not have content.
     *
     * @return SyndFeed
     */
    private SyndFeed createAtomFeedWithoutContent() {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle("Hudson all builds");
        feed.setLink("http://localhost:8000/rss/feed");
        feed.setDescription("Hudson feed of all builds");
        feed.setAuthor("Hudson Server");
        feed.setFeedType("atom_1.0");

        SyndEntry entry1 = new SyndEntryImpl();
        entry1.setTitle("rss-bc");
        entry1.setLink(
            "http://jaxbld01:8282/job/rss-bc/com.gestalt.jbi.components.binding.jbi.rss$rss-binding-component-install/88/");
        entry1.setPublishedDate(newDate);
        entry1.setUpdatedDate(newDate);

        List<SyndEntry> list = new ArrayList<SyndEntry>();
        list.add(entry1);
        feed.setEntries(list);

        return feed;
    }
}
