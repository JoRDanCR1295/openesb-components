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

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.rss.component.rss.RSSManager;
import com.gestalt.jbi.rss.extensions.RSSAddress;
import com.gestalt.jbi.rss.extensions.RSSOperation;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;

import org.xmlpull.mxp1.MXParser;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import java.net.URISyntaxException;
import java.net.URL;

import java.util.ArrayList;
import java.util.List;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Part;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


/**
 * Handles the Normalization and the De-Normalization of the message exchange,
 * and performs the operation defined in the WSDL to either publish and RSS feed
 * or getExternalFeed an RSS feed. Author: cgallemore Date: May 14, 2007
 */
public class RSSProviderHandler extends AbstractMessageExchangeHandler {
    public static final String SERVICE_LIST = "ServiceList";
    public static final String SERVICE = "Service";
    public static final String WRAPPER_MESSAGE = "jbi:message";
    public static final String WRAPPER_PART = "jbi:part";
    private QName messageType;
    private List<String> wrappedParts = new ArrayList<String>();
    private RSSManager rssManager;

    public RSSProviderHandler(Endpoint endpoint) {
        super(endpoint);
    }

    @Override
    protected void processError(Exception exception) {
    }

    @Override
    protected void processDone() {
    }

    /**
     * Called when a message is received by the BC.
     */
    @Override
    protected void processMessage() {
        String pattern = exchange.getPattern().toString().trim();

        if (MEP.IN_ONLY.toString().equals(pattern)) {
            processInOnly((InOnly) exchange, (RSSEndpoint) endpoint);
        } else {
            log.warning("RSS Binding only supports InOnly");
        }
    }

    /**
     * Process a Fault message.
     *
     * @param fault
     */
    @Override
    protected void processFault(Fault fault) {
    }

    /**
     * Handles the processing of the InOnly Message Exchange. Takes the in
     * message and creates a new feed (if needed) and publishes the new entry.
     *
     * @param inOnly
     * @param rssEndpoint
     */
    private void processInOnly(InOnly inOnly, RSSEndpoint rssEndpoint) {
        log.fine("RSS Provider processing InOnly ME");

        RSSAddress rssAddress = rssEndpoint.getRSSAddress();

        try {
            NormalizedMessage in = inOnly.getInMessage();
            Source src = in.getContent();
            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            transformer.transform(src, result);

            QName operationName = inOnly.getOperation();
            RSSOperation operation = ((RSSEndpoint) endpoint).getRSSOperations()
                                      .get(operationName);
            RSSOperationInput input = ((RSSEndpoint) endpoint).getRSSOperationInput(operation);

            RSSOperation.OperationName rssOperationName = operation.getOperationName();

            if (rssOperationName == RSSOperation.OperationName.publish) {
                parseContent(result.getWriter().toString());

                Definition wsdl = endpoint.getWsdlDefinition();
                Message wsdlMessageDefinition = wsdl.getMessage(messageType);
                publishFeed(input, wsdlMessageDefinition, rssAddress);
            } else if (rssOperationName == RSSOperation.OperationName.subscribe) {
                parseContentXML(result.getWriter().toString());

                Definition wsdl = endpoint.getWsdlDefinition();
                Message wsdlMessageDefinition = wsdl.getMessage(messageType);
                subscribe(input, wsdlMessageDefinition, rssAddress);
            } else {
                throw new Exception("Unsupported Operation: " +
                    rssOperationName);
            }

            sendDone();
        } catch (Exception e) {
            log.severe("Error processing RSS Provider InOnly ME: " + e);
            sendError(e);
        }
    }

    /**
     * Publish the new entry that was received via the inOnly ME.
     *
     * @param input
     * @param wsdlMessageDefinition
     * @param rssAddress
     * @throws URISyntaxException
     */
    private void publishFeed(RSSOperationInput input,
        Message wsdlMessageDefinition, RSSAddress rssAddress)
        throws URISyntaxException {
        String title = getPartValue(input.getEntryTitle(), wsdlMessageDefinition);
        String link = getPartValue(input.getEntryLink(), wsdlMessageDefinition);
        String description = getPartValue(input.getEntryDescription(),
                wsdlMessageDefinition);
        String longitude = getPartValue(input.getLongitude(),
                wsdlMessageDefinition);
        String latitude = getPartValue(input.getLatitude(),
                wsdlMessageDefinition);
        String destinationUrl = getPartValue(input.getDestinationUrl(),
                wsdlMessageDefinition);
        rssManager.publish(title, link, description, longitude, latitude,
            rssAddress, destinationUrl);
    }

    /**
     * Allows an entity to subscribe to multiple RSS feeds, provided at runtime
     *
     * @param input
     * @param wsdlMessageDefinition
     * @param rssAddress
     */
    private void subscribe(RSSOperationInput input,
        Message wsdlMessageDefinition, RSSAddress rssAddress) {
        String feedsXML = getPartValue(input.getFeedList(),
                wsdlMessageDefinition);
        List<URL> feedList = parseFeedList(feedsXML);
        rssManager.subscribe(feedList, rssAddress);
    }

    protected List<URL> parseFeedList(String feedsXML) {
        List<URL> urlList = new ArrayList<URL>();

        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(feedsXML));
            urlList = parseFeedList(xpp);
        } catch (Exception e) {
            log.throwing(getClass().getName(), "parseFeedList", e);
        }

        return urlList;
    }

    private List<URL> parseFeedList(XmlPullParser xpp)
        throws XmlPullParserException, IOException {
        List<URL> urlList = new ArrayList<URL>();

        boolean done = false;

        while (!done) {
            int eventType = xpp.next();

            if (eventType == XmlPullParser.START_TAG) {
                if ("Address".equalsIgnoreCase(xpp.getName())) {
                    URL url = new URL(xpp.nextText());
                    log.fine("Found URL: " + url.toString());
                    urlList.add(url);
                }
            } else if (eventType == XmlPullParser.END_DOCUMENT) {
                done = true;
            }
        }

        return urlList;
    }

    /**
     * Parses the content of the incoming normalized message.
     *
     * @param content
     */
    private void parseContent(String content) {
        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(content));
            parseContent(xpp);
        } catch (Exception e) {
            log.throwing(getClass().getName(), "parseContent", e);
        }
    }

    /**
     * Helper method for the parsing of the incoming normalized message.
     *
     * @param xpp
     * @throws XmlPullParserException
     * @throws IOException
     */
    private void parseContent(XmlPullParser xpp)
        throws XmlPullParserException, IOException {
        boolean done = false;
        messageType = null;
        wrappedParts.clear();

        while (!done) {
            int eventType = xpp.next();

            if (eventType == XmlPullParser.START_TAG) {
                if ("message".equals(xpp.getName())) {
                    String type = xpp.getAttributeValue(null, "type");
                    messageType = resolveQName(type, xpp);
                } else if ("part".equals(xpp.getName())) {
                    String partText = xpp.nextText();
                    wrappedParts.add(partText);
                }
            } else if (eventType == XmlPullParser.END_DOCUMENT) {
                done = true;
            }
        }
    }

    /**
     * Parses the XML content of an incoming message
     *
     * @param content
     */
    private void parseContentXML(String content) {
        parseMessageType(content);

        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(new InputSource(
                        new StringReader(content)));

            Transformer transformer = TransformerFactory.newInstance()
                                                        .newTransformer();
            StringWriter sw = new StringWriter();
            StreamResult result = new StreamResult(sw);
            transformer.transform(new DOMSource(doc), result);

            for (String x : addPartsXML(sw.getBuffer().toString())) {
                wrappedParts.add(x);
            }
        } catch (Exception e) {
            log.throwing(getClass().getName(),
                "Exception parsing XML Content of JBI Message", e);
        }
    }

    protected void parseMessageType(String content) {
        try {
            XmlPullParser xpp = new MXParser();
            xpp.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, true);
            xpp.setInput(new StringReader(content));

            boolean done = false;
            messageType = null;

            while (!done) {
                int eventType = xpp.next();

                if (eventType == XmlPullParser.START_TAG) {
                    if ("message".equals(xpp.getName())) {
                        String type = xpp.getAttributeValue(null, "type");
                        messageType = resolveQName(type, xpp);
                    }
                } else if (eventType == XmlPullParser.END_DOCUMENT) {
                    done = true;
                }
            }
        } catch (Exception e) {
            log.throwing(getClass().getName(), "parseMessageType", e);
        }
    }

    protected List<String> addPartsXML(String buffer) {
        List<String> parts = new ArrayList<String>();

        try {
            DocumentBuilder builder = DocumentBuilderFactory.newInstance()
                                                            .newDocumentBuilder();
            Document doc = builder.parse(new ByteArrayInputStream(
                        buffer.getBytes()));
            parts = getParts(doc);
        } catch (Exception e) {
            log.throwing(getClass().getName(), "addPartsXML", e);
        }

        return parts;
    }

    /**
     * Searches the given Document for all parts in the message.
     *
     * @param doc - The jbi message.
     * @return - List of all jbi parts.
     */
    private List<String> getParts(Document doc) {
        List<String> list = new ArrayList<String>();
        Element jbiMessageWrapper = doc.getDocumentElement();

        if (jbiMessageWrapper.getTagName().equalsIgnoreCase(WRAPPER_MESSAGE)) {
            NodeList childNodesI = jbiMessageWrapper.getChildNodes();

            for (int i = 0, I = childNodesI.getLength(); i < I; i++) {
                Node nodeI = childNodesI.item(i);

                if (nodeI.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }

                Element jbiPartWrapper = (Element) nodeI;

                if (!jbiPartWrapper.getTagName().equalsIgnoreCase(WRAPPER_PART)) {
                    continue;
                }

                NodeList childNodesJ = jbiPartWrapper.getChildNodes();

                for (int j = 0, J = childNodesJ.getLength(); j < J; j++) {
                    Node nodeJ = childNodesJ.item(j);

                    if (nodeJ.getNodeType() == Node.TEXT_NODE) {
                        list.add(nodeJ.getTextContent());

                        break;
                    }
                }
            }
        }

        return list;
    }

    /**
     * Helper method for resolving QNames of the incoming normalized message.
     *
     * @param value
     * @param xpp
     * @return QName
     */
    private QName resolveQName(String value, XmlPullParser xpp) {
        String[] parts = value.split(":");

        if (parts.length > 1) {
            return new QName(xpp.getNamespace(parts[0]), parts[1], parts[0]);
        }

        return new QName(null, value, null);
    }

    @SuppressWarnings("unchecked")
    private String getPartValue(String searchName, javax.wsdl.Message message) {
        List<Part> orderedParts = (List<Part>) message.getOrderedParts(null);

        for (int i = 0; i < orderedParts.size(); i++) {
            Part part = (Part) orderedParts.get(i);

            if (part.getName().equals(searchName)) {
                return wrappedParts.get(i);
            }
        }

        return null;
    }

    /**
     * Gets the RSSManager object.
     *
     * @return RSSManager
     */
    public RSSManager getRSSManager() {
        return rssManager;
    }

    /**
     * Sets the RSSManager object.
     *
     * @param rssManager
     */
    public void setRSSManager(RSSManager rssManager) {
        this.rssManager = rssManager;
    }
}
