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
 * @(#)AspectMapParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.xml.configuration.aspects;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.cam.xml.configuration.ConfigurationParser;
import com.sun.jbi.cam.xml.configuration.model.aspects.AspectMap;
import com.sun.jbi.cam.xml.configuration.model.aspects.Parameter;
import com.sun.jbi.cam.xml.configuration.model.aspects.ServiceEntry;
import com.sun.jbi.cam.xml.configuration.model.aspects.ServiceType;

/**
 * @author graj
 *
 */
public class AspectMapParser extends DefaultHandler implements Serializable {
    // Private members needed to parse the XML document
    private boolean parsingInProgress; // keep track of parsing

    private Stack<String> qNameStack = new Stack<String>(); // keep track of QName

    private Parameter input = new Parameter(); // keep track of element2

    private Parameter output = new Parameter(); // keep track of element2

    private ServiceEntry serviceEntry = new ServiceEntry(); // keep track of element2

    private AspectMap aspectMap = new AspectMap(); // keep track of element2

    private List<Parameter> outputList = new ArrayList<Parameter>();

    // XML TAGS
    private static final String PARTNERLINK_KEY = "partnerLink";

    private static final String ROLENAME_KEY = "roleName";

    private static final String PORTTYPE_KEY = "portType";

    private static final String OPERATION_KEY = "operation";

    private static final String MESSAGETYPE_KEY = "messageType";

    private static final String FILE_KEY = "file";

    private static final String TRANSFORMJBI_KEY = "transformJBI";

    private static final String INPUT_KEY = "input";

    private static final String OUTPUT_KEY = "output";

    private static final String REQUESTREPLYSERVICE_KEY = "requestReplyService";

    private static final String FILTERONEWAY_KEY = "filterOneWay";

    private static final String FILTERREQUESTREPLY_KEY = "filterRequestReply";

    private static final String ASPECTMAP_KEY = "aspectmap";

    /**
     *
     */
    public AspectMapParser() {
        // TODO Auto-generated constructor stub
    }

    /**
     * Start of document processing.
     * @throws org.xml.sax.SAXException is any SAX exception,
     * possibly wrapping another exception.
     */
    public void startDocument() throws SAXException {
        parsingInProgress = true;
        qNameStack.removeAllElements();
        outputList.clear();
    }

    /**
     * End of document processing.
     * @throws org.xml.sax.SAXException is any SAX exception,
     * possibly wrapping another exception.
     */
    public void endDocument() throws SAXException {
        parsingInProgress = false;
        // We have encountered the end of the document. Do any processing that is desired,
        // for example dump all collected element2 values.

    }

    /**
     * Process the new element.
     * @param uri is the Namespace URI, or the empty string if the element
     * has no Namespace URI or if Namespace processing is not being performed.
     * @param localName is the The local name (without prefix), or the empty
     * string if Namespace processing is not being performed.
     * @param qName is the qualified name (with prefix), or the empty string
     * if qualified names are not available.
     * @param attributes is the attributes attached to the element. If there
     * are no attributes, it shall be an empty Attributes object.
     * @throws org.xml.sax.SAXException is any SAX exception,
     * possibly wrapping another exception.
     */
    public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
        if (qName.endsWith(ASPECTMAP_KEY)) {
            // ELEMENT1 has an attribute, get it by name
            // Do something with the attribute
            this.aspectMap = new AspectMap();
        } else if (qName.endsWith(REQUESTREPLYSERVICE_KEY)) {
            // ELEMENT1 has an attribute, get it by name
            // Do something with the attribute
            this.serviceEntry = new ServiceEntry();
            serviceEntry.setServiceType(ServiceType.REQUEST_REPLY_SERVICE);
        } else if (qName.endsWith(FILTERONEWAY_KEY)) {
            // ELEMENT1 has an attribute, get it by name
            // Do something with the attribute
            this.serviceEntry = new ServiceEntry();
            serviceEntry.setServiceType(ServiceType.FILTER_ONE_WAY);
        } else if (qName.endsWith(FILTERREQUESTREPLY_KEY)) {
            // ELEMENT1 has an attribute, get it by name
            // Do something with the attribute
            this.serviceEntry = new ServiceEntry();
            this.aspectMap.addServiceEntry(serviceEntry);
        } else if (qName.endsWith(INPUT_KEY)) {
            // Do something with the attribute
            // Keep track of the value of element2
            QName partnerLink = new QName(attributes.getValue(PARTNERLINK_KEY));
            String roleName = attributes.getValue(ROLENAME_KEY);
            QName portType = new QName(attributes.getValue(PORTTYPE_KEY));
            String operation = attributes.getValue(OPERATION_KEY);
            QName messageType = null;
            if(attributes.getValue(MESSAGETYPE_KEY) != null) {
                messageType = new QName(attributes.getValue(MESSAGETYPE_KEY));
            }
            File file = null;
            if(attributes.getValue(FILE_KEY) != null) {
                file = new File(attributes.getValue(FILE_KEY));
            }
            boolean transformJBI = false;
            if(attributes.getValue(TRANSFORMJBI_KEY) != null) {
                Boolean.getBoolean(attributes
                        .getValue(TRANSFORMJBI_KEY));
            }
            input = new Parameter(partnerLink, roleName, portType, operation,
                    messageType, file, transformJBI);
        } else if (qName.endsWith(OUTPUT_KEY)) {
            // Do something with the attribute
            // Keep track of the value of element2
            QName partnerLink = new QName(attributes.getValue(PARTNERLINK_KEY));
            String roleName = attributes.getValue(ROLENAME_KEY);
            QName portType = new QName(attributes.getValue(PORTTYPE_KEY));
            String operation = attributes.getValue(OPERATION_KEY);
            QName messageType = null;
            if(attributes.getValue(MESSAGETYPE_KEY) != null) {
                messageType = new QName(attributes.getValue(MESSAGETYPE_KEY));
            }
            File file = null;
            if(attributes.getValue(FILE_KEY) != null) {
                file = new File(attributes.getValue(FILE_KEY));
            }
            boolean transformJBI = false;
            if(attributes.getValue(TRANSFORMJBI_KEY) != null) {
                Boolean.getBoolean(attributes
                        .getValue(TRANSFORMJBI_KEY));
            }
            output = new Parameter(partnerLink, roleName, portType,
                    operation, messageType, file, transformJBI);
        }
        // Keep track of QNames
        qNameStack.push(qName);
    }

    /**
     * Process the character data for current tag.
     * @param ch are the element's characters.
     * @param start is the start position in the character array.
     * @param length is the number of characters to use from the
     * character array.
     * @throws org.xml.sax.SAXException is any SAX exception,
     * possibly wrapping another exception.
     */
    public void characters(char[] ch, int start, int length)
            throws SAXException {
        String qName;
        String chars = new String(ch, start, length);
        // Get current QName
        qName = (String) qNameStack.peek();
        if (qName.endsWith(ASPECTMAP_KEY)) {
            // Nothing to process
        } else if (qName.endsWith(REQUESTREPLYSERVICE_KEY)) {
            // Nothing to process
        } else if (qName.endsWith(FILTERONEWAY_KEY)) {
            // Nothing to process
        } else if (qName.endsWith(FILTERREQUESTREPLY_KEY)) {
            // Nothing to process
        } else if (qName.endsWith(INPUT_KEY)) {
            // Nothing to process
        } else if (qName.endsWith(OUTPUT_KEY)) {
            // Nothing to process
        }
    }

    /**
     * Process the end element tag.
     * @param uri is the Namespace URI, or the empty string if the element
     * has no Namespace URI or if Namespace processing is not being performed.
     * @param localName is the The local name (without prefix), or the empty
     * string if Namespace processing is not being performed.
     * @param qName is the qualified name (with prefix), or the empty
     * string if qualified names are not available.
     * @throws org.xml.sax.SAXException is any SAX exception,
     * possibly wrapping another exception.
     */
    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        // Pop QName, since we are done with it
        qNameStack.pop();
        if (qName.endsWith(ASPECTMAP_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
        } else if (qName.endsWith(REQUESTREPLYSERVICE_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            this.aspectMap.addServiceEntry(serviceEntry);
        } else if (qName.endsWith(FILTERONEWAY_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            this.aspectMap.addServiceEntry(serviceEntry);
        } else if (qName.endsWith(FILTERREQUESTREPLY_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            serviceEntry.setServiceType(ServiceType.FILTER_REQUEST_REPLY);
        } else if (qName.endsWith(INPUT_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            this.serviceEntry.setInput(input);
        } else if (qName.endsWith(OUTPUT_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            this.serviceEntry.addOutputParameter(output);
        }
    }

    /**
     *
     * @param uriString
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static AspectMapParser parse(String uriString)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, URISyntaxException, IOException {

        // Get an instance of the SAX parser factory
        SAXParserFactory factory = SAXParserFactory.newInstance();

        // Get an instance of the SAX parser
        SAXParser saxParser = factory.newSAXParser();

        // Initialize the URI and XML Document InputStream
        URI uri = new URI(uriString);
        InputStream inputStream = uri.toURL().openStream();

        // Create an InputSource from the InputStream
        InputSource inputSource = new InputSource(inputStream);

        // Parse the input XML document stream, using my event handler
        AspectMapParser parser = new AspectMapParser();
        saxParser.parse(inputSource, parser);

        return parser;
    }

    /**
     *
     * @return
     */
    public AspectMap getAspectMap() {
        return this.aspectMap;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "file:///C:/test/servicemap.xml";
        try {
            AspectMapParser parser = AspectMapParser.parse(uri);
            parser.getAspectMap().dump();
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
