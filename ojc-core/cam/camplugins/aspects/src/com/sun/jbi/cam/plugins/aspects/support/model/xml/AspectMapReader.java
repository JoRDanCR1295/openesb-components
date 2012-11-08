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
 * @(#)AspectMapReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
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

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectAdvice;
import com.sun.jbi.cam.plugins.aspects.support.model.Aspect;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMap;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectType;
import com.sun.jbi.cam.plugins.aspects.support.model.ExchangeType;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectInput;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectOutput;

/**
 * @author graj
 * 
 */
public class AspectMapReader extends DefaultHandler implements Serializable {
	private static final long serialVersionUID = 1L;
	
    // Private members needed to parse the XML document
    private boolean parsingInProgress; // keep track of parsing

    private Stack<String> qNameStack = new Stack<String>(); // keep track of
                                                            // QName

    AspectMap aspectMap = new AspectMap();

    Aspect aspect = new Aspect();

    AspectInput aspectInput = new AspectInput();

    AspectOutput aspectOutput = new AspectOutput();

    AspectAdvice aspectAdvice = new AspectAdvice();

    /**
     * 
     */
    public AspectMapReader() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @return the aspectMap
     */
    public AspectMap getAspectMap() {
        return aspectMap;
    }

    /**
     * Start of document processing.
     * 
     * @throws org.xml.sax.SAXException
     *             is any SAX exception, possibly wrapping another exception.
     */
    public void startDocument() throws SAXException {
        parsingInProgress = true;
        qNameStack.removeAllElements();
    }

    /**
     * End of document processing.
     * 
     * @throws org.xml.sax.SAXException
     *             is any SAX exception, possibly wrapping another exception.
     */
    public void endDocument() throws SAXException {
        parsingInProgress = false;
        // We have encountered the end of the document. Do any processing that
        // is desired,
        // for example dump all collected element2 values.

    }

    /**
     * Process the new element.
     * 
     * @param uri
     *            is the Namespace URI, or the empty string if the element has
     *            no Namespace URI or if Namespace processing is not being
     *            performed.
     * @param localName
     *            is the The local name (without prefix), or the empty string if
     *            Namespace processing is not being performed.
     * @param qName
     *            is the qualified name (with prefix), or the empty string if
     *            qualified names are not available.
     * @param attributes
     *            is the attributes attached to the element. If there are no
     *            attributes, it shall be an empty Attributes object.
     * @throws org.xml.sax.SAXException
     *             is any SAX exception, possibly wrapping another exception.
     */
    public void startElement(String uri, String localName, String qName,
            Attributes attributes) throws SAXException {
        if (qName != null) {
            if (qName.equals(XmlConstants.ASPECTMAPXML_ASPECTMAP_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.aspectMap = new AspectMap();
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_ASPECT_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.aspect = new Aspect();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String exchangeType = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_EXCHANGETYPE_KEY);
                    String id = attributes.getValue(XmlConstants.ASPECTMAPXML_ID_KEY);
                    if (exchangeType != null) {
                        this.aspect.setExchangeType(ExchangeType.convert(exchangeType));
                    }
                    if (id != null) {
                        this.aspect.setId(id);
                    }
                }
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_INPUT_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.aspectInput = new AspectInput();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String partnerLink = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_PARTNERLINK_KEY);
                    String roleName = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_ROLENAME_KEY);
                    String portType = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_PORTTYPE_KEY);
                    String operationName = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_OPERATION_KEY);
                    String messageType = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_MESSAGETYPE_KEY);
                    String file = attributes.getValue(XmlConstants.ASPECTMAPXML_FILE_KEY);
                    String transformJBI = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_TRANSFORMJBI_KEY);
                    if (partnerLink != null) {
                        aspectInput.setPartnerLinkQName(QName.valueOf(partnerLink));
                    }
                    if (roleName != null) {
                        aspectInput.setRoleName(roleName);
                    }
                    if (portType != null) {
                        aspectInput.setPortTypeQName(QName.valueOf(portType));
                    }
                    if (operationName != null) {
                        aspectInput.setOperationName(operationName);
                    }
                    if (messageType != null) {
                        aspectInput.setMessageTypeQName(QName.valueOf(messageType));
                    }
                    if (file != null) {
                        aspectInput.setInputTransformation(file);
                    }
                    if (transformJBI != null) {
                        aspectInput.setTransformJBIMessage(Boolean
                                .valueOf(transformJBI));
                    }
                    this.aspect.setInput(aspectInput);
                }
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_OUTPUT_KEY)) {
                // Do something with the attribute
                // Keep track of the value of element2
                this.aspectOutput = new AspectOutput();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String id = attributes.getValue(XmlConstants.ASPECTMAPXML_ID_KEY);
                    String serviceName = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_SERVICENAME_KEY);
                    String portName = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_PORTNAME_KEY);
                    String portType = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_PORTTYPE_KEY);
                    String operationName = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_OPERATION_KEY);
                    String messageType = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_MESSAGETYPE_KEY);
                    String file = attributes.getValue(XmlConstants.ASPECTMAPXML_FILE_KEY);
                    String transformJBI = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_TRANSFORMJBI_KEY);
                    if (id != null) {
                        aspectOutput.setId(id);
                    }
                    if (serviceName != null) {
                        aspectOutput.setServiceQName(QName.valueOf(serviceName));
                    }
                    if (portName != null) {
                        aspectOutput.setPortName(portName);
                    }
                    if (portType != null) {
                        aspectOutput.setPortTypeQName(QName.valueOf(portType));
                    }
                    if (operationName != null) {
                        aspectOutput.setOperationName(operationName);
                    }
                    if (messageType != null) {
                        aspectOutput.setMessageTypeQName(QName.valueOf(messageType));
                    }
                    if (file != null) {
                        aspectOutput.setInputTransformation(file);
                    }
                    if (transformJBI != null) {
                        aspectOutput.setTransformJBIMessage(Boolean
                                .valueOf(transformJBI));
                    }
                    this.aspect.addOutput(aspectOutput);
                }
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_ADVICE_KEY)) {
                // Do something with the attribute
                // Keep track of the value of element2
                this.aspectAdvice = new AspectAdvice();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String type = attributes.getValue(XmlConstants.ASPECTMAPXML_TYPE_KEY);
                    String configurationFile = attributes
                            .getValue(XmlConstants.ASPECTMAPXML_CONFIGURATIONFILE_KEY);
                    String order = attributes.getValue(XmlConstants.ASPECTMAPXML_ORDER_KEY);
                    if (type != null) {
                        aspectAdvice.setAspectType(AspectType.convert(type));
                    }
                    if (configurationFile != null) {
                        aspectAdvice.setConfigurationFile(configurationFile);
                    }
                    if (order != null) {
                        aspectAdvice.setOrder(Integer.valueOf(order));
                    }
                    this.aspect.addAdvice(aspectAdvice);
                }
            }
            // Keep track of QNames
            qNameStack.push(qName);
        }
    }

    /**
     * Process the end element tag.
     * 
     * @param uri
     *            is the Namespace URI, or the empty string if the element has
     *            no Namespace URI or if Namespace processing is not being
     *            performed.
     * @param localName
     *            is the The local name (without prefix), or the empty string if
     *            Namespace processing is not being performed.
     * @param qName
     *            is the qualified name (with prefix), or the empty string if
     *            qualified names are not available.
     * @throws org.xml.sax.SAXException
     *             is any SAX exception, possibly wrapping another exception.
     */
    public void endElement(String uri, String localName, String qName)
            throws SAXException {
        // Pop QName, since we are done with it
        qNameStack.pop();
        if (qName != null) {
            if (qName.equals(XmlConstants.ASPECTMAPXML_ASPECTMAP_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_ASPECT_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                if (this.aspect != null) {
                    this.aspectMap.addAspect(this.aspect);
                }
                this.aspect = null;
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_INPUT_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.aspectInput = null;
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_OUTPUT_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.aspectOutput = null;
            } else if (qName.equals(XmlConstants.ASPECTMAPXML_ADVICE_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.aspectAdvice = null;
            }
        }
    }
    
    /**
     * 
     * @param rawXMLData
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static AspectMapReader parseFromXMLData(String rawXMLData)            
    throws MalformedURLException, ParserConfigurationException,
    SAXException, URISyntaxException, IOException {
        // System.out.println("Parsing file: "+uriString);
        // Get an instance of the SAX parser factory
        SAXParserFactory factory = SAXParserFactory.newInstance();

        // Get an instance of the SAX parser
        SAXParser saxParser = factory.newSAXParser();

        // Initialize the XML Document InputStream
        Reader reader = new StringReader(rawXMLData);
        
        // Create an InputSource from the InputStream
        InputSource inputSource = new InputSource(reader);

        // Parse the aspectInput XML document stream, using my event handler
        AspectMapReader parser = new AspectMapReader();
        saxParser.parse(inputSource, parser);

        return parser;
        
    }
 
    /**
     * 
     * @param fileName
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static AspectMapReader parseFromFile(String fileName)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, URISyntaxException, IOException {
        File file = new File(fileName);
        return parseFromFile(file);
    }

    /**
     * 
     * @param fileName
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static AspectMapReader parseFromFile(File file)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, URISyntaxException, IOException {

        //System.out.println("Parsing file: "+file.getAbsolutePath());
        // Get an instance of the SAX parser factory
        SAXParserFactory factory = SAXParserFactory.newInstance();

        // Get an instance of the SAX parser
        SAXParser saxParser = factory.newSAXParser();

        // Initialize the URI and XML Document InputStream
        InputStream inputStream = new FileInputStream(file);

        // Create an InputSource from the InputStream
        InputSource inputSource = new InputSource(inputStream);

        // Parse the aspectInput XML document stream, using my event handler
        AspectMapReader parser = new AspectMapReader();
        saxParser.parse(inputSource, parser);

        return parser;
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
    public static AspectMapReader parseFromURI(String uriString)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, URISyntaxException, IOException {
        URI uri = new URI(uriString);
        return parseFromURI(uri);
    }
    
    /**
     * 
     * @param uri
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static AspectMapReader parseFromURI(URI uri)
            throws MalformedURLException, ParserConfigurationException,
            SAXException, URISyntaxException, IOException {

        //System.out.println("Parsing URI: "+uri);
        // Get an instance of the SAX parser factory
        SAXParserFactory factory = SAXParserFactory.newInstance();

        // Get an instance of the SAX parser
        SAXParser saxParser = factory.newSAXParser();

        // Initialize the URI and XML Document InputStream
        InputStream inputStream = uri.toURL().openStream();

        // Create an InputSource from the InputStream
        InputSource inputSource = new InputSource(inputStream);

        // Parse the aspectInput XML document stream, using my event handler
        AspectMapReader parser = new AspectMapReader();
        saxParser.parse(inputSource, parser);

        return parser;
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "C:/test/aspectmapinfo.xml";
        AspectMapReader parser = null;
        try {
            parser = AspectMapReader.parseFromFile(uri);
            AspectMap aspectMap = parser.getAspectMap();
            List<Aspect> aspectList = aspectMap.getAspectList();
            for(Aspect aspect: aspectList) {
            	List<AspectOutput> outputList = aspect.getOutputList();
            	for(AspectOutput output: outputList) {
            		System.out.println(output.getServiceQName());
            		System.out.println(output.getPortName());
            		System.out.println(output.getPortTypeQName());
            		System.out.println(output.getOperationName());
            		System.out.println(output.getMessageTypeQName());
            		System.out.println(output.getInputTransformation());
            		System.out.println(output.getId());
            	}
            }
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
