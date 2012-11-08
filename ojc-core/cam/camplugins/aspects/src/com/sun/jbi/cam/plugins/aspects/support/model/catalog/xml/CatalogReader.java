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
 * @(#)CatalogReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml;

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
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.Catalog;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WSDLService;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WebServices;

/**
 * @author graj
 *
 */
public class CatalogReader extends DefaultHandler implements Serializable {
    private static final long serialVersionUID = 1L;

    // Private members needed to parse the XML document
    private boolean parsingInProgress; // keep track of parsing

    private Stack<String> qNameStack = new Stack<String>(); // keep track of
                                                            // QName

    Catalog catalog = new Catalog();
    WebServices webServices = new WebServices();
    WSDLService wsdlService = new WSDLService();

    /**
     *
     */
    public CatalogReader() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @return the catalog
     */
    public Catalog getCatalog() {
        return catalog;
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



            if (qName.equals(XmlConstants.CATALOG_CATALOG_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.catalog = new Catalog();
            } else if (qName.equals(XmlConstants.CATALOG_SERVICES_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.webServices = new WebServices();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String baseLocation = attributes
                            .getValue(XmlConstants.CATALOG_BASELOCATION_KEY);
                    webServices.setBaseLocation(baseLocation);
                }
                if (this.webServices != null) {
                    this.catalog.addWebServicesToCatalog(webServices);
                }
            } else if (qName.equals(XmlConstants.CATALOG_SERVICE_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute

                this.wsdlService = new WSDLService();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String name = attributes.getValue(XmlConstants.CATALOG_NAME_KEY);
                    String serviceGroupName = attributes.getValue(XmlConstants.CATALOG_SERVICEGROUPNAME_KEY);
                    String targetNamespace = attributes
                            .getValue(XmlConstants.CATALOG_TARGETNAMESPACE_KEY);
                    String portName = attributes
                            .getValue(XmlConstants.CATALOG_PORT_KEY);
                    String locationURI = attributes
                            .getValue(XmlConstants.CATALOG_LOCATIONURI_KEY);
                    String portType = attributes
                            .getValue(XmlConstants.CATALOG_PORTTYPE_KEY);
                    String folderName = attributes.getValue(XmlConstants.CATALOG_FOLDERNAME_KEY);
                    String wsdlFiles = attributes.getValue(XmlConstants.CATALOG_WSDLFILES_KEY);
                    String xsdFiles = attributes.getValue(XmlConstants.CATALOG_XSDFILES_KEY);
                    String serviceWSDLFile = attributes.getValue(XmlConstants.CATALOG_SERVICEWSDLFILE_KEY);
                    if (serviceGroupName != null) {
                        wsdlService.setServiceGroupName(serviceGroupName);
                    }
                    if (name != null) {
                        wsdlService.setName(QName.valueOf(name));
                    }
                    if (targetNamespace != null) {
                        wsdlService.setTargetNamespace(targetNamespace);
                    }
                    if (portName != null) {
                        wsdlService.setPortName(portName);
                    }
                    if (locationURI != null) {
                        wsdlService.setLocationURI(locationURI);
                    }
                    if (portType != null) {
                        wsdlService.setPortType(QName.valueOf(portType));
                    }
                    if (folderName != null) {
                        wsdlService.setFolder(new File(folderName));
                    }
                    if (wsdlFiles != null) {
                        wsdlService.setWsdlFiles(wsdlFiles);
                    }
                    if (xsdFiles != null) {
                        wsdlService.setXsdFiles(xsdFiles);
                    }
                    if(serviceWSDLFile != null) {
                    	wsdlService.setServiceWSDLFile(new File(serviceWSDLFile));
                    }
                    if (this.wsdlService != null) {
                        this.webServices.addWsdlServiceToList(wsdlService);
                    }
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
            if (qName.equals(XmlConstants.CATALOG_CATALOG_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
            } else if (qName.equals(XmlConstants.CATALOG_SERVICES_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.webServices = null;
            } else if (qName.equals(XmlConstants.CATALOG_SERVICE_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.wsdlService = null;
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
    public static CatalogReader parseFromXMLData(String rawXMLData)
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
        CatalogReader parser = new CatalogReader();
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
    public static CatalogReader parseFromFile(String fileName)
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
    public static CatalogReader parseFromFile(File file)
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
        CatalogReader parser = new CatalogReader();
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
    public static CatalogReader parseFromURI(String uriString)
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
    public static CatalogReader parseFromURI(URI uri)
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
        CatalogReader parser = new CatalogReader();
        saxParser.parse(inputSource, parser);

        return parser;
    }
    
    public static Catalog getCatalog(String uri) {
        Catalog catalog = null;
        CatalogReader parser = null;
        File catalogFile = new File(uri);
        if ( catalogFile.exists() ) {
            try {
                 parser = CatalogReader.parseFromFile(uri);
                 catalog = parser.getCatalog();
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
        return catalog;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "C:/test/aspects/catalog/catalog.xml";
        CatalogReader parser = null;
        try {
            parser = CatalogReader.parseFromFile(uri);
            Catalog catalog = parser.getCatalog();
            List<WebServices> servicesList = catalog.getWebServicesList();
            for(WebServices webServices : servicesList) {
                System.out.println("Base Location: "+webServices.getBaseLocation().getAbsolutePath());
                List<WSDLService> serviceList = webServices.getWsdlServiceList();
                for(WSDLService wsdlService : serviceList) {
                    System.out.println("    Location URI: "+wsdlService.getLocationURI());
                    System.out.println("    Port Name: "+wsdlService.getPortName());
                    System.out.println("    TargetNamespace: "+wsdlService.getTargetNamespace());
                    System.out.println("    FolderName: "+wsdlService.getFolder().getAbsolutePath());
                    System.out.println("    Name: "+wsdlService.getName());
                    System.out.println("    Service Group Name: "+wsdlService.getServiceGroupName());
                    System.out.println("    PortType: "+wsdlService.getPortType());
                    System.out.println("    Service WSDL File: "+wsdlService.getServiceWSDLFile().getAbsolutePath());
                    List<File> wsdlFileList = wsdlService.getWsdlFiles();
                    for(File file : wsdlFileList) {
                        System.out.println("      WSDL File: "+file.getAbsolutePath());
                    }
                    List<File> xsdFileList = wsdlService.getXsdFiles();
                    for(File file : xsdFileList) {
                        System.out.println("      XSD File: "+file.getAbsolutePath());
                    }
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
