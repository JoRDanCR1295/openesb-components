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
 * @(#)AspectPolicyGroupReader.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml;

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
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroup;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroupCollection;

/**
 * @author graj
 *
 */
/*
 * <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<aspectPolicyGroups>
    <policyGroupCollection baseLocation=System.getProperty("JBICOMPS_HOME") + "/camplugins\aspects\build\web\workspace">
        <policyGroup folderName=System.getProperty("JBICOMPS_HOME") + "/camplugins\aspects\build\web\workspace\aspectpolicygroups\PolicyGroup1"
                     name="PolicyGroup1"
                     policyGroupName="Policy Group 1"
                     inputString="http://terraserver-usa.com/terraserver/;{http://terraserver-usa.com/terraserver/}TerraService;TerraServiceSoap;{http://terraserver-usa.com/terraserver/}TerraServiceSoap;http://terraservice.net/TerraService.asmx?WSDL;XSDurl_0,XSDurl_1,XSDurl_2,XSDurl_3,|"
                     facadeString="serviceName-sn;portName-pn;http://www.soap.com;targetNameSpace-tns"
                     aspectString="export_asp5_mta_2;file;;;|export_asp4_la_3;WARNING;WEEKLY;/tmp/loggingse.log;|export_asp0_ca_4;GenericCache;95;;|"
        />
    </policyGroupCollection>
</aspectPolicyGroups>

 */

public class AspectPolicyGroupReader extends DefaultHandler implements
		Serializable {
    private static final long serialVersionUID = 1L;

    // Private members needed to parse the XML document
    private boolean parsingInProgress; // keep track of parsing

    private Stack<String> qNameStack = new Stack<String>(); // keep track of
                                                            // QName
    PolicyGroupCollection collection = new PolicyGroupCollection();
    PolicyGroup group = new PolicyGroup();

	/**
	 * 
	 */
	public AspectPolicyGroupReader() {
		// TODO Auto-generated constructor stub
	}
	
    /**
	 * @return the collection
	 */
	public PolicyGroupCollection getCollection() {
		return collection;
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
            if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_ASPECTPOLICYGROUPS_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
            } else if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPCOLLECTION_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.collection = new PolicyGroupCollection();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String baseLocation = attributes
                            .getValue(XmlConstants.ASPECTPOLICYGROUP_BASELOCATION_KEY);
                    if(baseLocation != null) {
                    	this.collection.setBaseLocation(new File(baseLocation));
                    }
                }
            } else if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUP_KEY)) {
                // ELEMENT1 has an attribute, get it by name
                // Do something with the attribute
                this.group = new PolicyGroup();
                if ((attributes != null) && (attributes.getLength() > 0)) {
                    String name = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_NAME_KEY);
                    String folderName = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_FOLDERNAME_KEY);
                    String policyGroupName = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPNAME_KEY);
                    String inputString = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_INPUTSTRING_KEY);
                    String facadeString = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_FACADESTRING_KEY);
                    String aspectString = attributes.getValue(XmlConstants.ASPECTPOLICYGROUP_ASPECTSTRING_KEY);
                    if (name != null) {
                        this.group.setName(name);
                    }
                    if (folderName != null) {
                        this.group.setBaseLocation(new File(folderName));
                    }
                    if (policyGroupName != null) {
                    	this.group.setPolicyGroupName(policyGroupName);
                    }
                    if (inputString != null) {
                    	this.group.setInputString(inputString);
                    }
                    if (facadeString != null) {
                    	this.group.setFacadeString(facadeString);
                    }
                    if (aspectString != null) {
                    	this.group.setAspectString(aspectString);
                    }
                    if (this.group != null) {
                        this.collection.addPolicyGroup(group);
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
            if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_ASPECTPOLICYGROUPS_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
            } else if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPCOLLECTION_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
            } else if (qName.equals(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUP_KEY)) {
                // We have encountered the end of ELEMENT1
                // ...
                this.group = null;
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
   public static AspectPolicyGroupReader parseFromXMLData(String rawXMLData)
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
       AspectPolicyGroupReader parser = new AspectPolicyGroupReader();
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
   public static AspectPolicyGroupReader parseFromFile(String fileName)
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
   public static AspectPolicyGroupReader parseFromFile(File file)
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
       AspectPolicyGroupReader parser = new AspectPolicyGroupReader();
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
   public static AspectPolicyGroupReader parseFromURI(String uriString)
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
   public static AspectPolicyGroupReader parseFromURI(URI uri)
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
       AspectPolicyGroupReader parser = new AspectPolicyGroupReader();
       saxParser.parse(inputSource, parser);

       return parser;
   }

   /**
    * 
    * @param rawXmlData
    * @return
    * @throws MalformedURLException
    * @throws ParserConfigurationException
    * @throws SAXException
    * @throws URISyntaxException
    * @throws IOException
    */
   public static PolicyGroupCollection getPolicyGroupCollection(String rawXmlData) throws MalformedURLException,
   ParserConfigurationException, SAXException, URISyntaxException, IOException {
	   PolicyGroupCollection collection = null;
	   AspectPolicyGroupReader parser = null;
       parser = AspectPolicyGroupReader.parseFromXMLData(rawXmlData);
       collection = parser.getCollection();
       return collection;
   }
    
   /**
    * 
    * @param file
    * @return
    * @throws MalformedURLException
    * @throws ParserConfigurationException
    * @throws SAXException
    * @throws URISyntaxException
    * @throws IOException
    */
   public static PolicyGroupCollection getPolicyGroupCollection(File file) throws MalformedURLException,
   ParserConfigurationException, SAXException, URISyntaxException, IOException {
	   PolicyGroupCollection collection = null;
	   AspectPolicyGroupReader parser = null;
       parser = AspectPolicyGroupReader.parseFromFile(file);
       collection = parser.getCollection();
       return collection;
   }

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
