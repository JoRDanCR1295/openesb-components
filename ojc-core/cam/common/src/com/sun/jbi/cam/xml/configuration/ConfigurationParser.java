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
 * @(#)ConfigurationParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.Stack;
import java.util.Map.Entry;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.cam.xml.configuration.model.Configuration;
import com.sun.jbi.cam.xml.configuration.model.DisplayInformation;

/**
 * @author graj
 *
 */
public class ConfigurationParser extends DefaultHandler implements Serializable {

    // Private members needed to parse the XML document
    private boolean parsingInProgress; // keep track of parsing
    private Stack<String> qNameStack = new Stack<String>(); // keep track of QName
    private Configuration configuration = new Configuration(); // keep track of element2
    private String[] displayKeys;
    private DisplayInformation[] displayInfo = null;

    // XML TAGS
    private static final String CONFIGURATION_KEY = "Configuration";
    private static final String DISPLAYNAME_KEY = "displayName";
    private static final String DISPLAYDESCRIPTION_KEY = "displayDescription";
    private static final String ISPASSWORDFIELD_KEY = "isPasswordField";
    private static final String NAME_KEY = "name";
    
    /**
     * 
     */
    public ConfigurationParser(String[] keys) {
        displayKeys = new String[keys.length];
        displayInfo = new DisplayInformation[keys.length];
        for(int index = 0; index < keys.length; index++) {
            displayKeys[index] = keys[index];
            displayInfo[index] = new DisplayInformation();
        }
    }
    
    public Configuration getComponentConfiguration() {
        return this.configuration;
    }

    /**
     * Start of document processing.
     * @throws org.xml.sax.SAXException is any SAX exception, 
     * possibly wrapping another exception.
    */
    public void startDocument() 
            throws SAXException {
        parsingInProgress = true;
        qNameStack.removeAllElements();
    }
    
    /**
     * End of document processing.
     * @throws org.xml.sax.SAXException is any SAX exception, 
     * possibly wrapping another exception.
     */
    public void endDocument() 
            throws SAXException {
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
    public void startElement(String uri, String localName, String qName, Attributes attributes) 
            throws SAXException {
        String displayName = null;
        String displayDescription = null;
        String password = null;
        boolean isPasswordField = false;
        if (qName.endsWith(CONFIGURATION_KEY)) {
            // ELEMENT1 has an attribute, get it by name
            String name = attributes.getValue(NAME_KEY);
            // Do something with the attribute
            this.configuration.setName(name);
        } else {
            for(int index = 0; index < displayKeys.length; index++) {
                if((displayKeys[index] != null) && 
                  (qName.endsWith(displayKeys[index]) == true)) {
                    // Keep track of the value of element2
                    isPasswordField = false;
                    displayName = attributes.getValue(DISPLAYNAME_KEY);
                    displayDescription = attributes.getValue(DISPLAYDESCRIPTION_KEY);
                    password = attributes.getValue(ISPASSWORDFIELD_KEY);
                    if((displayName != null) &&
                       (displayDescription != null) &&
                       (password != null)) {
                        isPasswordField = Boolean.parseBoolean(password);
                        System.out.println("displayName:"+ displayName+
                                           " displayDescription:"+displayDescription+
                                           " password:"+password+
                                           " isPasswordField:"+isPasswordField);
                        displayInfo[index] = new DisplayInformation(displayKeys[index],
                                displayName,displayDescription,isPasswordField);
                    } else {
                        System.out.println("displayName:"+ displayName+
                                           " displayDescription:"+displayDescription+
                                           " password:"+password);
                    }
                }
            }
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
        if (qName.endsWith(CONFIGURATION_KEY)) {
            // Nothing to process
        } else {
            for(int index = 0; index < displayKeys.length; index++) {
                if((displayKeys[index] != null) && 
                  (qName.endsWith(displayKeys[index]) == true)) {
                    // Keep track of the value of element2
                    if(chars != null) {
                        displayInfo[index].setDefaultValue(chars);
                    } else {
                        displayInfo[index].setDefaultValue("");
                    }
                } else {
                }
            }
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
        if (qName.endsWith(CONFIGURATION_KEY)) {
            // We have encountered the end of ELEMENT1
            // ...
            for(int index = 0; index < displayKeys.length; index++) {
                if((displayKeys[index] != null) && 
                   (displayInfo[index] != null)) {
                    this.configuration.addDisplayDetail(displayKeys[index], displayInfo[index]);
                } else {
                    System.out.println("Index "+index+" displayKeys or displayInfo is null.");
                    if(displayKeys[index] != null) {
                        System.out.println("displayKeys["+index+"] is: "+displayKeys[index]);
                    }
                    if(displayInfo[index] != null) {
                        System.out.println("displayInfo["+index+"] is: ");
                        displayInfo[index].dump();
                    }
                }
            }
        } else {
            // We have encountered the end of an ELEMENT2
            // ...
        }        
    }    
    
    /**
     * 
     * @param uriString
     * @param keys
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static ConfigurationParser parse(String uriString, String[] keys) throws MalformedURLException, ParserConfigurationException, SAXException, URISyntaxException, IOException {
        
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
            ConfigurationParser parser = new ConfigurationParser(keys);
            saxParser.parse(inputSource, parser);
            
            return parser;
    }
    
    /**
     * 
     * @param xmlData
     * @param keys
     * @return
     * @throws MalformedURLException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws URISyntaxException
     * @throws IOException
     */
    public static ConfigurationParser parseFromString(String xmlData, String[] keys) throws MalformedURLException, ParserConfigurationException, SAXException, URISyntaxException, IOException {
        
            // Get an instance of the SAX parser factory
            SAXParserFactory factory = SAXParserFactory.newInstance();
    
            // Get an instance of the SAX parser
            SAXParser saxParser = factory.newSAXParser();
            
            // Initialize the XML Document InputStream
            InputStream inputStream = new ByteArrayInputStream(xmlData.getBytes("UTF-8")); 
    
            // Create an InputSource from the InputStream
            InputSource inputSource = new InputSource(inputStream);
    
            // Parse the input XML document stream, using my event handler
            ConfigurationParser parser = new ConfigurationParser(keys);
            saxParser.parse(inputSource, parser);
            
            return parser;
    }
    
    
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "file:///${jbicomps_home}/cachese/jbiadapter/componentconfiguration.xml";
        String[] keys = null;
        String propertiesFile = System.getProperty("JBICOMPS_HOME") + "/cachese/jbiadapter/config.properties";
        Properties properties = new Properties();
        try {
            properties.load(new FileInputStream(propertiesFile));
        } catch (IOException e) {
        }
        keys = new String[properties.size()];
        Set set = properties.entrySet();
        Iterator iterator = set.iterator();
        for(int index = 0; iterator.hasNext() == true; index++) {
            Entry entry = (Entry)iterator.next();
            keys[index] = (String) entry.getKey(); 
        }
        try {
            ConfigurationParser parser = ConfigurationParser.parse(uri, keys);
            parser.getComponentConfiguration().dump();
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
