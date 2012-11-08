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
 * @(#)AdviceReader.java 
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
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.Advice;
import com.sun.jbi.cam.plugins.aspects.support.model.AdviceConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectType;

/*
 * 
 * <?xml version="1.0" encoding="UTF-8"?> <messageTracking> <config> <property
 * name="mode" value="file" /> </config> </messageTracking>
 */

/**
 * @author graj
 * 
 */
public class AdviceReader extends DefaultHandler implements Serializable {
	private static final long serialVersionUID = 1L;

	// Private members needed to parse the XML document
	private boolean parsingInProgress; // keep track of parsing

	private Stack<String> qNameStack = new Stack<String>(); // keep track of

	// QName

	Advice advice = new Advice();

	AdviceConfiguration adviceConfiguration = new AdviceConfiguration();

	/**
	 * 
	 */
	public AdviceReader() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the advice
	 */
	public Advice getAdvice() {
		return advice;
	}

	/**
	 * @return the adviceConfiguration
	 */
	public AdviceConfiguration getAdviceConfiguration() {
		return adviceConfiguration;
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
			if (qName.equals(AspectType.AutoReconnect.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.AutoReconnect);
			} else if (qName.equals(AspectType.Cache.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.Cache);
			} else if (qName.equals(AspectType.ContentBasedRouting
					.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.ContentBasedRouting);
			} else if (qName.equals(AspectType.Logging.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.Logging);
			} else if (qName
					.equals(AspectType.MessageTracking.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.MessageTracking);
			} else if (qName.equals(AspectType.Queueing.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.Queueing);
			} else if (qName.equals(AspectType.Tee.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.Tee);
			} else if (qName.equals(AspectType.Throttling.getDescription())) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.advice = new Advice();
				this.advice.setAspectType(AspectType.Throttling);
			} else if (qName.equals(XmlConstants.FACADEINFO_CONFIG_KEY)) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				this.adviceConfiguration = new AdviceConfiguration();
			} else if (qName.equals(XmlConstants.FACADEINFO_PROPERTY_KEY)) {
				// ELEMENT1 has an attribute, get it by name
				// Do something with the attribute
				if ((attributes != null) && (attributes.getLength() > 0)) {
					String name = attributes
							.getValue(XmlConstants.FACADEINFO_NAME_KEY);
					String value = attributes
							.getValue(XmlConstants.FACADEINFO_VALUE_KEY);
					if (name != null) {
						this.adviceConfiguration.addProperty(name, value);
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
			if (qName != null) {
				if (qName.equals(AspectType.AutoReconnect.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.Cache.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.ContentBasedRouting
						.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.Logging.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.MessageTracking
						.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.Queueing.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.Tee.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(AspectType.Throttling.getDescription())) {
					// We have encountered the end of ELEMENT1
					// ...
				} else if (qName.equals(XmlConstants.FACADEINFO_CONFIG_KEY)) {
					// We have encountered the end of ELEMENT1
					// ...
					this.advice.setAdviceConfiguration(this.adviceConfiguration);
					this.adviceConfiguration = null;

				} else if (qName.equals(XmlConstants.FACADEINFO_PROPERTY_KEY)) {
					// We have encountered the end of ELEMENT1
					// ...
				}
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
	public static AdviceReader parseFromXMLData(String rawXMLData)
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
		AdviceReader parser = new AdviceReader();
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
	public static AdviceReader parseFromFile(String fileName)
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
	public static AdviceReader parseFromFile(File file)
			throws MalformedURLException, ParserConfigurationException,
			SAXException, URISyntaxException, IOException {

		// System.out.println("Parsing file: "+file.getAbsolutePath());
		// Get an instance of the SAX parser factory
		SAXParserFactory factory = SAXParserFactory.newInstance();

		// Get an instance of the SAX parser
		SAXParser saxParser = factory.newSAXParser();

		// Initialize the URI and XML Document InputStream
		InputStream inputStream = new FileInputStream(file);

		// Create an InputSource from the InputStream
		InputSource inputSource = new InputSource(inputStream);

		// Parse the aspectInput XML document stream, using my event handler
		AdviceReader parser = new AdviceReader();
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
	public static AdviceReader parseFromURI(String uriString)
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
	public static AdviceReader parseFromURI(URI uri)
			throws MalformedURLException, ParserConfigurationException,
			SAXException, URISyntaxException, IOException {

		// System.out.println("Parsing URI: "+uri);
		// Get an instance of the SAX parser factory
		SAXParserFactory factory = SAXParserFactory.newInstance();

		// Get an instance of the SAX parser
		SAXParser saxParser = factory.newSAXParser();

		// Initialize the URI and XML Document InputStream
		InputStream inputStream = uri.toURL().openStream();

		// Create an InputSource from the InputStream
		InputSource inputSource = new InputSource(inputStream);

		// Parse the aspectInput XML document stream, using my event handler
		AdviceReader parser = new AdviceReader();
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
	public static Advice getAdvice(String rawXmlData)
			throws MalformedURLException, ParserConfigurationException,
			SAXException, URISyntaxException, IOException {
		Advice advice = null;
		AdviceReader parser = null;
		parser = AdviceReader.parseFromXMLData(rawXmlData);
		advice = parser.getAdvice();
		return advice;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String value = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><cache><config><property name=\"CachingStrategy\" value=\"FirstInFirstOutCache\" /><property name=\"MaximumEntries\" value=\"100\" /></config></cache>";
		Advice advice = null;
        try {
			advice = AdviceReader.getAdvice(value);
			
			System.out.println(advice.getAdviceConfiguration().retrieveValuesAsString());
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
