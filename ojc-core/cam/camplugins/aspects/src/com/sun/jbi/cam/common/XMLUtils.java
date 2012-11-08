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
 * @(#)XMLUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.common;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.apache.xml.serialize.XMLSerializer;
import org.apache.xml.serialize.OutputFormat;

/**
 * @author graj
 *
 */
public class XMLUtils {

	private Document document;
	
    private Element root;
    
    private File xmlFile;

    /** Creates a new instance of XMLUtil */
	public XMLUtils() {
	}

	/**
     * 
     * @param file
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public void parse(File file) throws IOException, ParserConfigurationException, SAXException {
        xmlFile = file;
        boolean isNamespaceAware = true, isValidating = false;
        parse(isNamespaceAware, isValidating);
    }

    /**
     * 
     * @param tagName
     * @return
     */
    public NodeList getElementByTagName(String tagName) {
        return root.getElementsByTagName(tagName);
    }
    
    /**
     * 
     * @param namespace
     * @param tagName
     * @return
     */
    public NodeList getElementByTagNameNS(String namespace, String tagName) {
        return root.getElementsByTagNameNS(namespace, tagName);
    }

    /**
     * 
     * @param tagName
     * @return
     */
    public Element createElement(String tagName) {
    	return document.createElement(tagName);
    }
    
    /**
     * 
     * @param namespace
     * @param tagName
     * @return
     */
    public Element createElementNS(String namespace, String tagName) {
    	return document.createElementNS(namespace, tagName);
    }
    
    /**
     * 
     * @param isNamespaceAware
     * @param isValidating
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    private void parse(boolean isNamespaceAware, boolean isValidating) throws IOException, ParserConfigurationException, SAXException {        
            // load the document from a file
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            factory.setValidating(false);
            

            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.parse(xmlFile);
            root = document.getDocumentElement();
    }    

	/**
	 * @return the xmlFile
	 */
	public File getXmlFile() {
		return xmlFile;
	}

	/**
	 * @param xmlFile the xmlFile to set
	 */
	public void setXmlFile(File xmlFile) {
		this.xmlFile = xmlFile;
	}

	/**
	 * @return the document
	 */
	public Document getDocument() {
		return document;
	}

	/**
	 * @return the root
	 */
	public Element getRoot() {
		return root;
	}

    /**
     * 
     * @param document
     * @param directoryPath
     * @throws TransformerConfigurationException
     * @throws TransformerException
     * @throws Exception
     */
    public void writeToFile(Document document, String directoryPath, String fileName)
            throws TransformerConfigurationException, TransformerException,
            Exception {
        File file = new File(directoryPath);
        if ((file.isDirectory() == false) || (file.exists() == false)) {
            throw new Exception("Directory Path: " + directoryPath
                    + " is invalid.");
        }
        String fileLocation = file.getAbsolutePath() + File.separator
                + fileName;
        System.out.println("Writing out to file: " + fileLocation);
        File outputFile = new File(fileLocation);
        this.writeToFile(document, outputFile);
        System.out.println("Created " + fileName + " at: " + fileLocation);    
        }

    
    /**
     * 
     * @param document
     * @param directoryPath
     * @throws TransformerConfigurationException
     * @throws TransformerException
     * @throws Exception
     */
    public void writeToFile(Document document, File outputFile) 
    throws TransformerConfigurationException, TransformerException {

        // Use a Transformer for aspectOutput
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StreamResult result = new StreamResult(outputFile);

        // indent the aspectOutput to make it more legible...
        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "4");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");

        transformer.transform(source, result);
    }

    /**
     * Change the contents of text file in its entirety, overwriting any
     * existing text. This style of implementation throws all exceptions to the
     * caller.
     * 
     * @param aFile
     *            is an existing file which can be written to.
     * @throws IllegalArgumentException
     *             if param does not comply.
     * @throws FileNotFoundException
     *             if the file does not exist.
     * @throws IOException
     *             if problem encountered during write.
     */
    public void setContents(File aFile, String aContents)
            throws FileNotFoundException, IOException {
        if (aFile == null) {
            throw new IllegalArgumentException("File should not be null.");
        }
        if (!aFile.exists()) {
            aFile.createNewFile();
        }
        if (!aFile.isFile()) {
            throw new IllegalArgumentException("Should not be a directory: "
                    + aFile);
        }
        if (!aFile.canWrite()) {
            throw new IllegalArgumentException("File cannot be written: "
                    + aFile);
        }

        // declared here only to make visible to finally clause; generic
        // reference
        Writer output = null;
        try {
            // use buffering
            // FileWriter always assumes default encoding is OK!
            output = new BufferedWriter(new FileWriter(aFile));
            output.write(aContents);
        } finally {
            // flush and close both "aspectOutput" and its underlying FileWriter
            if (output != null) {
                output.close();
            }
        }
    }
    

    
    /**
     * pretty printing XML data
     * @param   doc   - XML document
     * @param   out   - output stream
     */
    public void format(OutputStream out) throws Exception {
        OutputFormat format = new OutputFormat(document);
        format.setLineWidth(65);
        format.setIndenting(true);
        format.setIndent(2);
        XMLSerializer serializer = new XMLSerializer(out, format);
        serializer.serialize(document);
    }
    
    public void format2(OutputStream out) throws Exception {
        
        TransformerFactory tfactory = TransformerFactory.newInstance();
        Transformer serializer;
        try {
            serializer = tfactory.newTransformer();
            //Setup indenting to "pretty print"
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            
            serializer.transform(new DOMSource(document), new StreamResult(out));
        } catch (TransformerException e) {
            // this is fatal, just dump the stack and throw a runtime exception
            e.printStackTrace();
        }
    }    
    
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
}
