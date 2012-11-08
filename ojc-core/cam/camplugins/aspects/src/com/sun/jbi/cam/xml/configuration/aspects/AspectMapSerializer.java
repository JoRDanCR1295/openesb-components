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
 * @(#)AspectMapSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.aspects;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.List;

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
import org.xml.sax.SAXException;

import com.sun.jbi.cam.xml.configuration.model.aspects.AspectMap;
import com.sun.jbi.cam.xml.configuration.model.aspects.Parameter;
import com.sun.jbi.cam.xml.configuration.model.aspects.ServiceEntry;
import com.sun.jbi.cam.xml.configuration.model.aspects.ServiceType;

/**
 * @author graj
 *
 */
public class AspectMapSerializer implements Serializable {

    /**
     * 
     */
    public AspectMapSerializer() {
        // TODO Auto-generated constructor stub
    }
    
    public String process(AspectMap aspectMap) throws ParserConfigurationException, TransformerException {
        Document document = null;
        
        if (aspectMap != null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.newDocument(); // Create from whole cloth
            //////////////////////////////////
            // <aspectmap>
            //////////////////////////////////
            Element root = (Element) document.createElement("aspectmap");
            List<ServiceEntry> serviceEntryList = aspectMap.getServiceEntryList();
            if(serviceEntryList != null) {
                ServiceEntry entry = null;
                Iterator<ServiceEntry> entryIterator = serviceEntryList.iterator();
                while(entryIterator.hasNext() == true) {
                    entry = entryIterator.next();
                    if(entry != null) {
                        ServiceType type = entry.getServiceType();
                        if(type.equals(ServiceType.FILTER_ONE_WAY) == true) {
                            /////////////////////////////////////////////////////////////////////
                            // <filterOneWay>^</filterOneWay>
                            /////////////////////////////////////////////////////////////////////
                            Element filterOneWayElement = (Element) document.createElement("filterOneWay");
                            Parameter input = entry.getInput();
                            List<Parameter> outputList = entry.getOutputList();
                            this.appendParameters(document, filterOneWayElement, input, outputList);
                            root.appendChild(filterOneWayElement);
                        }
                        if(type.equals(ServiceType.FILTER_REQUEST_REPLY) == true) {
                            /////////////////////////////////////////////////////////////////////
                            // <filterRequestReply>^</filterRequestReply>
                            /////////////////////////////////////////////////////////////////////
                            Element filterRequestReplyElement = (Element) document.createElement("filterRequestReply");
                            Parameter input = entry.getInput();
                            List<Parameter> outputList = entry.getOutputList();
                            this.appendParameters(document, filterRequestReplyElement, input, outputList);
                            root.appendChild(filterRequestReplyElement);
                        }
                        if(type.equals(ServiceType.REQUEST_REPLY_SERVICE) == true) {
                            /////////////////////////////////////////////////////////////////////
                            // <requestReplyService>^</requestReplyService>
                            /////////////////////////////////////////////////////////////////////
                            Element requestReplyServiceElement = (Element) document.createElement("requestReplyService");
                            Parameter input = entry.getInput();
                            List<Parameter> outputList = entry.getOutputList();
                            this.appendParameters(document, requestReplyServiceElement, input, outputList);
                            root.appendChild(requestReplyServiceElement);
                        }
                    }
                }
            }
            document.appendChild(root);
        }
        return this.writeToString(document);
    }

    /**
     * 
     * @param document
     * @param element
     * @param input
     * @param outputList
     * @return
     */
    Element appendParameters(Document document, Element element, Parameter input, List<Parameter> outputList) {
        String partnerLink = null;
        String roleName = null;
        String portType = null;
        String operation = null;
        String messageType = null;
        String file = null;
        String transformJBI = null;                                
        if(input != null) {
            /////////////////////////////////////////////////////////////////////
            // <input>^</input>
            /////////////////////////////////////////////////////////////////////
            Element inputElement = (Element) document.createElement("input");
            if(input.getFile() != null) {
                file = input.getFile().getName();
                inputElement.setAttribute("file", file);
            }
            if(input.getMessageType() != null) {
                messageType = input.getMessageType().toString();
                inputElement.setAttribute("messageType", messageType);
            }
            if(input.getOperation() != null) {
                operation = input.getOperation();
                inputElement.setAttribute("operation", operation);
            }
            if(input.getPartnerLink() != null) {
                partnerLink = input.getPartnerLink().toString();
                inputElement.setAttribute("partnerLink", partnerLink);
            }
            if(input.getPortType() != null) {
                portType = input.getPortType().toString();
                inputElement.setAttribute("portType", portType);
            }
            if(input.getRoleName() != null) {
                roleName = input.getRoleName();
                inputElement.setAttribute("roleName", roleName);
            }
            transformJBI = Boolean.toString(input.isTransformJBI());
            inputElement.setAttribute("transformJBI", transformJBI);
            element.appendChild(inputElement);
        }
        if((outputList != null) && (outputList.size() > 0)) {
            Iterator<Parameter> outputIterator = outputList.iterator();
            while(outputIterator.hasNext() == true) {
                Parameter output = outputIterator.next();
                if(output != null) {
                    /////////////////////////////////////////////////////////////////////
                    // <output>^</output>
                    /////////////////////////////////////////////////////////////////////
                    Element outputElement = (Element) document.createElement("output");
                    if(output.getFile() != null) {
                        file = output.getFile().getName();
                        outputElement.setAttribute("file", file);
                    }
                    if(output.getMessageType() != null) {
                        messageType = output.getMessageType().toString();
                        outputElement.setAttribute("messageType", messageType);
                    }
                    if(output.getOperation() != null) {
                        operation = output.getOperation();
                        outputElement.setAttribute("operation", operation);
                    }
                    if(output.getPartnerLink() != null) {
                        partnerLink = output.getPartnerLink().toString();
                        outputElement.setAttribute("partnerLink", partnerLink);
                    }
                    if(output.getPortType() != null) {
                        portType = output.getPortType().toString();
                        outputElement.setAttribute("portType", portType);
                    }
                    if(output.getRoleName() != null) {
                        roleName = output.getRoleName();
                        outputElement.setAttribute("roleName", roleName);
                    }
                    transformJBI = Boolean.toString(output.isTransformJBI());
                    outputElement.setAttribute("transformJBI", transformJBI);
                    element.appendChild(outputElement);
                }
            }
        }
        return element;
    }
    
    /**
     * @param document
     * @return
     * @throws TransformerException
     */
    public String writeToString(Document document) throws TransformerException {
        // Use a Transformer for output
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);

        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");

        // indent the output to make it more legible...
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "4");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.transform(source, result);

        return result.getWriter().toString();
    }

    /**
     * 
     * @param document
     * @param directoryPath
     * @throws TransformerConfigurationException
     * @throws TransformerException
     * @throws Exception
     */
    public void writeToFile(Document document, String directoryPath)
            throws TransformerConfigurationException, TransformerException,
            Exception {
        File file = new File(directoryPath);
        if ((file.isDirectory() == false) || (file.exists() == false)) {
            throw new Exception("Directory Path: " + directoryPath
                    + " is invalid.");
        }
        String fileLocation = file.getAbsolutePath() + File.separator
                + "aspectmap.xml";
        System.out.println("Writing out to file: " + fileLocation);
        File outputFile = new File(fileLocation);
        // Use a Transformer for output
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StreamResult result = new StreamResult(outputFile);

        // indent the output to make it more legible...
        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "4");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");

        transformer.transform(source, result);
        System.out.println("Created aspectmap.xml at: "
                + fileLocation);

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
            // flush and close both "output" and its underlying FileWriter
            if (output != null) {
                output.close();
            }
        }
    }
    

    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "file:///C:/test/aspectmap.xml";
        AspectMapParser parser = null;
        AspectMap map = null;
        AspectMapSerializer serializer = null;
        String xmlString = null;
            try {
                parser = AspectMapParser.parse(uri);
                parser.getAspectMap().dump();
                map = parser.getAspectMap();
                serializer = new AspectMapSerializer();
                xmlString = serializer.process(map);
                File file = new File("C:/test/output.xml");
                serializer.setContents(file , xmlString);
                System.out.println("");
                System.out.println(xmlString);
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
            } catch (TransformerException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
            

    }

}
