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
 * @(#)AspectMapWriter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.xml;

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

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectAdvice;
import com.sun.jbi.cam.plugins.aspects.support.model.Aspect;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMap;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectInput;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectOutput;

/**
 * @author graj
 * 
 */
public class AspectMapWriter implements Serializable {
	private static final long serialVersionUID = 1L;
	
    static final String FILE_NAME_KEY = "aspectmap.xml";

    /**
     * 
     */
    public AspectMapWriter() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param document
     * @return
     * @throws TransformerException
     */
    public String writeToString(Document document) throws TransformerException {
        // Use a Transformer for aspectOutput
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);

        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");

        // indent the aspectOutput to make it more legible...
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
                + FILE_NAME_KEY;
        System.out.println("Writing out to file: " + fileLocation);
        File outputFile = new File(fileLocation);
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
        System.out.println("Created " + FILE_NAME_KEY + " at: " + fileLocation);

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
     * 
     * @param aspectMap
     * @return
     * @throws ParserConfigurationException
     * @throws TransformerException
     */
    public String serialize(AspectMap aspectMap)
            throws ParserConfigurationException, TransformerException {
        Document document = null;
        if (aspectMap != null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.newDocument(); // Create from whole cloth

            // ////////////////////////////////
            // <aspectmap>
            Element root = (Element) document
                    .createElement(XmlConstants.ASPECTMAPXML_ASPECTMAP_KEY);
            List<Aspect> aspectList = aspectMap.getAspectList();
            if (aspectList.size() > 0) {
                for (Aspect aspect : aspectList) {
                    // ////////////////////////////////
                    // <aspect>
                    Element aspectElementChild = createAspectElement(document,
                            aspect);
                    // </aspect>
                    root.appendChild(aspectElementChild);
                    // ////////////////////////////////
                }
            }
            // </aspectmap>
            document.appendChild(root);
            // ////////////////////////////////

        }
        return this.writeToString(document);
    }

    /**
     * 
     * @param document
     * @param aspect
     * @return
     */
    Element createAspectElement(Document document, Aspect aspect) {
        Element aspectElement = null;
        if ((document != null) && (aspect != null)) {
            // <aspect>
            aspectElement = document.createElement(XmlConstants.ASPECTMAPXML_ASPECT_KEY);

            // exchangeType = "filterRequestReply"
            aspectElement.setAttribute(XmlConstants.ASPECTMAPXML_EXCHANGETYPE_KEY, aspect
                    .getExchangeType().getExchangeType());
            // ID="1"
            aspectElement.setAttribute(XmlConstants.ASPECTMAPXML_ID_KEY, aspect.getId());

            // ////////////////////////////////
            // <aspectInput>
            Element inputElementChild = createInputElement(document, aspect
                    .getInput());
            // </aspectInput>
            if (inputElementChild != null) {
                aspectElement.appendChild(inputElementChild);
            }
            // ////////////////////////////////

            List<AspectOutput> outputList = aspect.getOutputList();
            for (AspectOutput aspectOutput : outputList) {
                // ////////////////////////////////
                // <aspectOutput>
                Element outputElementChild = createOutputElement(document,
                        aspectOutput);
                // </aspectOutput>
                if (outputElementChild != null) {
                    aspectElement.appendChild(outputElementChild);
                }
                // ////////////////////////////////
            }

            AspectAdvice[] advices = aspect.retrieveAdvicesInOrder();
            for (AspectAdvice aspectAdvice : advices) {
                // ////////////////////////////////
                // <aspectAdvice>
                Element adviceElementChild = createAdviceElement(document,
                        aspectAdvice);
                // </aspectAdvice>
                if (adviceElementChild != null) {
                    aspectElement.appendChild(adviceElementChild);
                }
                // ////////////////////////////////
            }

        }
        return aspectElement;
    }

    /**
     * 
     * @param document
     * @param aspectInput
     * @return
     */
    Element createInputElement(Document document, AspectInput aspectInput) {
        Element inputElement = null;
        if ((document != null) && (aspectInput != null)) {
            // <aspectInput>
            inputElement = document.createElement(XmlConstants.ASPECTMAPXML_INPUT_KEY);
            if (aspectInput.getPartnerLinkQName() != null) {
                // partnerLink="{tns}partnerLink"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_PARTNERLINK_KEY, aspectInput
                        .getPartnerLinkQName().toString());
            }
            if (aspectInput.getRoleName() != null) {
                // roleName="roleName"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_ROLENAME_KEY, aspectInput
                        .getRoleName());
            }
            if (aspectInput.getPortTypeQName() != null) {
                // portType="{tns}portType"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_PORTTYPE_KEY, aspectInput
                        .getPortTypeQName().toString());
            }
            if (aspectInput.getOperationName() != null) {
                // operation="operation"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_OPERATION_KEY, aspectInput
                        .getOperationName());
            }
            if (aspectInput.getMessageTypeQName() != null) {
                // messageType="replyMessageType"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_MESSAGETYPE_KEY, aspectInput
                        .getMessageTypeQName().toString());
            }
            if (aspectInput.getInputTransformation() != null) {
                // file="aspectInput.xsl"
                inputElement.setAttribute(XmlConstants.ASPECTMAPXML_FILE_KEY, aspectInput
                        .getInputTransformation());
            }
            // transformJBI="false"
            inputElement.setAttribute(XmlConstants.ASPECTMAPXML_TRANSFORMJBI_KEY, Boolean
                    .toString(aspectInput.isTransformJBIMessage()));
        }
        return inputElement;
    }

    /**
     * 
     * @param document
     * @param aspectOutput
     * @return
     */
    Element createOutputElement(Document document, AspectOutput aspectOutput) {
        Element outputElement = null;
        if ((document != null) && (aspectOutput != null)) {
            // <aspectOutput>
            outputElement = document.createElement(XmlConstants.ASPECTMAPXML_OUTPUT_KEY);
            if (aspectOutput.getId() != null) {
                // ID="1"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_ID_KEY, aspectOutput.getId());
            }
            if (aspectOutput.getServiceQName() != null) {
                // serviceName="{tns}serviceName"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_SERVICENAME_KEY, aspectOutput
                        .getServiceQName().toString());
            }
            if (aspectOutput.getPortName() != null) {
                // portName="portName"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_PORTNAME_KEY, aspectOutput
                        .getPortName());
            }
            if (aspectOutput.getPortTypeQName() != null) {
                // portType="{tns}portType"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_PORTTYPE_KEY, aspectOutput
                        .getPortTypeQName().toString());
            }
            if (aspectOutput.getOperationName() != null) {
                // operation="operation"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_OPERATION_KEY, aspectOutput
                        .getOperationName());
            }
            if (aspectOutput.getMessageTypeQName() != null) {
                // messageType="replyMessageType"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_MESSAGETYPE_KEY, aspectOutput
                        .getMessageTypeQName().toString());
            }
            if (aspectOutput.getInputTransformation() != null) {
                // file="aspectInput.xsl"
                outputElement.setAttribute(XmlConstants.ASPECTMAPXML_FILE_KEY, aspectOutput
                        .getInputTransformation());
            }
            // transformJBI="false"
            outputElement.setAttribute(XmlConstants.ASPECTMAPXML_TRANSFORMJBI_KEY, Boolean
                    .toString(aspectOutput.isTransformJBIMessage()));
        }
        return outputElement;
    }

    /**
     * 
     * @param document
     * @param aspectAdvice
     * @return
     */
    Element createAdviceElement(Document document, AspectAdvice aspectAdvice) {
        Element adviceElement = null;
        if ((document != null) && (aspectAdvice != null)) {
            // <aspectAdvice>
            adviceElement = document.createElement(XmlConstants.ASPECTMAPXML_ADVICE_KEY);
            if (aspectAdvice.getAspectType() != null) {
                // type="throttling"
                adviceElement.setAttribute(XmlConstants.ASPECTMAPXML_TYPE_KEY, aspectAdvice
                        .getAspectType().getAspectType());
            }
            if (aspectAdvice.getConfigurationFile() != null) {
                // configurationFile="asp1_ta_2.xml"
                adviceElement.setAttribute(XmlConstants.ASPECTMAPXML_CONFIGURATIONFILE_KEY,
                        aspectAdvice.getConfigurationFile());
            }
            if (aspectAdvice.getOrder() != null) {
                // order="1"
                adviceElement.setAttribute(XmlConstants.ASPECTMAPXML_ORDER_KEY, aspectAdvice
                        .getOrder().toString());
            }
        }
        return adviceElement;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "C:/test/aspects/demo/AspectApplication/src/aspectmap.xml";
        AspectMapReader parser = null;
        String fileLocation = "C:/test/" + AspectMapWriter.FILE_NAME_KEY;
        File file = new File(fileLocation);
        String xmlString = null;
        AspectMap aspectMap = new AspectMap();
        AspectMapWriter serializer = new AspectMapWriter();

        try {
            parser = AspectMapReader.parseFromFile(uri);
            aspectMap = parser.getAspectMap();
            xmlString = serializer.serialize(aspectMap);
            serializer.setContents(file, xmlString);
            System.out.println("");
            System.out.println(xmlString);
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (FileNotFoundException e) {
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
